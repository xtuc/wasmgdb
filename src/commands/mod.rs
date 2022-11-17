//! Handles the parsing and execution of commands

use colored::Colorize;
use log::error;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::digit1;
use nom::combinator::opt;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::IResult;
use std::fmt::Write;

use crate::{coredump, memory, print_value, BoxError, Context};
use wasmgdb_ddbug_parser as ddbug_parser;

mod frames;

pub(crate) enum Command<'a> {
    Unknown,
    Backtrace,
    SelectFrame(usize),
    Print(PrintFormat, &'a str),
    PrintDeref(PrintFormat, &'a str),
}

pub(crate) enum PrintFormat {
    None,
    String,
}

pub(crate) fn parse_command<'a>(input: &'a str) -> IResult<&'a str, Command<'a>> {
    let (input, word) = alpha1(input)?;

    Ok(match word {
        "bt" => (input, Command::Backtrace),
        "p" => {
            let (input, format) = opt(tag("/s"))(input)?;

            let format = if let Some(format) = format {
                match format {
                    "/s" => PrintFormat::String,
                    e => unimplemented!("unknow format {}", e),
                }
            } else {
                PrintFormat::None
            };

            let what = tuple((opt(tag("*")), alpha1));
            let (input, (deref, what)) = preceded(tag(" "), what)(input)?;

            if deref.is_some() {
                (input, Command::PrintDeref(format, what))
            } else {
                (input, Command::Print(format, what))
            }
        }
        "f" => {
            let (input, n) = preceded(tag(" "), digit1)(input)?;
            let n = n.parse::<usize>().unwrap();

            (input, Command::SelectFrame(n))
        }
        _ => (input, Command::Unknown),
    })
}

pub(crate) fn run_command<R: gimli::Reader>(
    ctx: &mut Context<R>,
    stack_frames: &Vec<coredump::StackFrame>,
    cmd: Command,
) -> Result<(), BoxError> {
    match cmd {
        Command::Backtrace => {
            frames::backtrace(ctx, stack_frames)?;
        }

        Command::PrintDeref(_format, what) => {
            if let Some(variable) = ctx.variables.get(what) {
                let selected_frame = ctx
                    .selected_frame
                    .as_ref()
                    .ok_or("no frame has been selected")?;
                let func = ctx
                    .ddbug
                    .functions_by_linkage_name
                    .get(&selected_frame.binary_name)
                    .ok_or(format!("function {} not found", selected_frame.binary_name))?;

                let ty = variable.ty(&ctx.ddbug).unwrap();

                match ty.kind() {
                    ddbug_parser::TypeKind::Modifier(type_modifier)
                        if type_modifier.kind() == ddbug_parser::TypeModifierKind::Pointer =>
                    {
                        let addr = memory::get_param_addr(&selected_frame, func, variable)?;
                        let ptr = memory::read_ptr(ctx.coredump, addr)?;

                        let target_type =
                            type_modifier.ty(&ctx.ddbug).ok_or("unknown target type")?;

                        let out = print_value(&ctx, ptr, target_type.as_ref(), 0)?;
                        println!("{}: {}", what, out);
                    }
                    _ => {
                        error!("variable {} is not a ptr", what);
                    }
                };
            } else {
                error!("variable {} not found", what);
            }
        }

        Command::Print(format, what) => {
            if let Some(variable) = ctx.variables.get(what) {
                let selected_frame = ctx
                    .selected_frame
                    .as_ref()
                    .ok_or("no frame has been selected")?;
                let func = ctx
                    .ddbug
                    .functions_by_linkage_name
                    .get(&selected_frame.binary_name)
                    .ok_or(format!("function {} not found", selected_frame.binary_name))?;

                let ty = variable.ty(&ctx.ddbug).unwrap();

                let addr = memory::get_param_addr(&selected_frame, func, variable)?;

                match format {
                    PrintFormat::String => {
                        let ptr = memory::read_ptr(ctx.coredump, addr)?;

                        let mut addr = ptr;
                        let mut out = "".to_owned();
                        loop {
                            let v = ctx.coredump[addr as usize];
                            if v == 0 {
                                break;
                            }
                            write!(out, "{}", v as char)?;
                            addr += 1;
                        }

                        println!("{} ({} char(s)) = {}", what, out.len(), out);
                    }
                    PrintFormat::None => {
                        let out = print_value(&ctx, addr, ty.as_ref(), 0)?;
                        println!("{}: {}", what, out);
                    }
                }
            } else {
                error!("variable {} not found", what);
            }
        }

        Command::SelectFrame(selected_frame) => {
            let stack_frame = &stack_frames[stack_frames.len() - 1 - selected_frame];

            frames::print_frame(ctx, &stack_frame)?;
            frames::select_frame(ctx, &stack_frame)?;

            ctx.selected_frame = Some(stack_frame.clone());
        }

        Command::Unknown => return Err("unknow command".into()),
    }

    Ok(())
}
