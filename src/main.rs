// https://dwarfstd.org/doc/DWARF5.pdf
use colored::Colorize;
use log::{debug, error, info, warn};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::digit1;
use nom::combinator::opt;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::IResult;
use std::collections::HashMap;
use std::fmt::Write;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::Write as IoWrite;
use std::sync::Arc;
use std::{borrow, env};

use wasmgdb_ddbug_parser as ddbug_parser;

type BoxError = Box<dyn std::error::Error>;

/// Struct collect while unwinding by wasm-edit
// The stack frame relies on Wasm funcidx instead of instruction binary offset
// because it has been buggy and hard to debug in the past.
// TODO: eventually switch back to code offsets.
#[derive(Debug, Clone)]
struct StackFrame {
    funcidx: u32,
    binary_name: String,
    locals: Vec<u32>,
}

fn print_value<R: gimli::Reader>(
    ctx: &Context<R>,
    addr: u32,
    type_: &ddbug_parser::Type,
    mut depth: usize,
) -> Result<String, BoxError> {
    let ident = "\t".repeat(depth);

    match &type_.kind() {
        ddbug_parser::TypeKind::Modifier(type_modifier)
            if type_modifier.kind() == ddbug_parser::TypeModifierKind::Pointer =>
        {
            let target_type = if let Some(ty) = type_modifier.ty(&ctx.ddbug) {
                format!("{}", ty)
            } else {
                "???".to_owned()
            };
            Ok(format!("{}*{} = 0x{:x}", ident, target_type, addr))
        }
        ddbug_parser::TypeKind::Base(base_type) => {
            let size_of = base_type.byte_size().unwrap_or(4);
            let mut bytes = read(ctx.coredump, addr, size_of)?.to_vec();
            bytes.reverse();
            let value = format!("0x{}", hex::encode(&bytes));
            Ok(format!(
                "{}{} = {}",
                ident,
                base_type.name().unwrap().yellow(),
                value
            ))
        }
        ddbug_parser::TypeKind::Struct(struct_type) => {
            let mut out = "".to_owned();
            write!(
                out,
                "{}{} = {{",
                ident,
                struct_type.name().unwrap().yellow()
            )?;

            if depth < 1 {
                write!(out, "\n")?;

                depth += 1;
                for member in struct_type.members() {
                    if let Some(member_type) = member.ty(&ctx.ddbug) {
                        let addr = get_member_addr(addr, member)?;
                        let value = print_value(ctx, addr, member_type.as_ref(), depth)?;

                        let ident = "\t".repeat(depth);

                        write!(
                            out,
                            "{}{}: {}\n",
                            ident,
                            member.name().unwrap().green(),
                            value
                        )?;
                    } else {
                        write!(
                            out,
                            "{}{}: <type unknown>\n",
                            ident,
                            member.name().unwrap().green()
                        )?;
                    }
                }
            } else {
                write!(out, "...")?;
            }
            write!(out, "}}")?;

            Ok(out)
        }
        e => unimplemented!("{:?}", e),
    }
}

fn select_frame<R: gimli::Reader>(
    ctx: &mut Context<R>,
    frame: &StackFrame,
) -> Result<(), BoxError> {
    // Clear previous selected scope
    ctx.variables.clear();

    let func = ctx
        .ddbug
        .functions_by_linkage_name
        .get(&frame.binary_name)
        .ok_or(format!("function {} not found", frame.binary_name))?;

    for param in func.details(&ctx.ddbug).parameters() {
        if let Some(name) = param.name() {
            ctx.variables.insert(name.to_owned(), param.clone());
        }
    }

    let params = func
        .details(&ctx.ddbug)
        .parameters()
        .iter()
        .map(|param| {
            let param_name = if let Some(name) = param.name() {
                name
            } else {
                "???"
            };

            let ty = param.ty(&ctx.ddbug).unwrap();
            let location = param
                .data_location()
                .map(|l| format!("{:?}", l))
                .unwrap_or_else(|| "???".to_owned());

            format!("{}\t=\t {} (location={})", param_name, ty, location)
        })
        .collect::<Vec<String>>()
        .join("\n");

    println!("frame base\t=\t{:?}", func.frame_base());
    println!("Arguments:\n{}", params);

    Ok(())
}

fn print_frame<'a, R: gimli::Reader>(ctx: &Context<R>, frame: &StackFrame) -> Result<(), BoxError> {
    if let Some(func) = ctx.ddbug.functions_by_linkage_name.get(&frame.binary_name) {
        let source = format!(
            "{}/{}",
            func.source()
                .directory()
                .unwrap_or_else(|| "<directory not found>"),
            func.source().file().unwrap_or_else(|| "<file not found>")
        );

        let function = {
            let name = func.name().unwrap();

            let params = func
                .details(&ctx.ddbug)
                .parameters()
                .iter()
                .map(|param| {
                    let param_name = if let Some(name) = param.name() {
                        name
                    } else {
                        "???"
                    };

                    // TODO: not always 4 bytes, right?
                    let size_of = 4;

                    let value = if let Ok(addr) = get_param_addr(frame, &func, param) {
                        let bytes = read(ctx.coredump, addr, size_of).unwrap();
                        format!("0x{}", hex::encode(&bytes))
                    } else {
                        "???".to_owned()
                    };
                    format!("{}={}", param_name.green(), value)
                })
                .collect::<Vec<String>>()
                .join(", ");

            format!("{} ({})", name.yellow(), params)

            // if let Some(ref symbol) = found_symbol {
            //     let name = &symbol.name;

            //     let params = symbol
            //         .paramuments
            //         .iter()
            //         .map(|param| {
            //             let param_value = match get_param_value(coredump, &symbol, &frame, &param) {
            //                 Ok(value) => value,
            //                 Err(err) => format!("<{}>", err),
            //             };
            //             format!("{}={}", param.name.green(), param_value)
            //         })
            //         .collect::<Vec<String>>()
            //         .join(", ");
            //     format!("{} ({}) (base={:?})", name.yellow(), params, symbol.base)
            // } else {
            //     format!("{} ()", name.yellow())
            // }
        };

        let addr = format!("{:0>6}", frame.funcidx).blue();
        println!("{} as {} at {}", addr, function, source);
    } else {
        // Functions that are generated by Wasi and don't have a source (ie
        // some Wasi transpolines) don't have a mapping in DWARF.
        let addr = format!("{:0>6}", frame.funcidx).blue();
        println!("{} as {} at <no location>", addr, frame.binary_name);
    }

    Ok(())
}

fn repl(
    coredump: &[u8],
    source: &wasm_edit::traverse::WasmModule,
    ddbug: ddbug_parser::FileHash<'_>,
) -> Result<(), BoxError> {
    // Load a section and return as `Cow<[u8]>`.
    let load_section = |id: gimli::SectionId| -> Result<borrow::Cow<[u8]>, gimli::Error> {
        if let Some(bytes) = source.get_custom_section(id.name()) {
            Ok(borrow::Cow::from(bytes))
        } else {
            warn!("DWARF section {} not found", id.name());
            Ok(borrow::Cow::Borrowed(&[][..]))
        }
    };

    let endian = gimli::RunTimeEndian::Little;

    // Load all of the sections.
    let dwarf_cow = gimli::Dwarf::load(&load_section)?;

    // Borrow a `Cow<[u8]>` to create an `EndianSlice`.
    let borrow_section: &dyn for<'a> Fn(
        &'a borrow::Cow<[u8]>,
    ) -> gimli::EndianSlice<'a, gimli::RunTimeEndian> =
        &|section| gimli::EndianSlice::new(&*section, endian);

    // Create `EndianSlice`s for all of the sections.
    let dwarf = Arc::new(dwarf_cow.borrow(&borrow_section));

    let stack_frames = decode_coredump(source, coredump)?;
    if stack_frames.len() == 0 {
        println!("No frames recorded");
    }

    // Start REPL

    let mut ctx = Context {
        ddbug,
        coredump,
        dwarf: Arc::clone(&dwarf),
        selected_frame: None,
        variables: HashMap::new(),
    };

    let stdin = io::stdin();
    loop {
        print!("wasmgdb> ");
        io::stdout().flush().unwrap();

        let line = stdin.lock().lines().next().unwrap()?;

        match parse_command(&line) {
            Ok((_, cmd)) => {
                if let Err(err) = run_command(&mut ctx, &stack_frames, cmd) {
                    error!("failed to run command ({}): {}", line, err);
                }
            }
            Err(err) => {
                error!("error while parsing ({}): {}", line, err);
            }
        }
    }
}

fn run_command<R: gimli::Reader>(
    ctx: &mut Context<R>,
    stack_frames: &Vec<StackFrame>,
    cmd: Command,
) -> Result<(), BoxError> {
    match cmd {
        Command::Backtrace => {
            backtrace(ctx, stack_frames)?;
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
                        let addr = get_param_addr(&selected_frame, func, variable)?;
                        let ptr = read_ptr(ctx.coredump, addr)?;

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

                let addr = get_param_addr(&selected_frame, func, variable)?;

                match format {
                    PrintFormat::String => {
                        let ptr = read_ptr(ctx.coredump, addr)?;

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

            print_frame(ctx, &stack_frame)?;
            select_frame(ctx, &stack_frame)?;

            ctx.selected_frame = Some(stack_frame.clone());
        }

        Command::Unknown => return Err("unknow command".into()),
    }

    Ok(())
}

enum Command<'a> {
    Unknown,
    Backtrace,
    SelectFrame(usize),
    Print(PrintFormat, &'a str),
    PrintDeref(PrintFormat, &'a str),
}

enum PrintFormat {
    None,
    String,
}

fn parse_command<'a>(input: &'a str) -> IResult<&'a str, Command<'a>> {
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

// impl Type {
//     fn size_of(&self) -> u64 {
//         if let TypeEnum::Ptr(_) = self.inner {
//             return 4;
//         }
//         if let Some(byte_size) = self.byte_size {
//             byte_size
//         } else {
//             unimplemented!()
//         }
//     }
// }

fn decode_coredump(
    source: &wasm_edit::traverse::WasmModule,
    coredump: &[u8],
) -> Result<Vec<StackFrame>, BoxError> {
    let mut addr = 0usize;
    let nframe = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
    addr += 4;
    let next_frame = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
    addr += 4;

    debug!("number of frames: {}", nframe);

    let mut stack_frames = Vec::with_capacity(nframe as usize);

    for i in 0..nframe {
        let funcidx = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
        addr += 4;
        let count_local = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
        addr += 4;

        let mut locals = Vec::with_capacity(count_local as usize);
        for _ in 0..count_local {
            let local = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
            locals.push(local);
            addr += 4;
        }

        let frame = StackFrame {
            binary_name: source
                .get_func_name(funcidx)
                .unwrap_or_else(|| "unknown".to_string()),
            funcidx,
            locals,
        };
        debug!("#{} stack frame {:?}", nframe - i - 1, frame);
        stack_frames.push(frame);
    }
    if nframe > 0 {
        assert_eq!(next_frame as usize, addr);
    }

    Ok(stack_frames)
}

fn backtrace<R: gimli::Reader>(
    ctx: &Context<R>,
    stack_frames: &Vec<StackFrame>,
) -> Result<(), BoxError> {
    let mut i = stack_frames.len();
    for frame in stack_frames {
        i -= 1;
        if let Some(selected_frame) = &ctx.selected_frame {
            if selected_frame.binary_name == frame.binary_name {
                print!("#{}*\t", i);
            } else {
                print!("#{}\t", i);
            }
        } else {
            print!("#{}\t", i);
        }

        print_frame(ctx, &frame)?;
    }

    Ok(())
}

/// Get the absolute addr of a member in memory
fn get_member_addr<'a>(addr: u32, member: &ddbug_parser::Member<'a>) -> Result<u32, BoxError> {
    let offset = member
        .data_location()
        .ok_or("no data location for member")?;
    Ok(addr + offset as u32)
}

/// Get the absolute addr of a function parameter in memory
fn get_param_addr<'a>(
    frame: &StackFrame,
    func: &ddbug_parser::Function<'a>,
    param: &ddbug_parser::Parameter<'a>,
) -> Result<u32, BoxError> {
    let location = param.data_location().ok_or("no data location for param")?;
    get_addr(frame, func, location)
}

/// Get the absolute addr in memory
fn get_addr<'a>(
    frame: &StackFrame,
    func: &ddbug_parser::Function<'a>,
    location: &ddbug_parser::DataLocation,
) -> Result<u32, BoxError> {
    let base = func.frame_base();
    let base = base.as_ref().ok_or("func has no base addr")?;

    let offset_from_base =
        if let ddbug_parser::DataLocation::OffsetFromBase(offset_from_base) = location {
            offset_from_base
        } else {
            unimplemented!()
        };

    match base {
        ddbug_parser::DataLocation::WasmLocal(base_local) => {
            if let Some(base_addr) = frame.locals.get(*base_local as usize) {
                Ok(base_addr + *offset_from_base as u32)
            } else {
                Err(format!("failed to load base addr in local {}", base_local).into())
            }
        }
        e => {
            warn!("implement {:?}", e);
            Err(format!("get_addr {:?} not implemented", e).into())
        }
    }
}

fn read_ptr(coredump: &[u8], addr: u32) -> Result<u32, BoxError> {
    let bytes = read(coredump, addr, 4)?;
    Ok(u32::from_le_bytes(bytes.try_into()?))
}

fn read<'a>(coredump: &'a [u8], addr: u32, size: u64) -> Result<&'a [u8], BoxError> {
    Ok(&coredump[(addr as usize)..(addr as usize + size as usize)])
}

struct Context<'a, R: gimli::Reader> {
    selected_frame: Option<StackFrame>,
    /// Variables present in the selected scope
    variables: HashMap<String, ddbug_parser::Parameter<'a>>,

    /// Input coredump, ie process memory image.
    coredump: &'a [u8],

    /// DWARF types
    dwarf: Arc<gimli::Dwarf<R>>,

    /// DWARF informations
    ddbug: ddbug_parser::FileHash<'a>,
}

pub fn main() -> Result<(), BoxError> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    let coredump_filename = args[1].clone();
    let source_filename = args[2].clone();

    let mut coredump = Vec::new();
    {
        let mut file = File::open(coredump_filename).expect("File not found");
        file.read_to_end(&mut coredump)
            .expect("Error while reading file");
    }

    let ctx = ddbug_parser::File::parse(source_filename.clone()).unwrap();
    let ddbug = ddbug_parser::FileHash::new(ctx.file());

    let mut source = Vec::new();
    {
        let mut file = File::open(source_filename).expect("File not found");
        file.read_to_end(&mut source)
            .expect("Error while reading file");
    }

    let source = wasm_edit::parser::decode(&source)
        .map_err(|err| format!("failed to parse Wasm module: {}", err))?;
    let source = wasm_edit::traverse::WasmModule::new(Arc::new(source));

    repl(&coredump, &source, ddbug)
}
