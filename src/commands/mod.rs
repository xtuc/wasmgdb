//! Handles the parsing and execution of commands

use std::fmt;

use crate::{coredump, BoxError, Context};

mod frames;
pub(crate) mod parser;
mod print;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expr<'a> {
    Name(&'a str),
    Deref(Box<Expr<'a>>),
    MemberAccess(Box<Expr<'a>>, &'a str),
}

impl<'a> Expr<'a> {
    pub(crate) fn object(&'a self) -> &'a str {
        match self {
            Expr::Name(n) => n,
            Expr::Deref(t) => t.object(),
            Expr::MemberAccess(o, _) => o.object(),
        }
    }
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Name(v) => write!(f, "{}", v),
            Expr::Deref(t) => write!(f, "*{}", t),
            Expr::MemberAccess(expr, v) => write!(f, "{}.{}", expr, v),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Command<'a> {
    Unknown,
    Backtrace,
    SelectFrame(usize),
    Print(PrintFormat, Expr<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum PrintFormat {
    None,
    String,
}

pub(crate) fn run_command(
    ctx: &mut Context,
    stack_frames: &Vec<coredump::StackFrame>,
    cmd: Command,
) -> Result<(), BoxError> {
    match cmd {
        Command::Backtrace => {
            frames::backtrace(ctx, stack_frames)?;
        }

        Command::Print(format, what) => {
            print::print(&ctx, format, what)?;
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
