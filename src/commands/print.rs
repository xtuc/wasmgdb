use crate::commands::{Expr, PrintFormat};
use crate::print_value;
use crate::{coredump, memory, BoxError, Context};
use colored::Colorize;
use log::error;
use std::fmt::Write;
use wasmgdb_ddbug_parser as ddbug_parser;

fn get_member<'a>(
    ty: ddbug_parser::Type<'a>,
    search: &str,
) -> Result<ddbug_parser::Member<'a>, BoxError> {
    for member in ty.members() {
        if search == member.name().unwrap() {
            return Ok(member.clone());
        }
    }

    Err(format!("member {} not found in object type {}", search, ty).into())
}

struct EvaluationCtx<'a, 'b> {
    frame: &'b coredump::StackFrame,
    func: &'b ddbug_parser::Function<'a>,
    param: &'b ddbug_parser::Parameter<'a>,
    ddbug: &'b ddbug_parser::FileHash<'a>,
    coredump: &'a [u8],
}

struct EvaluationResult<'a> {
    addr: u32,
    ty: ddbug_parser::Type<'a>,
    expr: Expr<'a>,
}

fn evaluate_expr<'a, 'b>(
    ctx: &'b EvaluationCtx<'a, 'b>,
    base_addr: u32,
    expr: Expr<'a>,
    expr_type: ddbug_parser::Type<'a>,
) -> Result<EvaluationResult<'a>, BoxError> {
    match &expr {
        Expr::Name(_) => Ok(EvaluationResult {
            addr: base_addr,
            ty: expr_type,
            expr,
        }),

        Expr::Deref(target) => {
            match expr_type.kind() {
                ddbug_parser::TypeKind::Modifier(type_modifier)
                    if type_modifier.kind() == ddbug_parser::TypeModifierKind::Pointer =>
                {
                    // *base_addr
                    let addr = memory::read_ptr(ctx.coredump, base_addr)?;
                    let ty = type_modifier
                        .ty(&ctx.ddbug)
                        .ok_or("unknown target type")?
                        .into_owned();

                    Ok(EvaluationResult {
                        addr,
                        ty,
                        expr: *target.clone(),
                    })
                }
                _ => return Err(format!("variable {} is not a ptr", target).into()),
            }
        }

        Expr::MemberAccess(base, member_access) => {
            // FIXME: assume for now base is the input expr. ie only works for one level of member.
            let base = evaluate_expr(ctx, base_addr, *base.clone(), expr_type)?;

            let member = get_member(base.ty, member_access)?;

            let addr = base.addr + member.data_location().unwrap() as u32;
            let ty = member.ty(&ctx.ddbug).unwrap().into_owned();

            Ok(EvaluationResult {
                addr,
                ty,
                expr: Expr::Name(member_access),
            })
        }
    }
}

pub(crate) fn print<'a>(
    ctx: &Context,
    format: PrintFormat,
    what: Expr<'a>,
) -> Result<(), BoxError> {
    if let Some(variable) = ctx.variables.get(what.object()) {
        let selected_frame = ctx
            .selected_frame
            .as_ref()
            .ok_or("no frame has been selected")?;
        let func = *ctx
            .ddbug
            .functions_by_linkage_name
            .get(&selected_frame.binary_name)
            .ok_or(format!("function {} not found", selected_frame.binary_name))?;

        let what_type = variable.ty(&ctx.ddbug).unwrap();
        let base_addr = memory::get_param_addr(&selected_frame, &func, &variable)?;

        // Evaluate the `what` expression
        let eval_ctx = EvaluationCtx {
            frame: selected_frame,
            func,
            param: variable,
            ddbug: &ctx.ddbug,
            coredump: ctx.coredump,
        };
        let result = evaluate_expr(&eval_ctx, base_addr, what, what_type.into_owned())?;

        match format {
            PrintFormat::String => {
                let ptr = memory::read_ptr(ctx.coredump, result.addr)?;

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

                println!("{} ({} char(s)) = {}", result.expr, out.len(), out);
            }
            PrintFormat::None => {
                let out = print_value(&ctx, result.addr, &result.ty, 0)?;
                println!("{}: {}", result.expr, out);
            }
        }
    } else {
        error!("variable {} not found", what);
    }

    Ok(())
}
