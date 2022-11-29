use crate::commands::{Expr, PrintFormat};
use crate::print_value;
use crate::{coredump, memory, BoxError, Context};
use log::error;
use std::fmt::Write;
use wasmgdb_ddbug_parser as ddbug_parser;

fn find_type_by_name<'a>(
    ddbug: &ddbug_parser::FileHash<'a>,
    name: &'_ str,
) -> Option<&'a ddbug_parser::Type<'a>> {
    for (_, t) in &ddbug.types {
        if t.to_string() == name {
            return Some(t);
        }
    }

    None
}

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
    expr_type: Option<ddbug_parser::Type<'a>>,
) -> Result<EvaluationResult<'a>, BoxError> {
    match &expr {
        Expr::Name(_) => Ok(EvaluationResult {
            addr: base_addr,
            ty: expr_type.unwrap(),
            expr,
        }),
        Expr::Hex(addr) => Ok(EvaluationResult {
            addr: *addr,
            ty: expr_type.unwrap(),
            expr,
        }),

        Expr::Cast(typename, expr) => {
            let type_ = find_type_by_name(ctx.ddbug, typename)
                .ok_or(format!("type {} not found", typename))?;
            evaluate_expr(ctx, base_addr, *expr.clone(), Some(type_.to_owned()))
        }

        Expr::Deref(target) => {
            match expr_type.unwrap().kind() {
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

pub(crate) fn print<'a, R: gimli::Reader>(
    ctx: &Context<R>,
    format: PrintFormat,
    what: Expr<'a>,
) -> Result<(), BoxError> {
    if let Some(object) = what.object() {
        if let Some(variable) = ctx.variables.get(object) {
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
                ddbug: &ctx.ddbug,
                coredump: ctx.coredump,
            };
            let result = evaluate_expr(&eval_ctx, base_addr, what, Some(what_type.into_owned()))?;

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
                    println!("{} (0x{:x}): {}", result.expr, result.addr, out);
                }
            }
        } else {
            error!("variable {} not found", what);
        }
    } else {
        // Evaluate the `what` expression
        let eval_ctx = EvaluationCtx {
            ddbug: &ctx.ddbug,
            coredump: ctx.coredump,
        };
        let result = evaluate_expr(&eval_ctx, 0, what, None)?;
        let out = print_value(&ctx, result.addr, &result.ty, 0)?;
        println!("{} (0x{:x}): {}", result.expr, result.addr, out);
    }

    Ok(())
}
