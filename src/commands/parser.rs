use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while1;
use nom::character::complete::alpha1;
use nom::character::complete::digit1;
use nom::character::complete::space1;
use nom::character::is_alphabetic;
use nom::combinator::map;
use nom::combinator::map_res;
use nom::combinator::opt;
use nom::sequence::delimited;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::IResult;

use crate::commands::{Command, Expr, PrintFormat};

fn ident<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    take_while1(|c: char| is_alphabetic(c as u8) || c == '_')(input)
}

fn parse_single_expr<'a>(input: &'a str) -> IResult<&'a str, Expr<'a>> {
    map(ident, |n| Expr::Name(n))(input)
}

fn parse_parens_expr<'a>(input: &'a str) -> IResult<&'a str, Expr<'a>> {
    delimited(tag("("), parse_expr, tag(")"))(input)
}

fn parse_expr<'a>(input: &'a str) -> IResult<&'a str, Expr<'a>> {
    alt((
        parse_expr_member_access,
        parse_parens_expr,
        parse_expr_deref,
        parse_single_expr,
    ))(input)
}

fn parse_expr_deref<'a>(input: &'a str) -> IResult<&'a str, Expr<'a>> {
    map(preceded(tag("*"), parse_expr), |expr| {
        Expr::Deref(Box::new(expr))
    })(input)
}

/// Member access lhs accepts a single expression or a parenthesed expression
fn parse_expr_member_access<'a>(input: &'a str) -> IResult<&'a str, Expr<'a>> {
    let object = alt((parse_parens_expr, parse_single_expr));

    map(
        tuple((object, tag("->"), ident)),
        |(object, _, member_access)| Expr::MemberAccess(Box::new(object), member_access),
    )(input)
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

            let (input, what) = preceded(space1, parse_expr)(input)?;
            (input, Command::Print(format, what))
        }
        "f" => {
            let (input, n) = preceded(tag(" "), digit1)(input)?;
            let n = n.parse::<usize>().unwrap();

            (input, Command::SelectFrame(n))
        }
        _ => (input, Command::Unknown),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr_name() {
        use Expr::*;

        let (_, cmd) = parse_expr("test").unwrap();
        assert_eq!(cmd, Name("test"));
    }

    #[test]
    fn test_expr_member_access() {
        use Expr::*;

        let (_, cmd) = parse_expr("test->ab_").unwrap();
        assert_eq!(cmd, MemberAccess(Box::new(Name("test")), "ab_"));
    }

    #[test]
    fn test_expr_deref_name() {
        use Expr::*;

        let (_, cmd) = parse_expr("*test").unwrap();
        assert_eq!(cmd, Deref(Box::new(Name("test"))));
    }

    #[test]
    fn test_expr_deref_member_access() {
        use Expr::*;

        let (_, cmd) = parse_expr("*test->ab").unwrap();
        assert_eq!(
            cmd,
            Deref(Box::new(MemberAccess(Box::new(Name("test")), "ab")))
        );
    }

    #[test]
    fn test_expr_deref_parens_deref() {
        use Expr::*;

        let (_, cmd) = parse_expr("*(*test)").unwrap();
        assert_eq!(cmd, Deref(Box::new(Deref(Box::new(Name("test"))))));
    }

    #[test]
    fn test_expr_deref_parens_member_access() {
        use Expr::*;

        let (_, cmd) = parse_expr("(*test)->ab").unwrap();
        assert_eq!(
            cmd,
            MemberAccess(Box::new(Deref(Box::new(Name("test")))), "ab")
        );
    }
}
