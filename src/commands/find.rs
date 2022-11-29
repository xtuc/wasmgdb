use crate::commands::Expr;
use crate::{BoxError, Context};
use colored::Colorize;

pub(crate) fn find<'a, R: gimli::Reader>(
    ctx: &Context<R>,
    start: Option<Expr<'a>>,
    end: Option<Expr<'a>>,
    txt: &'a str,
) -> Result<(), BoxError> {
    let start = if let Some(Expr::Hex(v)) = start {
        v as usize
    } else {
        0
    };
    let end = if let Some(Expr::Hex(v)) = end {
        v as usize
    } else {
        ctx.coredump.len()
    };

    let search_bytes = txt.as_bytes();
    let mem = &ctx.coredump[start..end];

    let mut offset = 0;
    let mut found = 0;
    let mut last_offset = 0;

    for window in mem.windows(search_bytes.len()) {
        if window == search_bytes {
            let v = format!("0x{:x}", offset);
            let distance_from_last = offset - last_offset;
            println!("{} after {} byte(s)", v.blue(), distance_from_last);

            found += 1;
            last_offset = offset;
        }

        offset += 1;
    }

    println!("{} pattern(s) found.", found);
    Ok(())
}

// x/16384s 0x18a1b90
