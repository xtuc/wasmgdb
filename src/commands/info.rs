use crate::{BoxError, Context};

pub(crate) fn info<'a, R: gimli::Reader>(ctx: &Context<R>, what: &'a str) -> Result<(), BoxError> {
    match what {
        "types" => {
            if ctx.ddbug.types.len() == 0 {
                println!("no types defined.");
            }
            for (_, t) in &ctx.ddbug.types {
                println!("{}", t);
            }

            Ok(())
        }

        _ => Err(format!("info {} not implemented", what).into()),
    }
}
