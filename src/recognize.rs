use std::marker::PhantomData;

use crate::{ContextlessUpParser, ContextlessUpResult, ContextualUpParser, UpResult, YesAnd};

pub fn recognize<Parser, Out>(parser: Parser) -> Recognize<Parser, Out> {
    Recognize(parser, PhantomData)
}

pub struct Recognize<T, Out>(T, PhantomData<Out>);

impl<'input, Out, Parser> ContextlessUpParser<'input, &'input str> for Recognize<Parser, Out>
where
    Parser: ContextlessUpParser<'input, Out>,
{
    fn parse_contextless(&self, input: &'input str) -> ContextlessUpResult<'input, &'input str> {
        let YesAnd {
            yes: _,
            and,
            could_also,
            ctx,
        } = self.0.parse_contextless(input)?;
        Ok(YesAnd {
            yes: input.strip_suffix(and).unwrap(),
            and,
            could_also,
            ctx,
        })
    }
}

impl<'input, Out, Ctx, Parser> ContextualUpParser<'input, &'input str, Ctx>
    for Recognize<Parser, Out>
where
    Parser: ContextualUpParser<'input, Out, Ctx>,
{
    fn parse_contextual(&self, input: &'input str, ctx: Ctx) -> UpResult<'input, &'input str, Ctx> {
        let YesAnd {
            yes: _,
            and,
            could_also,
            ctx,
        } = self.0.parse_contextual(input, ctx)?;
        Ok(YesAnd {
            yes: input.strip_suffix(and).unwrap(),
            and,
            could_also,
            ctx,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{series, tag, util::yes_and};

    use super::*;
    #[test]
    fn test() {
        let parser = recognize(series((tag("hello"), tag("world"))));
        assert_eq!(
            parser.parse_contextless("helloworld..."),
            Ok(yes_and("helloworld", "...").no_ctx())
        );
    }
}
