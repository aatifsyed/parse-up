use std::marker::PhantomData;

use crate::{
    util::yes_and, ContextlessUpParser, ContextlessUpResult, ContextualUpParser, UpResult,
};

pub fn recognize<Parser, Out>(parser: Parser) -> Recognize<Parser, Out> {
    Recognize(parser, PhantomData)
}

pub struct Recognize<T, Out>(T, PhantomData<Out>);

impl<'input, Out, Parser> ContextlessUpParser<'input, &'input str> for Recognize<Parser, Out>
where
    Parser: ContextlessUpParser<'input, Out>,
{
    fn parse_contextless(&self, input: &'input str) -> ContextlessUpResult<'input, &'input str> {
        let (and, _, _) = self.0.parse_contextless(input)?.cont();
        Ok(yes_and(input.strip_suffix(and).unwrap(), and).no_ctx())
    }
}

impl<'input, Out, Ctx, Parser> ContextualUpParser<'input, &'input str, Ctx>
    for Recognize<Parser, Out>
where
    Parser: ContextualUpParser<'input, Out, Ctx>,
{
    fn parse_contextual(&self, input: &'input str, ctx: Ctx) -> UpResult<'input, &'input str, Ctx> {
        let (and, _, ctx) = self.0.parse_contextual(input, ctx)?.cont();
        Ok(yes_and(input.strip_suffix(and).unwrap(), and).ctx(ctx))
    }
}

#[cfg(test)]
mod tests {
    use crate::{series, tag};

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
