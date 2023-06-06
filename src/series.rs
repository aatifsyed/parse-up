#![allow(unused_assignments, clippy::let_unit_value)]
use crate::{
    util::yes_and,
    ContextlessUpParser, ContextlessUpResult, ContextualUpParser,
    Suggestions::{self, Closed, Open},
    UpResult, YesAnd,
};

use std::iter::once;

pub fn series<ParserSequence>(
    parsers: ParserSequence,
) -> Series<ParserSequence> {
    Series(parsers)
}

pub struct Series<ParserSequence>(ParserSequence);

trait ContextlessSeriesParserSequence<'input, Out> {
    fn contextless_series(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out>;
}
trait ContextualSeriesParserSequence<'input, Out, Ctx> {
    fn contextual_series(
        &self,
        input: &'input str,
        ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx>;
}

impl<'input, Out, ParserSequence> ContextlessUpParser<'input, Out>
    for Series<ParserSequence>
where
    ParserSequence: ContextlessSeriesParserSequence<'input, Out>,
{
    fn parse_contextless(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out> {
        self.0.contextless_series(input)
    }
}

impl<'input, Out, Ctx, ParserSequence> ContextualUpParser<'input, Out, Ctx>
    for Series<ParserSequence>
where
    ParserSequence: ContextualSeriesParserSequence<'input, Out, Ctx>,
{
    fn parse_contextual(
        &self,
        input: &'input str,
        ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx> {
        self.0.contextual_series(input, ctx)
    }
}

fn finalize_suggestions<T, Ctx>(
    final_suggestions: Option<Suggestions>,
    yeses: T,
    input: &str,
    ctx: Ctx,
) -> YesAnd<T, Ctx> {
    match final_suggestions {
        None => yes_and(yeses, input).ctx(ctx),
        Some(Open(suggestions)) => {
            yes_and(yeses, input).open(suggestions).ctx(ctx)
        }
        Some(Closed(first, rest)) => yes_and(yeses, input)
            .closed(once(first).chain(rest))
            .ctx(ctx),
    }
}

impl<'input, Out0, Parser0, Out1, Parser1>
    ContextlessSeriesParserSequence<'input, (Out0, Out1)> for (Parser0, Parser1)
where
    Parser0: ContextlessUpParser<'input, Out0>,
    Parser1: ContextlessUpParser<'input, Out1>,
{
    fn contextless_series(
        &self,
        mut input: &'input str,
    ) -> ContextlessUpResult<'input, (Out0, Out1)> {
        let mut ctx = ();
        let mut final_suggestions;
        let yeses = (
            {
                let YesAnd {
                    yes,
                    and,
                    could_also,
                    ctx: new_ctx,
                } = self.0.parse_contextless(input)?;
                input = and;
                final_suggestions = could_also;
                ctx = new_ctx;
                yes
            },
            {
                let YesAnd {
                    yes,
                    and,
                    could_also,
                    ctx: new_ctx,
                } = self.1.parse_contextless(input)?;
                input = and;
                final_suggestions = could_also;
                ctx = new_ctx;
                yes
            },
        );
        Ok(finalize_suggestions(final_suggestions, yeses, input, ctx))
    }
}

parse_up_proc_macros::_impl_contextless_series_parser_sequence_for_tuples!(
    1..=1
);
parse_up_proc_macros::_impl_contextless_series_parser_sequence_for_tuples!(
    3..4
);

impl<'input, Ctx, Parser0, Parser1, Out0, Out1>
    ContextualSeriesParserSequence<'input, (Out0, Out1), Ctx>
    for (Parser0, Parser1)
where
    Parser0: ContextualUpParser<'input, Out0, Ctx>,
    Parser1: ContextualUpParser<'input, Out1, Ctx>,
{
    fn contextual_series(
        &self,
        mut input: &'input str,
        mut ctx: Ctx,
    ) -> UpResult<'input, (Out0, Out1), Ctx> {
        let mut final_suggestions;
        let yeses = (
            {
                let YesAnd {
                    yes,
                    and,
                    could_also,
                    ctx: new_ctx,
                } = self.0.parse_contextual(input, ctx)?;
                input = and;
                final_suggestions = could_also;
                ctx = new_ctx;
                yes
            },
            {
                let YesAnd {
                    yes,
                    and,
                    could_also,
                    ctx: new_ctx,
                } = self.1.parse_contextual(input, ctx)?;
                input = and;
                final_suggestions = could_also;
                ctx = new_ctx;
                yes
            },
        );
        Ok(finalize_suggestions(final_suggestions, yeses, input, ctx))
    }
}

parse_up_proc_macros::_impl_contextual_series_parser_sequence_for_tuples!(3..4);

#[cfg(test)]
mod tests {
    use crate::{
        contextless::{tag, whitespace},
        util::go_on,
    };

    use super::*;
    #[test]
    fn contextless() {
        let hello_world = series((tag("hello"), whitespace, tag("world")));
        assert_eq!(
            hello_world.parse_contextless(""),
            Err(go_on(["hello"]).closed().no_ctx()),
        );
        assert_eq!(
            hello_world.parse_contextless("hello"),
            Err(go_on([" "]).open().no_ctx())
        );
        assert_eq!(
            hello_world.parse_contextless("hello "),
            Err(go_on(["world"]).closed().no_ctx())
        );
        assert_eq!(
            hello_world.parse_contextless("hello world..."),
            Ok(yes_and(("hello", " ", "world"), "...").no_ctx())
        );
    }
}
