use crate::{
    util::{go_on, oops, yes_and},
    ContextlessUpParser, ContextlessUpResult, ContextualUpParser, UpError,
    UpResult, YesAnd,
};

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

impl<'input, Out0, Parser0> ContextlessSeriesParserSequence<'input, (Out0,)>
    for (Parser0,)
where
    Parser0: ContextlessUpParser<'input, Out0>,
{
    fn contextless_series(
        &self,
        mut input: &'input str,
    ) -> ContextlessUpResult<'input, (Out0,)> {
        let mut final_could_also = Vec::new();
        let yeses = ({
            let YesAnd {
                yes,
                and,
                could_also,
                ctx: _,
            } = self.0.parse_contextless(input)?;
            input = and;
            final_could_also = could_also;
            yes
        },);
        Ok(yes_and(yeses, input).also(final_could_also).no_ctx())
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
        let mut final_could_also = Vec::new();
        let yeses = (
            {
                let YesAnd {
                    yes,
                    and,
                    could_also,
                    ctx: _,
                } = self.0.parse_contextless(input)?;
                input = and;
                final_could_also = could_also;
                yes
            },
            {
                let YesAnd {
                    yes,
                    and,
                    could_also,
                    ctx: _,
                } = self.1.parse_contextless(input)?;
                input = and;
                final_could_also = could_also;
                yes
            },
        );
        Ok(yes_and(yeses, input).also(final_could_also).no_ctx())
    }
}

parse_up_proc_macros::_impl_contextless_series_parser_sequence_for_tuples!(
    3..4
);

impl<'input, Ctx, Parser0, Out0>
    ContextualSeriesParserSequence<'input, (Out0,), Ctx> for (Parser0,)
where
    Parser0: ContextualUpParser<'input, Out0, Ctx>,
{
    fn contextual_series(
        &self,
        mut input: &'input str,
        mut ctx: Ctx,
    ) -> UpResult<'input, (Out0,), Ctx> {
        let mut final_could_also = Vec::new();
        let yeses = ({
            let YesAnd {
                yes,
                and,
                could_also,
                ctx: new_ctx,
            } = self.0.parse_contextual(input, ctx)?;
            input = and;
            ctx = new_ctx;
            final_could_also = could_also;
            yes
        },);
        Ok(yes_and(yeses, input).also(final_could_also).ctx(ctx))
    }
}

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
        let mut final_could_also = Vec::new();
        let yeses = (
            {
                let YesAnd {
                    yes,
                    and,
                    could_also,
                    ctx: new_ctx,
                } = self.0.parse_contextual(input, ctx)?;
                input = and;
                ctx = new_ctx;
                final_could_also = could_also;
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
                ctx = new_ctx;
                final_could_also = could_also;
                yes
            },
        );
        Ok(yes_and(yeses, input).also(final_could_also).ctx(ctx))
    }
}

parse_up_proc_macros::_impl_contextual_series_parser_sequence_for_tuples!(3..4);

#[cfg(test)]
mod tests {
    use crate::contextless::{tag, whitespace};

    use super::*;
    #[test]
    fn contextless() {
        let hello_world = series((tag("hello"), whitespace, tag("world")));
        assert_eq!(
            hello_world.parse_contextless(""),
            Err(go_on(["hello"]).no_ctx()),
        );
        assert_eq!(
            hello_world.parse_contextless("hello"),
            Err(go_on([" "]).no_ctx())
        );
        assert_eq!(
            hello_world.parse_contextless("hello "),
            Err(go_on(["world"]).no_ctx())
        );
        assert_eq!(
            hello_world.parse_contextless("hello world..."),
            Ok(yes_and(("hello", " ", "world"), "...").no_ctx())
        );
    }
}
