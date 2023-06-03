use crate::{
    util::{go_on, oops},
    ContextlessUpParser, ContextlessUpResult, ContextualUpParser, UpError,
    UpResult,
};

pub fn one_of<ParserSequence>(
    parsers: ParserSequence,
) -> OneOf<ParserSequence> {
    OneOf(parsers)
}

pub struct OneOf<ParserSequence>(ParserSequence);

trait ContextlessOneOfParserSequence<'input, Out> {
    fn contextless_one_of(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out>;
}
trait ContextualOneOfParserSequence<'input, Out, Ctx> {
    fn contextual_one_of(
        &self,
        input: &'input str,
        ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx>;
}

impl<'input, Out, ParserSequence> ContextlessUpParser<'input, Out>
    for OneOf<ParserSequence>
where
    ParserSequence: ContextlessOneOfParserSequence<'input, Out>,
{
    fn parse_contextless(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out> {
        self.0.contextless_one_of(input)
    }
}

impl<'input, Out, Ctx, ParserSequence> ContextualUpParser<'input, Out, Ctx>
    for OneOf<ParserSequence>
where
    ParserSequence: ContextualOneOfParserSequence<'input, Out, Ctx>,
{
    fn parse_contextual(
        &self,
        input: &'input str,
        ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx> {
        self.0.contextual_one_of(input, ctx)
    }
}

impl<'input, Out, Parser0> ContextlessOneOfParserSequence<'input, Out>
    for (Parser0,)
where
    Parser0: ContextlessUpParser<'input, Out>,
{
    fn contextless_one_of(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out> {
        let mut all_go_ons = Vec::new();
        let mut error = true;
        match self.0.parse_contextless(input) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn { go_on, .. }) => {
                all_go_ons.extend(go_on);
                error = false
            }
            Err(UpError::Oops { .. }) => {}
        }
        match error {
            true => Err(oops(input, "no branches could continue").no_ctx()),
            false => Err(go_on(all_go_ons).no_ctx()),
        }
    }
}

impl<'input, Out, Parser0, Parser1> ContextlessOneOfParserSequence<'input, Out>
    for (Parser0, Parser1)
where
    Parser0: ContextlessUpParser<'input, Out>,
    Parser1: ContextlessUpParser<'input, Out>,
{
    fn contextless_one_of(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out> {
        let mut all_go_ons = Vec::new();
        let mut error = true;
        match self.0.parse_contextless(input) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn { go_on, .. }) => {
                all_go_ons.extend(go_on);
                error = false
            }
            Err(UpError::Oops { .. }) => {}
        }
        match self.1.parse_contextless(input) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn { go_on, .. }) => {
                all_go_ons.extend(go_on);
                error = false
            }
            Err(UpError::Oops { .. }) => {}
        }
        match error {
            true => Err(oops(input, "no branches could continue").no_ctx()),
            false => Err(go_on(all_go_ons).no_ctx()),
        }
    }
}

parse_up_proc_macros::_impl_contextless_one_of_parser_sequence_for_tuples!(
    3..10
);

impl<'input, Out, Ctx, Parser0> ContextualOneOfParserSequence<'input, Out, Ctx>
    for (Parser0,)
where
    Parser0: ContextualUpParser<'input, Out, Ctx>,
{
    fn contextual_one_of(
        &self,
        input: &'input str,
        mut ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx> {
        let mut all_go_ons = Vec::new();
        let mut error = true;
        match self.0.parse_contextual(input, ctx) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn {
                go_on,
                ctx: new_ctx,
            }) => {
                all_go_ons.extend(go_on);
                error = false;
                ctx = new_ctx
            }
            Err(UpError::Oops { ctx: new_ctx, .. }) => ctx = new_ctx,
        }
        match error {
            true => Err(oops(input, "no branches could continue").ctx(ctx)),
            false => Err(go_on(all_go_ons).ctx(ctx)),
        }
    }
}

impl<'input, Out, Ctx, Parser0, Parser1>
    ContextualOneOfParserSequence<'input, Out, Ctx> for (Parser0, Parser1)
where
    Parser0: ContextualUpParser<'input, Out, Ctx>,
    Parser1: ContextualUpParser<'input, Out, Ctx>,
{
    fn contextual_one_of(
        &self,
        input: &'input str,
        mut ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx> {
        let mut all_go_ons = Vec::new();
        let mut error = true;
        match self.0.parse_contextual(input, ctx) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn {
                go_on,
                ctx: new_ctx,
            }) => {
                all_go_ons.extend(go_on);
                error = false;
                ctx = new_ctx
            }
            Err(UpError::Oops { ctx: new_ctx, .. }) => ctx = new_ctx,
        }
        match self.1.parse_contextual(input, ctx) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn {
                go_on,
                ctx: new_ctx,
            }) => {
                all_go_ons.extend(go_on);
                error = false;
                ctx = new_ctx
            }
            Err(UpError::Oops { ctx: new_ctx, .. }) => ctx = new_ctx,
        }
        match error {
            true => Err(oops(input, "no branches could continue").ctx(ctx)),
            false => Err(go_on(all_go_ons).ctx(ctx)),
        }
    }
}

parse_up_proc_macros::_impl_contextual_one_of_parser_sequence_for_tuples!(3..4);

#[cfg(test)]
mod tests {
    use crate::{contextless::tag, util::yes_and};

    use super::*;
    #[test]
    fn contextless() {
        assert_eq!(
            one_of((tag("true"),)).parse_contextless("true..."),
            Ok(yes_and("true", "...").no_ctx())
        );
        assert_eq!(
            one_of((tag("true"), tag("false"))).parse_contextless("true..."),
            Ok(yes_and("true", "...").no_ctx())
        );
        assert_eq!(
            one_of((tag("true"), tag("false"))).parse_contextless("false..."),
            Ok(yes_and("false", "...").no_ctx())
        );
        assert_eq!(
            one_of((tag("true"), tag("false"))).parse_contextless(""),
            Err(go_on(["true", "false"]).no_ctx()),
        );
        assert_eq!(
            one_of((tag("true"), tag("false"))).parse_contextless("t"),
            Err(go_on(["rue"]).no_ctx()),
        );
        assert_eq!(
            one_of((tag("true"), tag("false"))).parse_contextless("..."),
            Err(oops("...", "no branches could continue").no_ctx()),
        );

        assert_eq!(
            one_of((tag("yes"), tag("no"), tag("maybe")))
                .parse_contextless("maybe..."),
            Ok(yes_and("maybe", "...").no_ctx()),
        );
    }
}
