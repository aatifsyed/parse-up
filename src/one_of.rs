use crate::{
    util::{go_on, oops},
    ContextlessUpParser, ContextlessUpResult, ContextualUpParser,
    Suggestions::{self, Closed, Open},
    UpError, UpResult,
};

pub fn one_of<ParserSequence>(parsers: ParserSequence) -> OneOf<ParserSequence> {
    OneOf(parsers)
}

pub fn one_of_iter<T>(parser: impl IntoIterator<Item = T>) -> OneOf<Vec<T>> {
    OneOf(parser.into_iter().collect())
}

pub struct OneOf<ParserSequence>(ParserSequence);

trait ContextlessOneOfParserSequence<'input, Out> {
    fn contextless_one_of(&self, input: &'input str) -> ContextlessUpResult<'input, Out>;
}
trait ContextualOneOfParserSequence<'input, Out, Ctx> {
    fn contextual_one_of(&self, input: &'input str, ctx: Ctx) -> UpResult<'input, Out, Ctx>;
}

impl<'input, Out, ParserSequence> ContextlessUpParser<'input, Out> for OneOf<ParserSequence>
where
    ParserSequence: ContextlessOneOfParserSequence<'input, Out>,
{
    fn parse_contextless(&self, input: &'input str) -> ContextlessUpResult<'input, Out> {
        self.0.contextless_one_of(input)
    }
}

impl<'input, Out, Ctx, ParserSequence> ContextualUpParser<'input, Out, Ctx>
    for OneOf<ParserSequence>
where
    ParserSequence: ContextualOneOfParserSequence<'input, Out, Ctx>,
{
    fn parse_contextual(&self, input: &'input str, ctx: Ctx) -> UpResult<'input, Out, Ctx> {
        self.0.contextual_one_of(input, ctx)
    }
}

fn fold_suggestions(
    suggestions: Suggestions,
    all_suggestions: &mut Vec<String>,
    open: &mut bool,
    error: &mut bool,
) {
    match suggestions {
        Open(suggestions) => {
            *open = true;
            all_suggestions.extend(suggestions)
        }
        Closed(first, rest) => {
            all_suggestions.push(first);
            all_suggestions.extend(rest)
        }
    }
    *error = false;
}
fn finalise_suggestions<Ctx>(
    input: &str,
    all_suggestions: Vec<String>,
    error: bool,
    open: bool,
    ctx: Ctx,
) -> UpError<Ctx> {
    match error {
        true => oops(input, "no branches could continue").ctx(ctx),
        false => match open {
            true => go_on(all_suggestions).open().ctx(ctx),
            false => go_on(all_suggestions).closed().ctx(ctx),
        },
    }
}
impl<'input, Out, Parser> ContextlessOneOfParserSequence<'input, Out> for Vec<Parser>
where
    Parser: ContextlessUpParser<'input, Out>,
{
    fn contextless_one_of(&self, input: &'input str) -> ContextlessUpResult<'input, Out> {
        self.as_slice().contextless_one_of(input)
    }
}

impl<'input, Out, Parser> ContextlessOneOfParserSequence<'input, Out> for &[Parser]
where
    Parser: ContextlessUpParser<'input, Out>,
{
    fn contextless_one_of(&self, input: &'input str) -> ContextlessUpResult<'input, Out> {
        let mut all_suggestions = Vec::new();
        let mut open = false;
        let mut error = true;
        for parser in self.iter() {
            match parser.parse_contextless(input) {
                Ok(o) => return Ok(o),
                Err(UpError::GoOn { go_on, .. }) => {
                    fold_suggestions(go_on, &mut all_suggestions, &mut open, &mut error)
                }
                Err(UpError::Oops { .. }) => {}
            }
        }
        Err(finalise_suggestions(
            input,
            all_suggestions,
            error,
            open,
            (),
        ))
    }
}

impl<'input, Out, Parser, const N: usize> ContextlessOneOfParserSequence<'input, Out>
    for [Parser; N]
where
    Parser: ContextlessUpParser<'input, Out>,
{
    fn contextless_one_of(&self, input: &'input str) -> ContextlessUpResult<'input, Out> {
        self.as_slice().contextless_one_of(input)
    }
}

impl<'input, Out, Parser0, Parser1> ContextlessOneOfParserSequence<'input, Out>
    for (Parser0, Parser1)
where
    Parser0: ContextlessUpParser<'input, Out>,
    Parser1: ContextlessUpParser<'input, Out>,
{
    fn contextless_one_of(&self, input: &'input str) -> ContextlessUpResult<'input, Out> {
        let mut all_suggestions = Vec::new();
        let mut open = false;
        let mut error = true;
        match self.0.parse_contextless(input) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn { go_on, .. }) => {
                fold_suggestions(go_on, &mut all_suggestions, &mut open, &mut error)
            }
            Err(UpError::Oops { .. }) => {}
        }
        match self.1.parse_contextless(input) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn { go_on, .. }) => {
                fold_suggestions(go_on, &mut all_suggestions, &mut open, &mut error)
            }
            Err(UpError::Oops { .. }) => {}
        }
        Err(finalise_suggestions(
            input,
            all_suggestions,
            error,
            open,
            (),
        ))
    }
}

parse_up_proc_macros::_impl_contextless_one_of_parser_sequence_for_tuples!(1, 3..10);

impl<'input, Out, Ctx, Parser0, Parser1> ContextualOneOfParserSequence<'input, Out, Ctx>
    for (Parser0, Parser1)
where
    Parser0: ContextualUpParser<'input, Out, Ctx>,
    Parser1: ContextualUpParser<'input, Out, Ctx>,
{
    fn contextual_one_of(&self, input: &'input str, mut ctx: Ctx) -> UpResult<'input, Out, Ctx> {
        let mut all_suggestions = Vec::new();
        let mut open = false;
        let mut error = true;
        match self.0.parse_contextual(input, ctx) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn {
                go_on,
                ctx: new_ctx,
            }) => {
                fold_suggestions(go_on, &mut all_suggestions, &mut open, &mut error);
                ctx = new_ctx
            }
            Err(UpError::Oops { ctx: new_ctx, .. }) => ctx = new_ctx,
        }
        match self.0.parse_contextual(input, ctx) {
            Ok(o) => return Ok(o),
            Err(UpError::GoOn {
                go_on,
                ctx: new_ctx,
            }) => {
                fold_suggestions(go_on, &mut all_suggestions, &mut open, &mut error);
                ctx = new_ctx
            }
            Err(UpError::Oops { ctx: new_ctx, .. }) => ctx = new_ctx,
        }
        Err(finalise_suggestions(
            input,
            all_suggestions,
            error,
            open,
            ctx,
        ))
    }
}

parse_up_proc_macros::_impl_contextual_one_of_parser_sequence_for_tuples!(1, 3..10);

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
            Err(go_on(["true", "false"]).closed().no_ctx()),
        );
        assert_eq!(
            one_of((tag("true"), tag("false"))).parse_contextless("t"),
            Err(go_on(["rue"]).closed().no_ctx()),
        );
        assert_eq!(
            one_of((tag("true"), tag("false"))).parse_contextless("f"),
            Err(go_on(["alse"]).closed().no_ctx()),
        );
        assert_eq!(
            one_of((tag("true"), tag("false"))).parse_contextless("..."),
            Err(oops("...", "no branches could continue").no_ctx()),
        );

        assert_eq!(
            one_of((tag("yes"), tag("no"), tag("maybe"))).parse_contextless("maybe..."),
            Ok(yes_and("maybe", "...").no_ctx()),
        );
    }

    #[test]
    fn contextless_arrs() {
        let parser = one_of([tag("yes"), tag("no")]);
        assert_eq!(
            parser.parse_contextless("yes..."),
            Ok(yes_and("yes", "...").no_ctx())
        )
    }
}
