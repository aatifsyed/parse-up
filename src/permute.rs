use crate::{
    ext::ContextlessUpParserExt, one_of::one_of, series::series,
    ContextlessUpParser, ContextlessUpResult, ContextualUpParser, UpResult,
    YesAnd,
};

pub fn permute<ParserSequence>(
    parsers: ParserSequence,
) -> Permute<ParserSequence> {
    Permute(parsers)
}

pub struct Permute<ParserSequence>(ParserSequence);

trait ContextlessPermuteParserSequence<'input, Out> {
    fn contextless_permute(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out>;
}
trait ContextualPermuteParserSequence<'input, Out, Ctx> {
    fn contextual_permute(
        &self,
        input: &'input str,
        ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx>;
}

impl<'input, Out, ParserSequence> ContextlessUpParser<'input, Out>
    for Permute<ParserSequence>
where
    ParserSequence: ContextlessPermuteParserSequence<'input, Out>,
{
    fn parse_contextless(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out> {
        self.0.contextless_permute(input)
    }
}

impl<'input, Out, Ctx, ParserSequence> ContextualUpParser<'input, Out, Ctx>
    for Permute<ParserSequence>
where
    ParserSequence: ContextualPermuteParserSequence<'input, Out, Ctx>,
{
    fn parse_contextual(
        &self,
        input: &'input str,
        ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx> {
        self.0.contextual_permute(input, ctx)
    }
}

impl<'input, Out0, Parser0> ContextlessPermuteParserSequence<'input, (Out0,)>
    for (Parser0,)
where
    Parser0: ContextlessUpParser<'input, Out0>,
{
    fn contextless_permute(
        &self,
        mut input: &'input str,
    ) -> ContextlessUpResult<'input, (Out0,)> {
        one_of((series((self.0.borrowed(),)).map_yes(|(yes0,)| (yes0,)),))
            .parse_contextless(input)
    }
}

impl<'input, Out0, Out1, Parser0, Parser1>
    ContextlessPermuteParserSequence<'input, (Out0, Out1)>
    for (Parser0, Parser1)
where
    Parser0: ContextlessUpParser<'input, Out0>,
    Parser1: ContextlessUpParser<'input, Out1>,
{
    fn contextless_permute(
        &self,
        mut input: &'input str,
    ) -> ContextlessUpResult<'input, (Out0, Out1)> {
        one_of((
            series((self.0.borrowed(), self.1.borrowed()))
                .map_yes(|(yes0, yes1)| (yes0, yes1)),
            series((self.1.borrowed(), self.0.borrowed()))
                .map_yes(|(yes1, yes0)| (yes0, yes1)),
        ))
        .parse_contextless(input)
    }
}

impl<'input, Out0, Out1, Out2, Parser0, Parser1, Parser2>
    ContextlessPermuteParserSequence<'input, (Out0, Out1, Out2)>
    for (Parser0, Parser1, Parser2)
where
    Parser0: ContextlessUpParser<'input, Out0>,
    Parser1: ContextlessUpParser<'input, Out1>,
    Parser2: ContextlessUpParser<'input, Out2>,
{
    fn contextless_permute(
        &self,
        mut input: &'input str,
    ) -> ContextlessUpResult<'input, (Out0, Out1, Out2)> {
        one_of((
            series((
                self.0.borrowed(),
                permute((self.1.borrowed(), self.2.borrowed())),
            ))
            .map_yes(|(yes0, (yes1, yes2))| (yes0, yes1, yes2)),
            series((
                self.1.borrowed(),
                permute((self.0.borrowed(), self.2.borrowed())),
            ))
            .map_yes(|(yes1, (yes0, yes2))| (yes0, yes1, yes2)),
            series((
                self.2.borrowed(),
                permute((self.0.borrowed(), self.1.borrowed())),
            ))
            .map_yes(|(yes2, (yes0, yes1))| (yes0, yes1, yes2)),
        ))
        .parse_contextless(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        contextless::tag,
        util::{go_on, yes_and},
    };

    use super::*;
    #[test]
    fn contextless() {
        let parser = permute((tag("yes"), tag("no"), tag("maybe")));
        assert_eq!(
            parser.parse_contextless(""),
            Err(go_on(["yes", "no", "maybe"]).no_ctx()),
        );

        assert_eq!(
            parser.parse_contextless("yes"),
            Err(go_on(["no", "maybe"]).no_ctx()),
        );
        assert_eq!(
            parser.parse_contextless("no"),
            Err(go_on(["yes", "maybe"]).no_ctx()),
        );
        assert_eq!(
            parser.parse_contextless("maybe"),
            Err(go_on(["yes", "no"]).no_ctx()),
        );

        assert_eq!(
            parser.parse_contextless("yesno"),
            Err(go_on(["maybe"]).no_ctx()),
        );
        assert_eq!(
            parser.parse_contextless("yesmaybe"),
            Err(go_on(["no"]).no_ctx()),
        );

        assert_eq!(
            parser.parse_contextless("noyes"),
            Err(go_on(["maybe"]).no_ctx()),
        );
        assert_eq!(
            parser.parse_contextless("nomaybe"),
            Err(go_on(["yes"]).no_ctx()),
        );

        assert_eq!(
            parser.parse_contextless("maybeyes"),
            Err(go_on(["no"]).no_ctx()),
        );
        assert_eq!(
            parser.parse_contextless("maybeno"),
            Err(go_on(["yes"]).no_ctx()),
        );

        assert_eq!(
            parser.parse_contextless("yesnomaybe..."),
            Ok(yes_and(("yes", "no", "maybe"), "...").no_ctx())
        );
        assert_eq!(
            parser.parse_contextless("yesmaybeno..."),
            Ok(yes_and(("yes", "no", "maybe"), "...").no_ctx())
        );

        assert_eq!(
            parser.parse_contextless("noyesmaybe..."),
            Ok(yes_and(("yes", "no", "maybe"), "...").no_ctx())
        );
        assert_eq!(
            parser.parse_contextless("nomaybeyes..."),
            Ok(yes_and(("yes", "no", "maybe"), "...").no_ctx())
        );

        assert_eq!(
            parser.parse_contextless("maybeyesno..."),
            Ok(yes_and(("yes", "no", "maybe"), "...").no_ctx())
        );
        assert_eq!(
            parser.parse_contextless("maybenoyes..."),
            Ok(yes_and(("yes", "no", "maybe"), "...").no_ctx())
        );
    }
}
