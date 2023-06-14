use crate::{
    ContextlessUpParser, ContextlessUpResult, ContextualUpParser, UpError, UpResult, YesAnd,
};

pub fn many1<Parser>(parser: Parser) -> Many1<Parser> {
    Many1(parser)
}

pub struct Many1<T>(T);

impl<'input, Out, Parser> ContextlessUpParser<'input, Vec<Out>> for Many1<Parser>
where
    Parser: ContextlessUpParser<'input, Out>,
{
    fn parse_contextless(&self, mut input: &'input str) -> ContextlessUpResult<'input, Vec<Out>> {
        let (and, yes0, _) = self.0.parse_contextless(input)?.cont();
        input = and;
        let mut yeses = vec![yes0];
        let mut final_could_also = None;
        loop {
            match self.0.parse_contextless(input) {
                Ok(YesAnd {
                    yes,
                    and,
                    could_also,
                    ctx: _,
                }) => {
                    yeses.push(yes);
                    input = and;
                    final_could_also = could_also;
                    continue; // try and parse another
                }
                Err(UpError::GoOn { go_on, ctx: _ }) => {
                    break Ok(YesAnd {
                        yes: yeses,
                        and: input,
                        could_also: Some(go_on),
                        ctx: (),
                    })
                }
                Err(UpError::Oops {
                    input: _,
                    message: _,
                    ctx: _,
                }) => {
                    break Ok(YesAnd {
                        yes: yeses,
                        and: input,
                        could_also: final_could_also,
                        ctx: (),
                    })
                }
            }
        }
    }
}

impl<'input, Out, Ctx, Parser> ContextualUpParser<'input, Vec<Out>, Ctx> for Many1<Parser>
where
    Parser: ContextualUpParser<'input, Out, Ctx>,
{
    fn parse_contextual(&self, input: &'input str, ctx: Ctx) -> UpResult<'input, Vec<Out>, Ctx> {
        todo!()
    }
}

pub fn many0<Parser>(parser: Parser) -> Many0<Parser> {
    Many0(parser)
}

pub struct Many0<T>(T);

impl<'input, Out, Parser> ContextlessUpParser<'input, Vec<Out>> for Many0<Parser>
where
    Parser: ContextlessUpParser<'input, Out>,
{
    fn parse_contextless(&self, mut input: &'input str) -> ContextlessUpResult<'input, Vec<Out>> {
        let mut yeses = vec![];
        let mut final_could_also = None;
        loop {
            match self.0.parse_contextless(input) {
                Ok(YesAnd {
                    yes,
                    and,
                    could_also,
                    ctx: _,
                }) => {
                    yeses.push(yes);
                    input = and;
                    final_could_also = could_also;
                    continue; // try and parse another
                }
                Err(UpError::GoOn { go_on, ctx: _ }) => {
                    break Ok(YesAnd {
                        yes: yeses,
                        and: input,
                        could_also: Some(go_on),
                        ctx: (),
                    })
                }
                Err(UpError::Oops {
                    input: _,
                    message: _,
                    ctx: _,
                }) => {
                    break Ok(YesAnd {
                        yes: yeses,
                        and: input,
                        could_also: final_could_also,
                        ctx: (),
                    })
                }
            }
        }
    }
}

impl<'input, Out, Ctx, Parser> ContextualUpParser<'input, Vec<Out>, Ctx> for Many0<Parser>
where
    Parser: ContextualUpParser<'input, Out, Ctx>,
{
    fn parse_contextual(&self, input: &'input str, ctx: Ctx) -> UpResult<'input, Vec<Out>, Ctx> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        one_of, tag,
        util::{go_on, yes_and},
        ContextlessUpParserExt,
    };

    use super::*;
    #[test]
    fn test() {
        let parser = many1(one_of((
            tag("true").map_yes(|_| true),
            tag("false").map_yes(|_| false),
        )));
        assert_eq!(
            parser.parse_contextless(""),
            Err(go_on(["true", "false"]).closed().no_ctx())
        );
        assert_eq!(
            parser.parse_contextless("t"),
            Err(go_on(["rue"]).closed().no_ctx())
        );
        assert_eq!(
            parser.parse_contextless("true"),
            Ok(yes_and(vec![true], "").closed(["true", "false"]).no_ctx())
        );
        assert_eq!(
            parser.parse_contextless("truet"),
            Ok(yes_and(vec![true], "t").closed(["rue"]).no_ctx())
        );
    }
}
