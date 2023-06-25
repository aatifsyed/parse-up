#[cfg(test)]
use crate::{
    tag,
    util::{go_on, yes_and},
};
use crate::{util::assert_up_parser_fn, UpParser, UpResult, YesAnd};

pub fn series<'input, Out, Parsers>(
    mut parsers: Parsers,
) -> impl FnMut(&'input str) -> UpResult<'input, Out>
where
    Parsers: Series<'input, Out>,
{
    assert_up_parser_fn(move |input| parsers.series(input))
}

pub trait Series<'input, Out> {
    fn series(&mut self, input: &'input str) -> UpResult<'input, Out>;
}

impl<'input, Out0, Out1, Parser0, Parser1> Series<'input, (Out0, Out1)> for (Parser0, Parser1)
where
    Parser0: UpParser<'input, Out0>,
    Parser1: UpParser<'input, Out1>,
{
    #[allow(unused_variables, unused_assignments)]
    fn series(&mut self, mut input: &'input str) -> UpResult<'input, (Out0, Out1)> {
        let mut final_suggestions = None;
        Ok(YesAnd {
            yes: (
                {
                    let YesAnd {
                        yes,
                        and,
                        suggestions,
                    } = self.0.parse_up(input)?;
                    final_suggestions = suggestions;
                    input = and;
                    yes
                },
                {
                    let YesAnd {
                        yes,
                        and,
                        suggestions,
                    } = self.1.parse_up(input)?;
                    final_suggestions = suggestions;
                    input = and;
                    yes
                },
            ),
            and: input,
            suggestions: final_suggestions,
        })
    }
}

parse_up_proc_macros::_impl_series_for_tuples!(1, 3..=16);

#[test]
fn test() {
    let mut parser = series((tag("hello"), tag("world")));
    assert_eq!(parser.parse_up(""), Err(go_on(["hello"]).closed()));
    assert_eq!(parser.parse_up("hello"), Err(go_on(["world"]).closed()));
    assert_eq!(
        parser.parse_up("helloworld..."),
        Ok(yes_and(("hello", "world"), "..."))
    );
}
