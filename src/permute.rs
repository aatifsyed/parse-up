#[cfg(test)]
use crate::{
    tag,
    util::{go_on, yes_and},
};
use crate::{
    util::{assert_up_parser_fn, oops},
    UpParser, UpResult, YesAnd,
};

pub fn permute<'input, Out, Parsers>(
    mut parsers: Parsers,
) -> impl FnMut(&'input str) -> UpResult<'input, Out>
where
    Parsers: Permute<'input, Out>,
{
    assert_up_parser_fn(move |input| parsers.permute(input))
}

pub trait Permute<'input, Out> {
    fn permute(&mut self, input: &'input str) -> UpResult<'input, Out>;
}

impl<'input, Out0, Out1, Parser0, Parser1> Permute<'input, (Out0, Out1)> for (Parser0, Parser1)
where
    Parser0: UpParser<'input, Out0>,
    Parser1: UpParser<'input, Out1>,
{
    #[allow(unused_variables, unused_assignments)]
    fn permute(&mut self, mut input: &'input str) -> UpResult<'input, (Out0, Out1)> {
        let mut yes0 = None;
        let mut yes1 = None;
        loop {
            let mut errs = oops(input, "no branches could continue");
            let mut current_suggestions = None;
            if let None = yes0 {
                match self.0.parse_up(input) {
                    Ok(YesAnd {
                        yes,
                        and,
                        suggestions,
                    }) => {
                        input = and;
                        yes0 = Some(yes);
                        current_suggestions = suggestions;
                        continue;
                    }
                    Err(e) => errs = errs + e,
                }
            }
            if let None = yes1 {
                match self.1.parse_up(input) {
                    Ok(YesAnd {
                        yes,
                        and,
                        suggestions,
                    }) => {
                        input = and;
                        yes1 = Some(yes);
                        current_suggestions = suggestions;
                        continue;
                    }
                    Err(e) => errs = errs + e,
                }
            }
            match (yes0, yes1) {
                (Some(yes0), Some(yes1)) => {
                    break Ok(YesAnd {
                        yes: (yes0, yes1),
                        and: input,
                        suggestions: current_suggestions,
                    })
                }
                _ => break Err(errs),
            }
        }
    }
}

parse_up_proc_macros::_impl_permute_for_tuples!(1, 3..=16);

#[test]
fn test() {
    let mut parser = permute((tag("hello"), tag("world")));
    assert_eq!(parser.parse_up(""), Err(go_on(["hello", "world"]).closed()));
    assert_eq!(parser.parse_up("hello"), Err(go_on(["world"]).closed()));
    assert_eq!(parser.parse_up("world"), Err(go_on(["hello"]).closed()));
    assert_eq!(
        parser.parse_up("helloworld..."),
        Ok(yes_and(("hello", "world"), "..."))
    );
    assert_eq!(
        parser.parse_up("worldhello..."),
        Ok(yes_and(("hello", "world"), "..."))
    );

    let mut parser = permute((tag("a"), tag("b"), tag("c")));
    assert_eq!(parser.parse_up(""), Err(go_on(["a", "b", "c"]).closed()));

    assert_eq!(parser.parse_up("a"), Err(go_on(["b", "c"]).closed()));
    assert_eq!(parser.parse_up("b"), Err(go_on(["a", "c"]).closed()));
    assert_eq!(parser.parse_up("c"), Err(go_on(["a", "b"]).closed()));

    assert_eq!(parser.parse_up("ab"), Err(go_on(["c"]).closed()));
    assert_eq!(parser.parse_up("ac"), Err(go_on(["b"]).closed()));

    assert_eq!(parser.parse_up("ba"), Err(go_on(["c"]).closed()));
    assert_eq!(parser.parse_up("bc"), Err(go_on(["a"]).closed()));

    assert_eq!(parser.parse_up("ca"), Err(go_on(["b"]).closed()));
    assert_eq!(parser.parse_up("cb"), Err(go_on(["a"]).closed()));

    assert_eq!(
        parser.parse_up("abc..."),
        Ok(yes_and(("a", "b", "c"), "..."))
    );
    assert_eq!(
        parser.parse_up("acb..."),
        Ok(yes_and(("a", "b", "c"), "..."))
    );

    assert_eq!(
        parser.parse_up("bac..."),
        Ok(yes_and(("a", "b", "c"), "..."))
    );
    assert_eq!(
        parser.parse_up("bca..."),
        Ok(yes_and(("a", "b", "c"), "..."))
    );

    assert_eq!(
        parser.parse_up("cab..."),
        Ok(yes_and(("a", "b", "c"), "..."))
    );
    assert_eq!(
        parser.parse_up("cba..."),
        Ok(yes_and(("a", "b", "c"), "..."))
    );
}
