use crate::{
    util::{assert_up_parser_fn, oops},
    UpParser, UpResult,
};

pub fn one_of<'input, Out, Parsers>(
    mut parsers: Parsers,
) -> impl FnMut(&'input str) -> UpResult<'input, Out>
where
    Parsers: OneOf<'input, Out>,
{
    assert_up_parser_fn(move |input| parsers.one_of(input))
}

pub trait OneOf<'input, Out> {
    fn one_of(&mut self, input: &'input str) -> UpResult<'input, Out>;
}

impl<'input, Out, Parser0, Parser1> OneOf<'input, Out> for (Parser0, Parser1)
where
    Parser0: UpParser<'input, Out>,
    Parser1: UpParser<'input, Out>,
{
    fn one_of(&mut self, input: &'input str) -> UpResult<'input, Out> {
        let mut err = oops(input, "no branches could continue");
        match self.0.parse_up(input) {
            Ok(o) => return Ok(o),
            Err(e) => err = err + e,
        }
        match self.1.parse_up(input) {
            Ok(o) => return Ok(o),
            Err(e) => err = err + e,
        }
        Err(err)
    }
}
