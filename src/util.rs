use crate::{UpParser, UpResult};

pub fn assert_up_parser<'input, Out, Parser>(p: Parser) -> Parser
where
    Parser: UpParser<'input, Out>,
{
    p
}

pub fn assert_up_parser_fn<'input, Out, Parser>(p: Parser) -> Parser
where
    Parser: FnMut(&'input str) -> UpResult<'input, Out>,
{
    assert_up_parser(p)
}
