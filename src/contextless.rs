use crate::{
    util::{assert_up_parser_fn, chars_needed_to_complete, go_on, oops, yes_and},
    UpParser, UpResult, YesAnd,
};

pub fn tag<'tag, 'input>(
    tag: &'tag str,
) -> impl FnMut(&'input str) -> UpResult<'input, &'input str> + 'tag + Copy {
    assert_up_parser_fn(move |input| match input.strip_prefix(tag) {
        Some(rest) => Ok(yes_and(&input[..tag.len()], rest)),
        None => match chars_needed_to_complete(tag, input) {
            Some("") => unreachable!("would've been caught in strip_prefix"),
            Some(suggestion) => Err(go_on([suggestion]).closed()),
            None => Err(oops(input, format!("expected {tag}"))),
        },
    })
}

pub fn whitespace(input: &str) -> UpResult<&str> {
    if input.is_empty() {
        return Err(go_on([" "]).open());
    }
    let trimmed = input.trim_start();
    let bytes_trimmed = input.len() - trimmed.len();
    match bytes_trimmed {
        0 => Err(oops(input, "expected whitespace")),
        _ => Ok(yes_and(&input[..bytes_trimmed], trimmed)),
    }
}

const _: () = {
    assert_up_parser_fn(whitespace);
};

pub fn recognize<'input, Out, Parser>(
    mut parser: Parser,
) -> impl FnMut(&'input str) -> UpResult<&str>
where
    Parser: UpParser<'input, Out>,
{
    assert_up_parser_fn(move |input| {
        let YesAnd {
            yes: _,
            and,
            suggestions,
        } = parser.parse_up(input)?;
        Ok(YesAnd {
            yes: input.strip_suffix(and).unwrap(),
            and,
            suggestions,
        })
    })
}
