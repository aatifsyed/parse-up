use crate::{
    util::{
        assert_contextless_parser_fn, chars_needed_to_complete, go_on, oops,
        yes_and,
    },
    ContextlessUpResult,
};

pub fn tag<'tag, 'input>(
    tag: &'tag str,
) -> impl Fn(&'input str) -> ContextlessUpResult<'input, &'input str> + 'tag + Copy
{
    move |input| match input.strip_prefix(tag) {
        Some(rest) => Ok(yes_and(&input[..tag.len()], rest).no_ctx()),
        None => match chars_needed_to_complete(tag, input) {
            Some("") => unreachable!("would've been caught in strip_prefix"),
            Some(suggestion) => Err(go_on([suggestion]).no_ctx()),
            None => Err(oops(input, format!("expected {tag}")).no_ctx()),
        },
    }
}
const _: () = {
    fn test() {
        assert_contextless_parser_fn(tag("hello"));
    }
};

pub fn whitespace(input: &str) -> ContextlessUpResult<&str> {
    if input.is_empty() {
        return Err(go_on([" "]).no_ctx());
    }
    let trimmed = input.trim_start();
    let bytes_trimmed = input.len() - trimmed.len();
    match bytes_trimmed {
        0 => Err(oops(input, "expected whitespace").no_ctx()),
        _ => Ok(yes_and(&input[..bytes_trimmed], trimmed).no_ctx()),
    }
}

const _: () = {
    fn test() {
        assert_contextless_parser_fn(whitespace);
    }
};
