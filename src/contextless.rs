use regex::Regex;

use crate::{
    one_of,
    util::{assert_contextless_parser_fn, chars_needed_to_complete, go_on, oops, yes_and},
    ContextlessUpParser, ContextlessUpParserExt, ContextlessUpResult,
};

pub fn tag<'tag, 'input>(
    tag: &'tag str,
) -> impl Fn(&'input str) -> ContextlessUpResult<'input, &'input str> + 'tag + Copy {
    move |input| match input.strip_prefix(tag) {
        Some(rest) => Ok(yes_and(&input[..tag.len()], rest).no_ctx()),
        None => match chars_needed_to_complete(tag, input) {
            Some("") => unreachable!("would've been caught in strip_prefix"),
            Some(suggestion) => Err(go_on([suggestion]).closed().no_ctx()),
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
        return Err(go_on([" "]).open().no_ctx());
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

pub fn bool(input: &str) -> ContextlessUpResult<bool> {
    one_of((
        tag("true").map_yes(|_| true),
        tag("false").map_yes(|_| false),
    ))
    .parse_contextless(input)
}

pub fn displayed<'input, T>(item: T) -> impl Fn(&'input str) -> ContextlessUpResult<'input, T>
where
    T: ToString + Clone,
{
    let display = item.to_string();
    move |input| {
        let item = item.clone();
        tag(&display)
            .map_yes(move |_| item.clone())
            .parse_contextless(input)
    }
}

/// # Panics
/// - If the regex is invalid
pub fn regex<'input>(
    re: &str,
) -> impl Fn(&'input str) -> ContextlessUpResult<'input, regex::Match<'input>> + Clone {
    let re = Regex::new(re).unwrap_or_else(|e| panic!("invalid regex {re}: {e}"));
    move |input| match re.find(input) {
        Some(m) => Ok(yes_and(m, &input[m.end()..]).no_ctx()),
        None => Err(oops(input, format!("regex {re} failed to match")).no_ctx()),
    }
}

pub fn take_until<'tag, 'input>(
    tag: &'tag str,
) -> impl Fn(&'input str) -> ContextlessUpResult<'input, &'input str> + 'tag + Copy {
    move |input| match input.find(tag) {
        Some(ix) => Ok(yes_and(&input[..ix], &input[ix..]).open([tag]).no_ctx()),
        None => Err(go_on([tag]).open().no_ctx()),
    }
}
