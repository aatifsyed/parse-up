#![allow(unused)]
use std::{fmt::Display, process::Output};

use itertools::Itertools;

use util::{chars_needed_to_complete, go_on, oops, yes_and, yes_and_also};

mod contextless;
mod ext;
mod frunk;
mod one_of;
pub mod reedline;
mod series;
pub mod util;
// pub use contextless::{dictionary, many1, opt, tag, whitespace};
pub use one_of::one_of;
pub use series::series;

pub type UpResult<'input, T> = Result<YesAnd<'input, T>, UpError<'input>>;

/// Successful parse so far.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct YesAnd<'input, T> {
    /// The value.
    pub yes: T,
    /// The remaining input.
    pub and: &'input str,
    /// Other valid options.
    pub could_also: Vec<String>,
}

impl<'input, T> YesAnd<'input, T> {
    /// Discard the suggestions, and extract the remainder of input, and the parsed output.
    /// Useful for chaining together parsers.
    pub fn cont(self) -> (&'input str, T) {
        (self.and, self.yes)
    }
}

/// Parsing couldn't continue.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UpError<'input> {
    /// Parse error.
    Oops { input: &'input str, message: String },
    /// Suggestions to append to the current input which would make parsing succeed.
    /// You should only return this when you're at the end of the input.
    GoOn { go_on: Vec<String> },
}

pub trait ContextualUpParser<'input, Context, Output> {
    fn parse_contextual(&self, input: &'input str, ctx: &mut Context) -> UpResult<'input, Output>;
}

impl<'input, Context, Output, ParseFn> ContextualUpParser<'input, Context, Output> for ParseFn
where
    ParseFn: Fn(&'input str, &mut Context) -> UpResult<'input, Output>,
{
    fn parse_contextual(&self, input: &'input str, ctx: &mut Context) -> UpResult<'input, Output> {
        self(input, ctx)
    }
}

pub trait ContextlessUpParser<'input, Output> {
    fn parse_contextless(&self, input: &'input str) -> UpResult<'input, Output>;
}

impl<'input, Output, ParseFn> ContextlessUpParser<'input, Output> for ParseFn
where
    ParseFn: Fn(&'input str) -> UpResult<'input, Output>,
{
    fn parse_contextless(&self, input: &'input str) -> UpResult<'input, Output> {
        self(input)
    }
}

pub trait ContextlessUpParserExt<'input, Output>: ContextlessUpParser<'input, Output> {
    fn ignore_context(self) -> IgnoreContext<Self>
    where
        Self: Sized,
    {
        IgnoreContext(self)
    }
}

impl<'input, Output, Contextless> ContextlessUpParserExt<'input, Output> for Contextless where
    Contextless: ContextlessUpParser<'input, Output>
{
}

pub struct IgnoreContext<Contextless>(pub Contextless);

impl<'input, Context, Output, Contextless> ContextualUpParser<'input, Context, Output>
    for IgnoreContext<Contextless>
where
    Contextless: ContextlessUpParser<'input, Output>,
{
    fn parse_contextual(&self, input: &'input str, _: &mut Context) -> UpResult<'input, Output> {
        self.0.parse_contextless(input)
    }
}

pub trait UpResultExt<'input, T> {
    fn into_up_result(self) -> UpResult<'input, T>;
    fn map_yes<U>(self, f: impl FnOnce(T) -> U) -> UpResult<'input, U>
    where
        Self: Sized,
    {
        self.into_up_result().map(
            |YesAnd {
                 yes,
                 and,
                 could_also,
             }| YesAnd {
                yes: f(yes),
                and,
                could_also,
            },
        )
    }
}

impl<'input, T> UpResultExt<'input, T> for UpResult<'input, T> {
    fn into_up_result(self) -> UpResult<'input, T> {
        self
    }
}

pub fn tag_ctx<'tag, 'input, Context>(
    tag: &'tag str,
) -> impl Fn(&'input str, &mut Context) -> UpResult<'input, &'input str> + 'tag {
    move |input, _| match input.strip_prefix(tag) {
        Some(rest) => Ok(yes_and(&input[..tag.len()], rest)),
        None => match chars_needed_to_complete(tag, input) {
            Some("") => unreachable!("would've been caught in prefix"),
            Some(suggestion) => Err(go_on([suggestion])),
            None => Err(oops(input, format!("expected {tag}"))),
        },
    }
}

pub fn tag<'tag, 'input>(
    tag: &'tag str,
) -> impl Fn(&'input str) -> UpResult<'input, &'input str> + 'tag {
    move |input| match input.strip_prefix(tag) {
        Some(rest) => Ok(yes_and(&input[..tag.len()], rest)),
        None => match chars_needed_to_complete(tag, input) {
            Some("") => unreachable!("would've been caught in prefix"),
            Some(suggestion) => Err(go_on([suggestion])),
            None => Err(oops(input, format!("expected {tag}"))),
        },
    }
}

pub fn whitespace(input: &str) -> UpResult<&'_ str> {
    if input.is_empty() {
        return Err(go_on([" "]));
    }
    let trimmed = input.trim_start();
    let bytes_trimmed = input.len() - trimmed.len();
    match bytes_trimmed {
        0 => Err(oops(input, "expected whitespace")),
        _ => Ok(yes_and(&input[..bytes_trimmed], trimmed)),
    }
}

const _: () = {
    fn assert_impl_contextual_up_parser<'input, Context, Output>(
        _: impl ContextualUpParser<'input, Context, Output>,
    ) {
    }
    fn assert_impl_contexual_parser_fn<'input, Context, Output>(
        f: impl Fn(&'input str, &mut Context) -> UpResult<'input, Output>,
    ) {
        assert_impl_contextual_up_parser(f)
    }
    fn assert_impl_contextless_up_parser<'input, Output>(
        _: impl ContextlessUpParser<'input, Output>,
    ) {
    }
    fn assert_impl_contextless_parser_fn<'input, Output>(
        f: impl Fn(&'input str) -> UpResult<'input, Output>,
    ) {
        assert_impl_contextless_up_parser(f)
    }
    fn test() {
        assert_impl_contextual_up_parser::<(), _>(tag_ctx("hello"));
        assert_impl_contexual_parser_fn::<(), _>(tag_ctx("hello"));
        assert_impl_contextless_parser_fn(tag("hello"));
        assert_impl_contextless_up_parser(tag("hello"));
        assert_impl_contextual_up_parser::<(), _>(tag("hello").ignore_context());
        assert_impl_contextless_parser_fn(whitespace);
        whitespace.ignore_context();
    }
};
