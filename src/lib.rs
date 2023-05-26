use std::fmt::Display;

use itertools::Itertools;

use util::{chars_needed_to_complete, go_on, oops, yes_and};

pub mod util;

macro_rules! oops {
    ($tt:tt) => {
        Err(UpError::Oops {
            message: format!($tt),
        })
    };
}

pub type UpResult<'input, T> = Result<YesAnd<'input, T>, UpError>;

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

/// Parsing couldn't continue.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UpError {
    /// Parse error.
    Oops { message: String },
    /// Suggestions to append to the current input which would make parsing succeed.
    /// You should only return this when you're at the end of the input.
    GoOn { go_on: Vec<String> },
}

pub trait UpParser<'input, T> {
    fn parse(&self, input: &'input str) -> UpResult<'input, T>;
}

impl<'input, T, F> UpParser<'input, T> for F
where
    F: Fn(&'input str) -> UpResult<'input, T>,
{
    fn parse(&self, input: &'input str) -> UpResult<'input, T> {
        self(input)
    }
}

/// Takes the string `tag` from the input.
/// ```
/// use parse_up::{UpParser as _, tag, util::{yes_and, go_on, oops}};
/// assert_eq!(
///     tag("hello").parse(""),
///     go_on(["hello"]),
/// );
/// assert_eq!(
///     tag("hello").parse("hell"),
///     go_on(["o"]),
/// );
/// assert_eq!(
///     tag("hello").parse("hello"),
///     yes_and("hello", ""),
/// );
/// assert_eq!(
///     tag("hello").parse("hello, world!"),
///     yes_and("hello", ", world!"),
/// );
/// assert_eq!(
///     tag("hello").parse("world"),
///     oops("expected hello, not world"),
/// );
/// ```
pub fn tag<'input, 'tag>(tag: &'tag str) -> impl UpParser<'input, &'input str> + 'tag {
    move |input: &'input str| match input.strip_prefix(tag) {
        Some(rest) => yes_and(&input[..tag.len()], rest),
        None => match chars_needed_to_complete(tag, input) {
            Some("") => unreachable!("would've been caught in prefix"),
            Some(suggestion) => go_on([suggestion]),
            None => oops!("expected {tag}, not {input}"),
        },
    }
}

pub fn dictionary<'input, KeyT, ValueT>(
    items: impl IntoIterator<Item = (KeyT, ValueT)>,
) -> impl UpParser<'input, ValueT>
where
    KeyT: Display,
    ValueT: Clone,
{
    let pairs = items
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        // largest keys first
        .sorted_by_key(|(k, _v)| std::cmp::Reverse(k.clone()))
        .collect::<Vec<_>>();
    move |input: &str| todo!()
}

pub fn and_then<'input, L, R>(
    left: impl UpParser<'input, L>,
    right: impl UpParser<'input, R>,
) -> impl UpParser<'input, (L, R)> {
    move |input| todo!()
}
