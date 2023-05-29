use std::fmt::Display;

use itertools::Itertools;

use util::{chars_needed_to_complete, go_on, oops, yes_and, yes_and_also};

pub mod reedline;
pub mod util;

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
    /// Discard the suggestions, and extract the parse output and remainder of input.
    /// Useful for chaining together parsers.
    pub fn cont(self) -> (T, &'input str) {
        (self.yes, self.and)
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

/// Takes the string `tag` from the input.
/// ```
/// use parse_up::{tag, util::{yes_and, go_on, oops}};
/// assert_eq!(
///     tag("hello")(""),
///     go_on(["hello"]),
/// );
/// assert_eq!(
///     tag("hello")("hell"),
///     go_on(["o"]),
/// );
/// assert_eq!(
///     tag("hello")("hello"),
///     yes_and("hello", ""),
/// );
/// assert_eq!(
///     tag("hello")("hello, world!"),
///     yes_and("hello", ", world!"),
/// );
/// assert_eq!(
///     tag("hello")("world"),
///     oops("world", "expected hello"),
/// );
/// ```
#[allow(clippy::needless_lifetimes)]
pub fn tag<'tag>(tag: &'tag str) -> impl Fn(&str) -> UpResult<&str> + Copy + 'tag {
    move |input: &str| match input.strip_prefix(tag) {
        Some(rest) => yes_and(&input[..tag.len()], rest),
        None => match chars_needed_to_complete(tag, input) {
            Some("") => unreachable!("would've been caught in prefix"),
            Some(suggestion) => go_on([suggestion]),
            None => oops(input, format!("expected {tag}")),
        },
    }
}

/// ```
/// use parse_up::{dictionary, util::{yes_and, go_on, oops}};
///
/// let parser = dictionary([
///     ("true", true),
///     ("false", false),
///     ("yes", true),
///     ("no", false),
/// ]);
///
/// assert_eq!(
///     parser("true etc"),
///     yes_and(true, " etc"),
/// );
/// assert_eq!(
///     parser(""),
///     go_on(["yes", "true", "no", "false"]),
/// );
/// assert_eq!(
///     parser("y"),
///     go_on(["es"]),
/// );
/// assert_eq!(
///     parser("yep"),
///     oops("yep", "expected one of [yes, true, no, false]"),
/// );
/// ```
pub fn dictionary<KeyT, ValueT>(
    items: impl IntoIterator<Item = (KeyT, ValueT)>,
) -> impl Fn(&str) -> UpResult<ValueT> + Clone
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
    move |input: &str| {
        let mut suggestions = vec![];
        for (k, v) in &pairs {
            match map(tag(k), |_| v.clone())(input) {
                Ok(ok) => return Ok(ok),
                Err(UpError::Oops { .. }) => continue, // try another key
                Err(UpError::GoOn { go_on }) => suggestions.extend(go_on),
            }
        }
        match suggestions.is_empty() {
            true => oops(
                input,
                format!(
                    "expected one of [{}]",
                    pairs.iter().map(|it| &it.0).join(", ")
                ),
            ),
            false => go_on(suggestions),
        }
    }
}

/// ```
/// use parse_up::{tag, map, util::yes_and};
///
/// assert_eq!(
///     map(tag("true"), |_| true)("true..."),
///     yes_and(true, "..."),
/// );
/// ```
pub fn map<'input, T, U>(
    parser: impl Fn(&'input str) -> UpResult<'input, T>,
    f: impl Fn(T) -> U,
) -> impl Fn(&'input str) -> UpResult<U> {
    move |input: &'input str| {
        parser(input).map(
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

/// ```
/// use parse_up::{dictionary, many1, util::{yes_and, go_on}};
///
/// let parser = dictionary([("true", true), ("false", false)]);
///
/// assert_eq!(
///     many1(parser.clone())("truefalse..."),
///     yes_and(vec![true, false], "..."),
/// );
/// assert_eq!(
///     many1(parser)("t"),
///     go_on(["rue"]),
/// );
/// ```
pub fn many1<T>(parser: impl Fn(&str) -> UpResult<T>) -> impl Fn(&str) -> UpResult<Vec<T>> {
    move |input: &str| {
        let YesAnd {
            yes,
            and,
            could_also,
        } = parser(input)?;
        let mut yeses = vec![yes];
        let mut input = and;
        while let Ok(YesAnd { yes, and, .. }) = parser(input) {
            input = and;
            yeses.push(yes);
        }
        yes_and_also(yeses, input, could_also)
    }
}

/// ```
/// use parse_up::{whitespace, util::{yes_and, go_on, oops}};
///
/// assert_eq!(
///     whitespace(" hello"),
///     yes_and(" ", "hello"),
/// );
/// assert_eq!(
///     whitespace("    hello"),
///     yes_and("    ", "hello"),
/// );
/// assert_eq!(
///     whitespace(""),
///     go_on([" "]),
/// );
/// assert_eq!(
///     whitespace("hello"),
///     oops("hello", "expected whitespace"),
/// );
/// ```
pub fn whitespace(input: &str) -> UpResult<&str> {
    if input.is_empty() {
        return go_on([" "]);
    }
    let trimmed = input.trim_start();
    let bytes_trimmed = input.len() - trimmed.len();
    match bytes_trimmed {
        0 => oops(input, "expected whitespace"),
        _ => yes_and(&input[..bytes_trimmed], trimmed),
    }
}

/// Expands to a block which runs a sequence of parsers, one after the other,
/// returning their results in a tuple.
/// ```
/// use parse_up::{tag, sequence, util::yes_and, UpResult};
///
/// let parser = |input| {
///     sequence!(input {
///         tag("foo"),
///         tag("bar"),
///     })
/// };
///
/// assert_eq!(
///     parser("foobar..."),
///     yes_and(("foo", "bar"), "..."),
/// );
/// ```
#[macro_export]
macro_rules! sequence {
    ($input:ident {
        $($parser:expr),* $(,)?
    }) => {{
        let mut input = $input;
        let parsed = (
            $({
                let (t, rest) = ($parser)(input)?.cont();
                input = rest;
                t
            },)*
        );
        $crate::util::yes_and(parsed, input)
    }};
}
