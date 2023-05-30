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
///     Err(go_on(["hello"])),
/// );
/// assert_eq!(
///     tag("hello")("hell"),
///     Err(go_on(["o"])),
/// );
/// assert_eq!(
///     tag("hello")("hello"),
///     Ok(yes_and("hello", "")),
/// );
/// assert_eq!(
///     tag("hello")("hello, world!"),
///     Ok(yes_and("hello", ", world!")),
/// );
/// assert_eq!(
///     tag("hello")("world"),
///     Err(oops("world", "expected hello")),
/// );
/// ```
#[allow(clippy::needless_lifetimes)]
pub fn tag<'tag>(tag: &'tag str) -> impl Fn(&str) -> UpResult<&str> + Copy + 'tag {
    move |input: &str| match input.strip_prefix(tag) {
        Some(rest) => Ok(yes_and(&input[..tag.len()], rest)),
        None => match chars_needed_to_complete(tag, input) {
            Some("") => unreachable!("would've been caught in prefix"),
            Some(suggestion) => Err(go_on([suggestion])),
            None => Err(oops(input, format!("expected {tag}"))),
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
///     Ok(yes_and(true, " etc")),
/// );
/// assert_eq!(
///     parser(""),
///     Err(go_on(["yes", "true", "no", "false"])),
/// );
/// assert_eq!(
///     parser("y"),
///     Err(go_on(["es"])),
/// );
/// assert_eq!(
///     parser("yep"),
///     Err(oops("yep", "expected one of [yes, true, no, false]")),
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
            true => Err(oops(
                input,
                format!(
                    "expected one of [{}]",
                    pairs.iter().map(|it| &it.0).join(", ")
                ),
            )),
            false => Err(go_on(suggestions)),
        }
    }
}

/// ```
/// use parse_up::{tag, map, util::yes_and};
///
/// assert_eq!(
///     map(tag("true"), |_| true)("true..."),
///     Ok(yes_and(true, "...")),
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
///     Ok(yes_and(vec![true, false], "...")),
/// );
/// assert_eq!(
///     many1(parser)("t"),
///     Err(go_on(["rue"])),
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
        Ok(yes_and_also(yeses, input, could_also))
    }
}

/// ```
/// use parse_up::{whitespace, util::{yes_and, go_on, oops}};
///
/// assert_eq!(
///     whitespace(" hello"),
///     Ok(yes_and(" ", "hello")),
/// );
/// assert_eq!(
///     whitespace("    hello"),
///     Ok(yes_and("    ", "hello")),
/// );
/// assert_eq!(
///     whitespace(""),
///     Err(go_on([" "])),
/// );
/// assert_eq!(
///     whitespace("hello"),
///     Err(oops("hello", "expected whitespace")),
/// );
/// ```
pub fn whitespace(input: &str) -> UpResult<&str> {
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

/// Returns [`None`] if the inner parser fails, feeding it empty input to get suggestions as required.
/// ```
/// use parse_up::{opt, util::{yes_and, yes_and_also}, dictionary};
///
/// let parser = opt(dictionary([("true", true), ("false", false)]));
///
/// assert_eq!(
///     parser("true..."),
///     Ok(yes_and(Some(true), "...")),
/// );
///
/// assert_eq!(
///     parser("..."),
///     Ok(yes_and_also(None, "...", ["true", "false"])),
/// );
///
/// ```
pub fn opt<T>(parser: impl Fn(&str) -> UpResult<T>) -> impl Fn(&str) -> UpResult<Option<T>> {
    move |input| match parser(input) {
        Ok(YesAnd {
            yes,
            and,
            could_also,
        }) => Ok(YesAnd {
            yes: Some(yes),
            and,
            could_also,
        }),
        Err(UpError::GoOn { go_on }) if !go_on.is_empty() => Ok(yes_and_also(None, input, go_on)),
        Err(_) => {
            let suggestions = if let Err(UpError::GoOn { go_on }) = parser("") {
                go_on
            } else {
                todo!("is this a user bug?")
            };
            Ok(yes_and_also(None, input, suggestions))
        }
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
///     Ok(yes_and(("foo", "bar"), "...")),
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
        $crate::UpResult::Ok($crate::util::yes_and(parsed, input))
    }};
}
