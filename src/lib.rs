use std::fmt::Display;

use itertools::Itertools;

use util::{chars_needed_to_complete, go_on, oops, yes_and};

pub mod util;

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

impl<'input, T> YesAnd<'input, T> {
    /// Discard the suggestions, and extract the parse output and remainder of input
    pub fn cont(self) -> (T, &'input str) {
        (self.yes, self.and)
    }
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

pub trait UpParser<'input, T>: Clone {
    fn parse(&self, input: &'input str) -> UpResult<'input, T>;
}

impl<'input, T, F> UpParser<'input, T> for F
where
    F: Fn(&'input str) -> UpResult<'input, T> + Clone,
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
            None => oops(format!("expected {tag}, not {input}")),
        },
    }
}

/// ```
/// use parse_up::{UpParser as _, dictionary, util::{yes_and, go_on, oops}};
///
/// let parser = dictionary([
///     ("true", true),
///     ("false", false),
///     ("yes", true),
///     ("no", false),
/// ]);
///
/// assert_eq!(
///     parser.parse("true etc"),
///     yes_and(true, " etc"),
/// );
/// assert_eq!(
///     parser.parse(""),
///     go_on(["yes", "true", "no", "false"]),
/// );
/// assert_eq!(
///     parser.parse("y"),
///     go_on(["es"]),
/// );
/// assert_eq!(
///     parser.parse("yep"),
///     oops("expected one of [yes, true, no, false], not yep"),
/// );
/// ```
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
    move |input: &'input str| {
        let mut suggestions = vec![];
        for (k, v) in &pairs {
            match map(tag(k), |_| v.clone()).parse(input) {
                Ok(ok) => return Ok(ok),
                Err(UpError::Oops { .. }) => continue, // try another key
                Err(UpError::GoOn { go_on }) => suggestions.extend(go_on),
            }
        }
        match suggestions.is_empty() {
            true => oops(format!(
                "expected one of [{}], not {input}",
                pairs.iter().map(|it| &it.0).join(", ")
            )),
            false => go_on(suggestions),
        }
    }
}

/// ```
/// use parse_up::{tag, map, UpParser as _, util::yes_and};
///
/// assert_eq!(
///     map(tag("true"), |_| true).parse("true..."),
///     yes_and(true, "..."),
/// );
/// ```
pub fn map<'input, T, U>(
    parser: impl UpParser<'input, T>,
    f: impl Fn(T) -> U + Clone,
) -> impl UpParser<'input, U> {
    move |input: &'input str| {
        parser.parse(input).map(
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
/// use parse_up::{dictionary, followed_by, UpParser as _, util::yes_and};
///
/// let parser = dictionary([("true", true), ("false", false)]);
///
/// assert_eq!(
///     followed_by(parser.clone(), parser.clone()).parse("truefalse..."),
///     yes_and((true, false), "..."),
/// );
/// ```
pub fn followed_by<'input, L, R>(
    left: impl UpParser<'input, L>,
    right: impl UpParser<'input, R>,
) -> impl UpParser<'input, (L, R)> {
    move |input| {
        let (left, input) = left.parse(input)?.cont();
        right.parse(input).map(
            |YesAnd {
                 yes: right,
                 and,
                 could_also,
             }| YesAnd {
                yes: (left, right),
                and,
                could_also,
            },
        )
    }
}
