use std::fmt::Display;

use crate::{Closed, Open, Suggestions, UpError, UpParser, UpResult, YesAnd};

pub const fn assert_up_parser<'input, Out, Parser>(p: Parser) -> Parser
where
    Parser: UpParser<'input, Out>,
{
    p
}

pub const fn assert_up_parser_fn<'input, Out, Parser>(p: Parser) -> Parser
where
    Parser: FnMut(&'input str) -> UpResult<'input, Out>,
{
    assert_up_parser(p)
}

pub fn yes_and<Out>(yes: Out, and: &str) -> YesAnd<Out> {
    YesAnd {
        yes,
        and,
        suggestions: None,
    }
}

pub(crate) const EMPTY_CLOSED_SUGGESTION_PANIC_MSG: &str =
    "closed suggestions must contain at least one suggestion. \
return an open suggestion or an error instead.";

impl<Out> YesAnd<'_, Out> {
    pub fn completely_open(self) -> Self {
        self.open(Vec::<&str>::new())
    }
    pub fn open<SuggestionT>(mut self, suggestions: impl IntoIterator<Item = SuggestionT>) -> Self
    where
        SuggestionT: Display,
    {
        let new_suggestions = suggestions.into_iter().map(|it| it.to_string());
        let suggestions = match self.suggestions.take() {
            Some(already) => already.into_iter().chain(new_suggestions).collect(),
            None => new_suggestions.collect(),
        };
        self.suggestions = Some(Open(suggestions));
        self
    }
    /// # Panics
    /// If no suggestions are given
    pub fn closed<SuggestionT>(mut self, suggestions: impl IntoIterator<Item = SuggestionT>) -> Self
    where
        SuggestionT: Display,
    {
        let new_suggestions = suggestions.into_iter().map(|it| it.to_string());
        let mut suggestions = match self.suggestions.take() {
            Some(already) => already
                .into_iter()
                .chain(new_suggestions)
                .collect::<Vec<_>>(),
            None => new_suggestions.collect(),
        };
        if suggestions.is_empty() {
            panic!("{}", EMPTY_CLOSED_SUGGESTION_PANIC_MSG)
        }
        let first = suggestions.remove(0);
        self.suggestions = Some(Closed(first, suggestions));
        self
    }
}

pub struct GoOnBuilder(Vec<String>);

pub fn go_on<SuggestionT: Display>(
    suggestions: impl IntoIterator<Item = SuggestionT>,
) -> GoOnBuilder {
    GoOnBuilder(suggestions.into_iter().map(|g| g.to_string()).collect())
}

impl GoOnBuilder {
    pub fn or<GoOnT: Display>(
        mut self,
        other_suggestions: impl IntoIterator<Item = GoOnT>,
    ) -> GoOnBuilder {
        self.0
            .extend(other_suggestions.into_iter().map(|it| it.to_string()));
        self
    }
    pub fn open<'any>(self) -> UpError<'any> {
        UpError::GoOn(Suggestions::Open(self.0))
    }
    /// # Panics
    /// If no suggestions are given
    pub fn closed<'any>(mut self) -> UpError<'any> {
        if self.0.is_empty() {
            panic!("{}", EMPTY_CLOSED_SUGGESTION_PANIC_MSG)
        };
        let first = self.0.remove(0);
        UpError::GoOn(Closed(first, self.0))
    }
}

pub fn oops(input: &str, message: impl Display) -> UpError {
    UpError::Oops {
        input,
        message: message.to_string(),
    }
}

/// ```
/// use parse_up::util::strip_matching_prefixes;
/// assert_eq!(
///     strip_matching_prefixes("hel", ["hello", "helsinki"]),
///     ["lo", "sinki"],
/// );
/// assert_eq!(
///     strip_matching_prefixes("bar", ["bartender", "bar"]),
///     ["tender"],
/// );
/// ```
pub fn strip_matching_prefixes<'haystack>(
    needle: &str,
    haystack: impl IntoIterator<Item = &'haystack str>,
) -> Vec<String> {
    haystack
        .into_iter()
        .filter_map(|hay| {
            hay.strip_prefix(needle).and_then(|s| match s {
                "" => None,
                _ => Some(s),
            })
        })
        .map(String::from)
        .collect()
}

/// What characters would need to be added to `haystack` to get to `needle`?
/// returns [None] if wouldn't be possible, and [Some("")] if they already match.
/// ```
/// use parse_up::util::chars_needed_to_complete;
/// assert_eq!(chars_needed_to_complete("tag", "ta" ), Some("g"));
/// assert_eq!(chars_needed_to_complete("tag", ""   ), Some("tag"));
/// assert_eq!(chars_needed_to_complete("tag", "tag"), Some("")); // Special case
/// assert_eq!(chars_needed_to_complete("tag", "tar"), None);
/// assert_eq!(chars_needed_to_complete(""   , "foo"), None);
/// ```
pub fn chars_needed_to_complete<'needle>(
    needle: &'needle str,
    haystack: &str,
) -> Option<&'needle str> {
    use itertools::{
        EitherOrBoth::{Both, Left, Right},
        Itertools as _,
    };
    for chars in needle.char_indices().zip_longest(haystack.chars()) {
        match chars {
            Both((_, l), r) if l == r => continue, // matched
            Both(..) => return None,               // diverged
            Left((ix, _)) => return Some(&needle[ix..]),
            Right(_) => return None,
        }
    }
    Some("") // all chars have matched
}
