use std::fmt::Display;

use itertools::Itertools as _;

use crate::{
    ContextlessUpParser, ContextlessUpResult, ContextualUpParser,
    Suggestions::{self, Closed, Open},
    UpError, UpResult, YesAnd,
};

pub mod assert {
    use crate::{ContextlessUpParser, ContextlessUpResult};

    pub fn contextless_up_parser<'input, Out, T>(t: T) -> T
    where
        T: ContextlessUpParser<'input, Out>,
    {
        t
    }

    pub fn contextless_parser_fn<'input, Out, F>(f: F) -> F
    where
        F: Fn(&'input str) -> ContextlessUpResult<'input, Out>,
    {
        contextless_up_parser(f)
    }
}
pub fn assert_contextless_up_parser<'input, Out>(
    _: impl ContextlessUpParser<'input, Out>,
) {
}
pub fn assert_contextless_parser_fn<'input, Out>(
    f: impl Fn(&'input str) -> ContextlessUpResult<'input, Out>,
) {
    assert_contextless_up_parser(f)
}
pub fn assert_contextual_up_parser<'input, Out, Ctx>(
    _: impl ContextualUpParser<'input, Out, Ctx>,
) {
}
pub fn assert_contextual_parser_fn<'input, Out, Ctx>(
    f: impl Fn(&'input str, Ctx) -> UpResult<'input, Out, Ctx>,
) {
    assert_contextual_up_parser(f)
}

pub fn yes_and<T>(yes: T, and: &str) -> YesAndBuilder<T> {
    YesAndBuilder(YesAnd {
        yes,
        and,
        could_also: None,
        ctx: (),
    })
}

pub struct YesAndBuilder<'input, T>(YesAnd<'input, T, ()>);

pub(crate) const EMPTY_CLOSED_SUGGESTION_PANIC_MSG: &str =
    "closed suggestions must contain at least one suggestion. \
return an open suggestion or an error instead.";

impl<'input, T> YesAndBuilder<'input, T> {
    pub fn open<SuggestionT>(
        mut self,
        suggestions: impl IntoIterator<Item = SuggestionT>,
    ) -> Self
    where
        SuggestionT: Display,
    {
        self.0.could_also = Some(Open(
            suggestions.into_iter().map(|it| it.to_string()).collect(),
        ));
        self
    }
    /// # Panics
    /// If no suggestions are given
    pub fn closed<SuggestionT>(
        mut self,
        suggestions: impl IntoIterator<Item = SuggestionT>,
    ) -> Self
    where
        SuggestionT: Display,
    {
        let mut suggestions = suggestions.into_iter().map(|it| it.to_string());
        let first =
            suggestions.next().expect(EMPTY_CLOSED_SUGGESTION_PANIC_MSG);
        self.0.could_also = Some(Closed(first, suggestions.collect()));
        self
    }
    pub fn no_ctx(self) -> YesAnd<'input, T, ()> {
        self.0
    }
    pub fn ctx<Ctx>(self, ctx: Ctx) -> YesAnd<'input, T, Ctx> {
        let YesAnd {
            yes,
            and,
            could_also,
            ..
        } = self.0;
        YesAnd {
            yes,
            and,
            could_also,
            ctx,
        }
    }
}

pub fn go_on<GoOnT: Display>(
    suggestions: impl IntoIterator<Item = GoOnT>,
) -> GoOnBuilder<Vec<String>> {
    GoOnBuilder(suggestions.into_iter().map(|g| g.to_string()).collect())
}

pub struct GoOnBuilder<T>(T);

impl GoOnBuilder<Vec<String>> {
    pub fn open(self) -> GoOnBuilder<Suggestions> {
        GoOnBuilder(Open(self.0))
    }
    /// # Panics
    /// If no suggestions are given
    pub fn closed(mut self) -> GoOnBuilder<Suggestions> {
        if self.0.is_empty() {
            panic!("{}", EMPTY_CLOSED_SUGGESTION_PANIC_MSG)
        };
        let first = self.0.remove(0);
        GoOnBuilder(Closed(first, self.0))
    }
}

impl GoOnBuilder<Suggestions> {
    pub fn no_ctx<'any>(self) -> UpError<'any, ()> {
        UpError::GoOn {
            go_on: self.0,
            ctx: (),
        }
    }
    pub fn ctx<'any, Ctx>(self, ctx: Ctx) -> UpError<'any, Ctx> {
        UpError::GoOn { go_on: self.0, ctx }
    }
}

pub fn oops(input: &str, message: impl Display) -> OopsBuilder {
    OopsBuilder {
        input,
        message: message.to_string(),
    }
}

pub struct OopsBuilder<'input> {
    input: &'input str,
    message: String,
}

impl<'input> OopsBuilder<'input> {
    pub fn no_ctx(self) -> UpError<'input, ()> {
        let Self { input, message } = self;
        UpError::Oops {
            input,
            message,
            ctx: (),
        }
    }
    pub fn ctx<Ctx>(self, ctx: Ctx) -> UpError<'input, Ctx> {
        let Self { input, message } = self;
        UpError::Oops {
            input,
            message,
            ctx,
        }
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
/// returns [None] if wouldn't be possible.
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
    use itertools::EitherOrBoth::{Both, Left, Right};
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
