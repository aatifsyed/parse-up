use std::fmt::Display;

use itertools::Itertools as _;

use crate::{
    ContextlessUpParser, ContextlessUpResult, ContextualUpParser, UpError,
    UpResult, YesAnd,
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
    pub fn foo() {}
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
        could_also: Vec::new(),
        ctx: (),
    })
}

pub struct YesAndBuilder<'input, T>(YesAnd<'input, T, ()>);

impl<'input, T> YesAndBuilder<'input, T> {
    pub fn also<AlsoT>(mut self, also: impl IntoIterator<Item = AlsoT>) -> Self
    where
        AlsoT: Display,
    {
        self.0
            .could_also
            .extend(also.into_iter().map(|it| it.to_string()));
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

impl<'input, T, AnyCtx> PartialEq<YesAnd<'input, T, AnyCtx>>
    for YesAndBuilder<'input, T>
where
    T: PartialEq,
{
    fn eq(&self, other: &YesAnd<'input, T, AnyCtx>) -> bool {
        self.0.yes == other.yes
            && self.0.and == other.and
            && self.0.could_also == other.could_also
    }
}

impl<'input, T, AnyCtx> PartialEq<YesAndBuilder<'input, T>>
    for YesAnd<'input, T, AnyCtx>
where
    T: PartialEq,
{
    fn eq(&self, other: &YesAndBuilder<'input, T>) -> bool {
        self.yes == other.0.yes
            && self.and == other.0.and
            && self.could_also == other.0.could_also
    }
}

pub fn go_on<GoOnT: Display>(
    suggestions: impl IntoIterator<Item = GoOnT>,
) -> GoOnBuilder {
    GoOnBuilder(suggestions.into_iter().map(|g| g.to_string()).collect())
}

pub struct GoOnBuilder(Vec<String>);

impl GoOnBuilder {
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

impl<'any, AnyCtx> PartialEq<UpError<'any, AnyCtx>> for GoOnBuilder {
    fn eq(&self, other: &UpError<'any, AnyCtx>) -> bool {
        matches!(other, UpError::GoOn { go_on, .. } if go_on == &self.0)
    }
}
impl<'any, AnyCtx> PartialEq<GoOnBuilder> for UpError<'any, AnyCtx> {
    fn eq(&self, other: &GoOnBuilder) -> bool {
        matches!(self, UpError::GoOn { go_on, .. } if go_on == &other.0)
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

impl<'input, AnyCtx> PartialEq<UpError<'input, AnyCtx>>
    for OopsBuilder<'input>
{
    fn eq(&self, other: &UpError<'input, AnyCtx>) -> bool {
        matches!(other, UpError::Oops { input, message, .. } if input == &self.input && message == &self.message)
    }
}
impl<'input, AnyCtx> PartialEq<OopsBuilder<'input>>
    for UpError<'input, AnyCtx>
{
    fn eq(&self, other: &OopsBuilder<'input>) -> bool {
        matches!(self, UpError::Oops { input, message, .. } if input == &other.input && message == &other.message)
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
