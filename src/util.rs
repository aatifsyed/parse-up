use std::fmt::Display;

use itertools::Itertools as _;

use crate::{UpError, UpResult, YesAnd};

pub fn yes_and<T>(yes: T, and: &str) -> UpResult<T> {
    Ok(YesAnd {
        yes,
        and,
        could_also: vec![],
    })
}

pub fn yes_and_also<T, AlsoT: Display>(
    yes: T,
    and: &str,
    also: impl IntoIterator<Item = AlsoT>,
) -> UpResult<T> {
    Ok(YesAnd {
        yes,
        and,
        could_also: also.into_iter().map(|a| a.to_string()).collect(),
    })
}

pub fn go_on<'any, AnyT, GoOnT: Display>(
    suggestions: impl IntoIterator<Item = GoOnT>,
) -> UpResult<'any, AnyT> {
    Err(UpError::GoOn {
        go_on: suggestions.into_iter().map(|g| g.to_string()).collect(),
    })
}

pub fn oops<AnyT, MessageT: Display>(input: &str, message: MessageT) -> UpResult<AnyT> {
    Err(UpError::Oops {
        input,
        message: message.to_string(),
    })
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

#[test]
fn test_chars_needed_to_complete() {
    assert_eq!(chars_needed_to_complete("tag", "ta"), Some("g"));
    assert_eq!(chars_needed_to_complete("foo", ""), Some("foo"));
    assert_eq!(chars_needed_to_complete("foo", "bar"), None);
    assert_eq!(chars_needed_to_complete("foo", "food"), None);
}
