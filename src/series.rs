use crate::{util::yes_and, UpParser, UpResult};

/// Accepts a tuple of parsers, and returns their results in a tuple.
/// ```
/// use parse_up::{series, tag, whitespace, util::{yes_and, go_on}};
///
/// let ctx = &mut ();
///
/// assert_eq!(
///     series((tag("hello"), whitespace, tag("world")))("hello world...", ctx),
///     Ok(yes_and(("hello".into(), " ".into(), "world".into()), "...")),
/// );
///
/// assert_eq!(
///     series((tag("hello"), whitespace, tag("world")))("hello", ctx),
///     Err(go_on([" "])),
/// );
/// ```
pub fn series<Context, ParserSequenceT>(
    parser_sequence: ParserSequenceT,
) -> impl for<'input> Fn(&'input str, &mut Context) -> UpResult<'input, ParserSequenceT::SequenceOut>
where
    ParserSequenceT: Series<Context>,
{
    move |input, context| parser_sequence.sequence(input, context)
}

pub trait Series<Context> {
    type SequenceOut;
    fn sequence<'input>(
        &self,
        input: &'input str,
        context: &mut Context,
    ) -> UpResult<'input, Self::SequenceOut>;
}

parse_up_proc_macros::_impl_series_for_tuples!();
