use crate::{
    util::{go_on, oops, yes_and, yes_and_also},
    UpError, UpParser, UpResult, YesAnd,
};

/// Run a sequence of parsers (with the same output type).
/// Return the first that succeeds.
/// ```
/// use parse_up::{tag, one_of, util::{go_on, oops, yes_and, yes_and_also}};
///
/// let ctx = &mut ();
///
/// assert_eq!(
///     // all branches are searched
///     one_of((tag("hello"), tag("world")))("world...", ctx),
///     Ok(yes_and("world".into(), "...")),
/// );
///
/// assert_eq!(
///     // suggestions are integrated across branches
///     one_of((tag("foo"), tag("food")))("foo", ctx),
///     Ok(yes_and_also("foo".into(), "", ["d"])),
/// );
///
/// assert_eq!(
///     one_of((tag("hello"), tag("world")))("", ctx),
///     Err(go_on(["hello", "world"])),
/// );
///
/// assert_eq!(
///     one_of((tag("hello"), tag("helsinki")))("hel", ctx),
///     Err(go_on(["lo", "sinki"])),
/// );
///
/// assert_eq!(
///     // error if no branches match
///     one_of((tag("foo"), tag("bar")))("baz", ctx),
///     Err(oops("baz", "no branches matched")),
/// );
/// ```
pub fn one_of<ParserSequenceT, Context>(
    parser_sequence: ParserSequenceT,
) -> impl for<'input> Fn(&'input str, &mut Context) -> UpResult<'input, ParserSequenceT::Output>
where
    ParserSequenceT: OneOf<Context>,
{
    move |input, context| parser_sequence.one_of(input, context)
}

pub trait OneOf<Context> {
    type Output;
    fn one_of<'input>(
        &self,
        input: &'input str,
        context: &mut Context,
    ) -> UpResult<'input, Self::Output>;
}

parse_up_proc_macros::_impl_one_of_for_tuples!();
