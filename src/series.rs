use crate::{util::yes_and, UpParser, UpResult};

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

parse_up_proc_macros::impl_series_for_tuples!();

#[cfg(test)]
mod tests {
    use super::*;
    use crate::contextless::{tag, ContextlessUpParser as _};
    // #[test]
    // fn test() {
    //     series((
    //         tag("hello").ignoring_context(),
    //         tag("world").ignoring_context(),
    //     ))("helloworld", &mut ());
    // }
}
