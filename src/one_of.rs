use crate::{util::yes_and, UpParser, UpResult};

pub fn one_of<ParserSequenceT, Context>(
    parser_sequence: ParserSequenceT,
) -> impl for<'input> Fn(&'input str, &mut Context) -> UpResult<'input, ParserSequenceT::SequenceOut>
where
    ParserSequenceT: OneOf<Context>,
{
    move |input, context| parser_sequence.one_of(input, context)
}

pub trait OneOf<Context> {
    type SequenceOut;
    fn one_of<'input>(
        &self,
        input: &'input str,
        context: &mut Context,
    ) -> UpResult<'input, Self::SequenceOut>;
}

impl<Context, Parser0> OneOf<Context> for (Parser0,)
where
    Parser0: UpParser<Context>,
{
    type SequenceOut = (Parser0::Output,);

    fn one_of<'input>(
        &self,
        mut input: &'input str,
        context: &mut Context,
    ) -> UpResult<'input, Self::SequenceOut> {
        let parsed = ({
            let (rest, t) = self.0.parse(input, context)?.cont();
            input = rest;
            t
        },);
        Ok(yes_and(parsed, input))
    }
}

impl<Context, Parser0, Parser1> OneOf<Context> for (Parser0, Parser1)
where
    Parser0: UpParser<Context>,
    Parser1: UpParser<Context>,
{
    type SequenceOut = (Parser0::Output, Parser1::Output);

    fn one_of<'input>(
        &self,
        mut input: &'input str,
        context: &mut Context,
    ) -> UpResult<'input, Self::SequenceOut> {
        let parsed = (
            {
                let (rest, t) = self.0.parse(input, context)?.cont();
                input = rest;
                t
            },
            {
                let (rest, t) = self.1.parse(input, context)?.cont();
                input = rest;
                t
            },
        );
        Ok(yes_and(parsed, input))
    }
}
