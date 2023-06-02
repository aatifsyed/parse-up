use crate::{
    ContextlessUpParser, ContextualUpParser, UpError, UpResult, YesAnd,
};

pub struct IgnoreContext<T>(T);

pub trait ContextlessUpParserExt<'input, Out>:
    ContextlessUpParser<'input, Out>
{
    fn ignore_context(self) -> IgnoreContext<Self>
    where
        Self: Sized,
    {
        IgnoreContext(self)
    }
}

impl<'input, Out, Ctx, T> ContextualUpParser<'input, Out, Ctx>
    for IgnoreContext<T>
where
    T: ContextlessUpParser<'input, Out>,
{
    fn parse_contextual(
        &self,
        input: &'input str,
        ctx: Ctx,
    ) -> crate::UpResult<'input, Out, Ctx> {
        self.0.parse_contextless(input).map_ctx(|_| ctx)
    }
}

pub trait UpResultExt<'input, Out, Ctx> {
    fn map_ctx<NewCtx>(
        self,
        f: impl FnOnce(Ctx) -> NewCtx,
    ) -> UpResult<'input, Out, NewCtx>;
}

impl<'input, Out, Ctx> UpResultExt<'input, Out, Ctx>
    for UpResult<'input, Out, Ctx>
{
    fn map_ctx<NewCtx>(
        self,
        f: impl FnOnce(Ctx) -> NewCtx,
    ) -> UpResult<'input, Out, NewCtx> {
        match self {
            Ok(YesAnd {
                yes,
                and,
                could_also,
                ctx,
            }) => Ok(YesAnd {
                yes,
                and,
                could_also,
                ctx: f(ctx),
            }),
            Err(UpError::GoOn { go_on, ctx }) => {
                Err(UpError::GoOn { go_on, ctx: f(ctx) })
            }
            Err(UpError::Oops {
                input,
                message,
                ctx,
            }) => Err(UpError::Oops {
                input,
                message,
                ctx: f(ctx),
            }),
        }
    }
}
