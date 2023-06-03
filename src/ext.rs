use std::marker::PhantomData;

use crate::{
    ContextlessUpParser, ContextlessUpResult, ContextualUpParser, UpError,
    UpResult, YesAnd,
};

pub struct IgnoreContext<T>(T);
pub struct MapYes<Parser, MapFn, Out>(Parser, MapFn, PhantomData<Out>);

// Can't impl<T: Parser> Parser for &T because it will conflict with the implementation for Fn
// So make our own wrapper.
#[derive(Clone, Copy)]
pub struct BorrowedParser<'a, T>(&'a T);

pub trait ContextlessUpParserExt<'input, Out>:
    ContextlessUpParser<'input, Out>
{
    fn ignore_context(self) -> IgnoreContext<Self>
    where
        Self: Sized,
    {
        IgnoreContext(self)
    }
    fn map_yes<Out2, MapFn>(self, f: MapFn) -> MapYes<Self, MapFn, Out>
    where
        Self: Sized,
        MapFn: Fn(Out) -> Out2,
    {
        MapYes(self, f, PhantomData)
    }
    fn borrowed(&self) -> BorrowedParser<Self>
    where
        Self: Sized,
    {
        BorrowedParser(self)
    }
}

impl<'input, Out, Out2, Parser, MapFn> ContextlessUpParser<'input, Out2>
    for MapYes<Parser, MapFn, Out>
where
    Parser: ContextlessUpParser<'input, Out>,
    MapFn: Fn(Out) -> Out2,
{
    fn parse_contextless(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out2> {
        Ok(self.0.parse_contextless(input)?.map_yes(&self.1))
    }
}

impl<'borrowed, 'input, Out, Parser> ContextlessUpParser<'input, Out>
    for BorrowedParser<'borrowed, Parser>
where
    Parser: ContextlessUpParser<'input, Out>,
{
    fn parse_contextless(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out> {
        self.0.parse_contextless(input)
    }
}

impl<'input, Out, T> ContextlessUpParserExt<'input, Out> for T where
    T: ContextlessUpParser<'input, Out>
{
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
    fn map_yes<Out2>(
        self,
        f: impl FnOnce(Out) -> Out2,
    ) -> UpResult<'input, Out2, Ctx>;
    fn map_ctx<Ctx2>(
        self,
        f: impl FnOnce(Ctx) -> Ctx2,
    ) -> UpResult<'input, Out, Ctx2>;
}

impl<'input, Out, Ctx> UpResultExt<'input, Out, Ctx>
    for UpResult<'input, Out, Ctx>
{
    fn map_yes<Out2>(
        self,
        f: impl FnOnce(Out) -> Out2,
    ) -> UpResult<'input, Out2, Ctx> {
        match self {
            Ok(YesAnd {
                yes,
                and,
                could_also,
                ctx,
            }) => Ok(YesAnd {
                yes: f(yes),
                and,
                could_also,
                ctx,
            }),
            Err(e) => Err(e),
        }
    }
    fn map_ctx<Ctx2>(
        self,
        f: impl FnOnce(Ctx) -> Ctx2,
    ) -> UpResult<'input, Out, Ctx2> {
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

#[cfg(test)]
mod tests {
    use crate::{contextless::tag, one_of::one_of, util::yes_and};

    use super::*;
    #[test]
    fn test() {
        let bool = one_of((
            tag("true").map_yes(|_| true),
            tag("false").map_yes(|_| false),
        ));
        assert_eq!(
            bool.parse_contextless("true..."),
            Ok(yes_and(true, "...").no_ctx()),
        );
        assert_eq!(
            bool.parse_contextless("false..."),
            Ok(yes_and(false, "...").no_ctx()),
        );
    }
}
