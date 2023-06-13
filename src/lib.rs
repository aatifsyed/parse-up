mod contextless;
mod ext;
mod one_of;
mod permute;
mod series;
pub mod util;
pub use contextless::{bool, displayed, tag, whitespace};
pub use ext::{ContextlessUpParserExt, UpResultExt};
pub use one_of::{one_of, one_of_iter};
pub use permute::permute;
pub use series::series;
mod lex;

pub type UpResult<'input, Out, Ctx> = Result<YesAnd<'input, Out, Ctx>, UpError<'input, Ctx>>;
pub type ContextlessUpResult<'input, Out> = UpResult<'input, Out, ()>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Suggestions {
    Open(Vec<String>),
    /// Must have at least one
    Closed(String, Vec<String>),
}

/// Successful parse so far.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct YesAnd<'input, Out, Ctx> {
    /// The value.
    pub yes: Out,
    /// The remaining input.
    pub and: &'input str,
    /// Other valid options.
    pub could_also: Option<Suggestions>,
    /// Context for future parsers to base suggestions off
    pub ctx: Ctx,
}

impl<'input, Out, Ctx> YesAnd<'input, Out, Ctx> {
    pub fn map_yes<Out2>(self, f: impl Fn(Out) -> Out2) -> YesAnd<'input, Out2, Ctx> {
        YesAnd {
            yes: f(self.yes),
            and: self.and,
            could_also: self.could_also,
            ctx: self.ctx,
        }
    }
    pub fn map_ctx<Ctx2>(self, f: impl Fn(Ctx) -> Ctx2) -> YesAnd<'input, Out, Ctx2> {
        YesAnd {
            yes: self.yes,
            and: self.and,
            could_also: self.could_also,
            ctx: f(self.ctx),
        }
    }
}

impl<'input, T, Ctx> YesAnd<'input, T, Ctx> {
    /// Discard the suggestions, and extract the remainder of input, the parsed output, and the context.
    /// Useful for chaining together parsers.
    pub fn cont(self) -> (&'input str, T, Ctx) {
        (self.and, self.yes, self.ctx)
    }
}

/// Parsing couldn't continue.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UpError<'input, Ctx> {
    /// Parse error.
    Oops {
        input: &'input str,
        message: String,
        ctx: Ctx,
    },
    /// Suggestions to append to the current input which would make parsing succeed.
    /// You should only return this when you're at the end of the input.
    GoOn { go_on: Suggestions, ctx: Ctx },
}

pub trait ContextlessUpParser<'input, Out> {
    fn parse_contextless(&self, input: &'input str) -> ContextlessUpResult<'input, Out>;
}

impl<'input, ParserFn, Out> ContextlessUpParser<'input, Out> for ParserFn
where
    ParserFn: Fn(&'input str) -> ContextlessUpResult<'input, Out>,
{
    fn parse_contextless(&self, input: &'input str) -> ContextlessUpResult<'input, Out> {
        self(input)
    }
}

pub trait ContextualUpParser<'input, Out, Ctx, MapCtx = Ctx> {
    fn parse_contextual(&self, input: &'input str, ctx: Ctx) -> UpResult<'input, Out, MapCtx>;
}

impl<'input, ParserFn, Out, Ctx, MapCtx> ContextualUpParser<'input, Out, Ctx, MapCtx> for ParserFn
where
    ParserFn: Fn(&'input str, Ctx) -> UpResult<'input, Out, MapCtx>,
{
    fn parse_contextual(&self, input: &'input str, ctx: Ctx) -> UpResult<'input, Out, MapCtx> {
        self(input, ctx)
    }
}
