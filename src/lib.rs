#![allow(unused)]
use std::{fmt::Display, process::Output};

use itertools::Itertools;

use util::{chars_needed_to_complete, go_on, oops, yes_and};

mod contextless;
mod ext;
mod one_of;
mod series;
pub mod util;

pub type UpResult<'input, Out, Ctx> =
    Result<YesAnd<'input, Out, Ctx>, UpError<'input, Ctx>>;
pub type ContextlessUpResult<'input, Out> = UpResult<'input, Out, ()>;

/// Successful parse so far.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct YesAnd<'input, Out, Ctx> {
    /// The value.
    pub yes: Out,
    /// The remaining input.
    pub and: &'input str,
    /// Other valid options.
    pub could_also: Vec<String>,
    /// Context for future parsers to base suggestions off
    pub ctx: Ctx,
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
    GoOn { go_on: Vec<String>, ctx: Ctx },
}

pub trait ContextlessUpParser<'input, Out> {
    fn parse_contextless(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out>;
}

impl<'input, ParserFn, Out> ContextlessUpParser<'input, Out> for ParserFn
where
    ParserFn: Fn(&'input str) -> ContextlessUpResult<'input, Out>,
{
    fn parse_contextless(
        &self,
        input: &'input str,
    ) -> ContextlessUpResult<'input, Out> {
        self(input)
    }
}

pub trait ContextualUpParser<'input, Out, Ctx> {
    fn parse_contextual(
        &self,
        input: &'input str,
        ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx>;
}

impl<'input, ParserFn, Out, Ctx> ContextualUpParser<'input, Out, Ctx>
    for ParserFn
where
    ParserFn: Fn(&'input str, Ctx) -> UpResult<'input, Out, Ctx>,
{
    fn parse_contextual(
        &self,
        input: &'input str,
        ctx: Ctx,
    ) -> UpResult<'input, Out, Ctx> {
        self(input, ctx)
    }
}
