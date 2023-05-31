#![allow(unused)]
use std::fmt::Display;

use itertools::Itertools;

use util::{chars_needed_to_complete, go_on, oops, yes_and, yes_and_also};

mod contextless;
mod frunk;
mod one_of;
pub mod reedline;
mod series;
pub mod util;

pub type UpResult<'input, T> = Result<YesAnd<'input, T>, UpError<'input>>;

/// Successful parse so far.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct YesAnd<'input, T> {
    /// The value.
    pub yes: T,
    /// The remaining input.
    pub and: &'input str,
    /// Other valid options.
    pub could_also: Vec<String>,
}

impl<'input, T> YesAnd<'input, T> {
    /// Discard the suggestions, and extract the remainder of input, and the parsed output.
    /// Useful for chaining together parsers.
    pub fn cont(self) -> (&'input str, T) {
        (self.and, self.yes)
    }
}

/// Parsing couldn't continue.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UpError<'input> {
    /// Parse error.
    Oops { input: &'input str, message: String },
    /// Suggestions to append to the current input which would make parsing succeed.
    /// You should only return this when you're at the end of the input.
    GoOn { go_on: Vec<String> },
}

pub trait UpParser<Context> {
    type Output;
    fn parse<'input>(
        &self,
        input: &'input str,
        context: &mut Context,
    ) -> UpResult<'input, Self::Output>;
}

impl<ParserFn, FnOut, Context> UpParser<Context> for ParserFn
where
    ParserFn: for<'input> Fn(&'input str, &mut Context) -> UpResult<'input, FnOut>,
{
    type Output = FnOut;

    fn parse<'input>(
        &self,
        input: &'input str,
        context: &mut Context,
    ) -> UpResult<'input, Self::Output> {
        self(input, context)
    }
}

pub trait UpParserExt<Context>: UpParser<Context> + Sized {}
impl<T, Context> UpParserExt<Context> for T where T: UpParser<Context> + Sized {}
