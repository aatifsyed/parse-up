mod contextless;
mod ext;
mod one_of;
pub mod ron;
pub mod util;

use std::ops::Add;

pub use contextless::*;
pub use one_of::one_of;
pub type UpResult<'input, Out> = Result<YesAnd<'input, Out>, UpError<'input>>;
pub use Suggestions::{Closed, Open};

/// Successful parse so far.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct YesAnd<'input, Out> {
    /// The value.
    pub yes: Out,
    /// The remaining input.
    pub and: &'input str,
    /// Other valid options.
    pub suggestions: Option<Suggestions>,
}

/// This branch couldn't continue.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UpError<'input> {
    /// Parse error.
    Oops { input: &'input str, message: String },
    /// Suggestions to append to the current input which would make parsing succeed.
    /// You should only return this when you're at the end of the input.
    GoOn(Suggestions),
}

pub trait UpParser<'input, Out> {
    fn parse_up(&mut self, input: &'input str) -> UpResult<'input, Out>;
}

impl<'input, Out, ParserFn> UpParser<'input, Out> for ParserFn
where
    ParserFn: FnMut(&'input str) -> UpResult<'input, Out>,
{
    fn parse_up(&mut self, input: &'input str) -> UpResult<'input, Out> {
        self(input)
    }
}
impl<'input, Out> UpParser<'input, Out> for Box<dyn UpParser<'input, Out>> {
    fn parse_up(&mut self, input: &'input str) -> UpResult<'input, Out> {
        (**self).parse_up(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Suggestions {
    Open(Vec<String>),
    /// Must have at least one
    Closed(String, Vec<String>),
}

impl<'input, Out> YesAnd<'input, Out> {
    pub fn map_yes<Out2>(self, f: impl FnOnce(Out) -> Out2) -> YesAnd<'input, Out2> {
        YesAnd {
            yes: f(self.yes),
            and: self.and,
            suggestions: self.suggestions,
        }
    }
}

impl IntoIterator for Suggestions {
    type Item = String;

    type IntoIter = std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Suggestions::Open(all) => all.into_iter(),
            Suggestions::Closed(first, mut rest) => {
                rest.insert(0, first);
                rest.into_iter()
            }
        }
    }
}

impl Add for Suggestions {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use std::iter::once;
        match (self, rhs) {
            (Open(mut o), more @ Open(_) | more @ Closed(..)) => {
                o.extend(more);
                Open(o)
            }
            (Closed(f, r), Open(more)) => Open(once(f).chain(r).chain(more).collect()),
            (Closed(f0, mut r0), Closed(f1, r1)) => {
                r0.extend(once(f1).chain(r1));
                Closed(f0, r0)
            }
        }
    }
}

impl Add for UpError<'_> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use UpError::{GoOn, Oops};

        match (self, rhs) {
            (oops @ Oops { .. }, Oops { .. }) => oops,
            (Oops { .. }, go_on @ GoOn(_)) => go_on,
            (go_on @ GoOn(_), Oops { .. }) => go_on,
            (GoOn(s0), GoOn(s1)) => GoOn(s0 + s1),
        }
    }
}
