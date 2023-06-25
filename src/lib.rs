pub type UpResult<'input, Out> = Result<YesAnd<'input, Out>, UpError<'input>>;

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
