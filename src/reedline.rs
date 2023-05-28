use std::marker::PhantomData;

use crate::{UpError, UpResult};

pub fn completer<'parser, U: Send + 'parser>(
    parser: impl Fn(&str) -> UpResult<U> + Send + 'parser,
) -> Box<dyn reedline::Completer + 'parser> {
    Box::new(UpParseCompleter::new(parser))
}
pub fn validator<'parser, U: Send + 'parser>(
    parser: impl Fn(&str) -> UpResult<U> + Send + 'parser,
) -> Box<dyn reedline::Validator + 'parser> {
    Box::new(UpParseValidator::new(parser))
}

struct UpParseCompleter<T, U>(T, PhantomData<U>);

impl<ParserT, U> UpParseCompleter<ParserT, U>
where
    ParserT: Fn(&str) -> UpResult<U>,
{
    pub fn new(parser: ParserT) -> Self {
        Self(parser, PhantomData)
    }
}

impl<ParserT, U> reedline::Completer for UpParseCompleter<ParserT, U>
where
    ParserT: Fn(&str) -> UpResult<U> + Send,
    U: Send,
{
    fn complete(&mut self, line: &str, pos: usize) -> Vec<reedline::Suggestion> {
        if pos != line.len() {
            return vec![];
        }
        if let Err(UpError::GoOn { go_on }) = (self.0)(line) {
            return go_on
                .into_iter()
                .map(|value| reedline::Suggestion {
                    value,
                    description: None,
                    extra: None,
                    span: reedline::Span {
                        start: line.len(),
                        end: line.len(),
                    },
                    append_whitespace: false,
                })
                .collect();
        }
        vec![]
    }
}

struct UpParseValidator<T, U>(T, PhantomData<U>);

impl<ParserT, U> UpParseValidator<ParserT, U>
where
    ParserT: Fn(&str) -> UpResult<U>,
{
    pub fn new(parser: ParserT) -> Self {
        Self(parser, PhantomData)
    }
}

impl<ParserT, U> reedline::Validator for UpParseValidator<ParserT, U>
where
    ParserT: Fn(&str) -> UpResult<U> + Send,
    U: Send,
{
    fn validate(&self, line: &str) -> reedline::ValidationResult {
        match (self.0)(line) {
            Ok(_) => reedline::ValidationResult::Complete,
            Err(_) => reedline::ValidationResult::Incomplete,
        }
    }
}
