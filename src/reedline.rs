use std::marker::PhantomData;

use crate::{UpError, UpParser};

pub fn completer_and_validator<U: Send + 'static>(
    parser: impl for<'input> UpParser<'input, U> + Send + 'static,
) -> (Box<dyn reedline::Completer>, Box<dyn reedline::Validator>) {
    (
        Box::new(UpParseCompleter::new(parser.clone())),
        Box::new(UpParseValidator::new(parser)),
    )
}

pub struct UpParseCompleter<T, U>(T, PhantomData<U>);

impl<ParserT, U> UpParseCompleter<ParserT, U>
where
    ParserT: for<'input> UpParser<'input, U>,
{
    pub fn new(parser: ParserT) -> Self {
        Self(parser, PhantomData)
    }
}

impl<ParserT, U> reedline::Completer for UpParseCompleter<ParserT, U>
where
    ParserT: for<'input> UpParser<'input, U> + Send,
    U: Send,
{
    fn complete(&mut self, line: &str, pos: usize) -> Vec<reedline::Suggestion> {
        if pos != line.len() {
            return vec![];
        }
        if let Err(UpError::GoOn { go_on }) = self.0.parse(line) {
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

pub struct UpParseValidator<T, U>(T, PhantomData<U>);

impl<ParserT, U> UpParseValidator<ParserT, U>
where
    ParserT: for<'input> UpParser<'input, U>,
{
    pub fn new(parser: ParserT) -> Self {
        Self(parser, PhantomData)
    }
}

impl<ParserT, U> reedline::Validator for UpParseValidator<ParserT, U>
where
    ParserT: for<'input> UpParser<'input, U> + Send,
    U: Send,
{
    fn validate(&self, line: &str) -> reedline::ValidationResult {
        match self.0.parse(line) {
            Ok(_) => reedline::ValidationResult::Complete,
            Err(_) => reedline::ValidationResult::Incomplete,
        }
    }
}
