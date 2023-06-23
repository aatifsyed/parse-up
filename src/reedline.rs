use reedline::{Completer, Span};
use std::marker::PhantomData;

use crate::{ContextlessUpParser, UpError, YesAnd};

pub struct UpCompleter<'input, Parser, Out>(Parser, PhantomData<Out>, PhantomData<&'input ()>);

impl<'input, Parser, Out> UpCompleter<'input, Parser, Out> {
    pub fn new(parser: Parser) -> Self
    where
        Parser: ContextlessUpParser<'input, Out>,
    {
        Self(parser, PhantomData, PhantomData)
    }
}

unsafe impl<'input, Parser, Out> Send for UpCompleter<'input, Parser, Out> where Parser: Send {}

impl<'input, Parser, Out> Completer for UpCompleter<'input, Parser, Out>
where
    Parser: Send + ContextlessUpParser<'input, Out>,
{
    fn complete(&mut self, line: &str, pos: usize) -> Vec<reedline::Suggestion> {
        if pos != line.len() {
            return vec![]; // only suggest at the end
        }
        let line = unsafe { std::mem::transmute::<_, &'input str>(line) };
        match self.0.parse_contextless(line) {
            Ok(YesAnd {
                could_also: Some(suggestions),
                ..
            })
            | Err(UpError::GoOn {
                go_on: suggestions, ..
            }) => suggestions
                .into_iter()
                .map(|value| reedline::Suggestion {
                    value,
                    description: None,
                    extra: None,
                    span: Span {
                        start: line.len(),
                        end: line.len(),
                    },
                    append_whitespace: false,
                })
                .collect(),
            _ => vec![], // nothing to suggest
        }
    }
}
