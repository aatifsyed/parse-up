use crate::{ext::UpParserExt, util::assert_up_parser_fn, UpParser, UpResult, YesAnd};

#[cfg(test)]
use crate::{
    ron, tag,
    util::{go_on, yes_and},
};

use std::ops::RangeBounds;

pub fn many_terminated<'input, Out, RepeatParser, TerminalParser, Terminator>(
    repeat: RepeatParser,
    terminal: TerminalParser,
    bounds: impl RangeBounds<usize>,
) -> impl FnMut(&'input str) -> UpResult<'input, Vec<Out>>
where
    RepeatParser: UpParser<'input, Out>,
    TerminalParser: UpParser<'input, Terminator>,
{
    let mut parser =
        many_terminated_full(repeat, terminal, bounds).map_yes(|(yeses, _rest, _terminator)| yeses);
    assert_up_parser_fn(move |input| parser.parse_up(input))
}

pub fn many_terminated_recognised<'input, Out, RepeatParser, TerminalParser, Terminator>(
    repeat: RepeatParser,
    terminal: TerminalParser,
    bounds: impl RangeBounds<usize>,
) -> impl FnMut(&'input str) -> UpResult<'input, &'input str>
where
    RepeatParser: UpParser<'input, Out>,
    TerminalParser: UpParser<'input, Terminator>,
{
    let mut parser = many_terminated_full(repeat, terminal, bounds);
    assert_up_parser_fn(move |input| match parser.parse_up(input) {
        Ok(YesAnd {
            yes: (_yeses, rest, _terminator),
            and,
            suggestions,
        }) => Ok(YesAnd {
            yes: input.strip_suffix(rest).unwrap(),
            and,
            suggestions,
        }),
        Err(e) => Err(e),
    })
}

/// Returns
/// - The repeats
/// - The slice where the terminator started
/// - The terminator
///
/// # Panics
/// If `many` and `terminal` both successfully parse an input
pub fn many_terminated_full<'input, Out, RepeatParser, TerminalParser, Terminator>(
    mut repeat: RepeatParser,
    mut terminal: TerminalParser,
    bounds: impl RangeBounds<usize>,
) -> impl FnMut(&'input str) -> UpResult<'input, (Vec<Out>, &'input str, Terminator)>
where
    RepeatParser: UpParser<'input, Out>,
    TerminalParser: UpParser<'input, Terminator>,
{
    let bounds = (bounds.start_bound().cloned(), bounds.end_bound().cloned());

    assert_up_parser_fn(move |mut input: &'input str| {
        let mut yeses = vec![];

        // can't terminate
        while !bounds.contains(&yeses.len()) {
            let YesAnd { yes, and, .. } = repeat.parse_up(input)?;
            yeses.push(yes);
            input = and;
        }

        // could terminate
        while bounds.contains(&yeses.len()) {
            loop {
                match (repeat.parse_up(input), terminal.parse_up(input)) {
                    (Ok(_), Ok(_)) => panic!(
                        "repeating parser and terminal parser both succeeded on input {input}"
                    ),
                    (Ok(YesAnd { yes, and, .. }), Err(_)) => {
                        input = and;
                        yeses.push(yes);
                    }
                    (
                        Err(_),
                        Ok(YesAnd {
                            yes,
                            and,
                            suggestions,
                        }),
                    ) => {
                        return Ok(YesAnd {
                            yes: (yeses, input, yes),
                            and,
                            suggestions,
                        })
                    }
                    (Err(e0), Err(e1)) => return Err(e0 + e1),
                }
            }
        }

        // must terminate
        terminal
            .borrowed()
            .map_yes(|terminator| (std::mem::take(&mut yeses), input, terminator))
            .parse_up(input)
    })
}

#[test]
fn test() {
    let mut many0 = many_terminated_full(ron::bool, tag("end"), ..);

    assert_eq!(
        many0.parse_up(""),
        Err(go_on(["true", "false", "end"]).closed())
    );
    assert_eq!(many0.parse_up("t"), Err(go_on(["rue"]).closed()));
    assert_eq!(
        many0.parse_up("true"),
        Err(go_on(["true", "false", "end"]).closed())
    );
    assert_eq!(
        many0.parse_up("trueend..."),
        Ok(yes_and((vec![true], "end...", "end"), "..."))
    );

    let mut parser = many_terminated_recognised(ron::bool, tag("!"), ..);
    assert_eq!(
        parser.parse_up("truefalse!..."),
        Ok(yes_and("truefalse", "..."))
    );
}
