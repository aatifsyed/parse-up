use crate::{ext::UpParserExt, util::assert_up_parser_fn, UpParser, UpResult, YesAnd};

#[cfg(test)]
use crate::{
    ron, tag,
    util::{go_on, yes_and},
};

use std::ops::RangeBounds;

/// Parsing multiple items while keeping suggestions is non-trivial
///
/// # Panics
/// If `many` and `terminal` both successfully parse an input
pub fn many_terminated<'input, Out, RepeatParser, TerminalParser, Terminator>(
    mut repeat: RepeatParser,
    mut terminal: TerminalParser,
    bounds: impl RangeBounds<usize>,
) -> impl FnMut(&'input str) -> UpResult<'input, (Vec<Out>, Terminator)>
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
                match (
                    repeat.borrowed().parse_up(input),
                    terminal.borrowed().parse_up(input),
                ) {
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
                            yes: (yeses, yes),
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
            .map_yes(|terminator| (std::mem::take(&mut yeses), terminator))
            .parse_up(input)
    })
}

#[test]
fn test() {
    let mut many0 = many_terminated(ron::bool, tag("end"), ..);

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
        many0.parse_up("trueend"),
        Ok(yes_and((vec![true], "end"), ""))
    );
}
