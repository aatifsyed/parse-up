// https://github.com/ron-rs/ron/blob/master/docs/grammar.md

use std::borrow::Cow;

use crate::{
    many0, many1, one_of, recognize, series, tag, take_until,
    util::{go_on, oops, yes_and},
    ContextlessUpParser, ContextlessUpParserExt, ContextlessUpResult,
};

pub fn ws(input: &str) -> ContextlessUpResult<&str> {
    // ws = { ws_single | comment };
    recognize(many1(one_of((ws_single, line_comment)))).parse_contextless(input)
}

pub fn ws_single(input: &str) -> ContextlessUpResult<&str> {
    // ws_single = "\n" | "\t" | "\r" | " ";
    one_of((tag("\n"), tag("\t"), tag("\r"), tag(" "))).parse_contextless(input)
}

pub fn line_comment(input: &str) -> ContextlessUpResult<&str> {
    // comment = ["//", { no_newline }, "\n"] | ["/*", nested_block_comment, "*/"];

    // TODO(aatifsyed): nested block comments:
    // nested_block_comment = { ? any characters except "/*" or "*/" ? }, [ "/*", nested_block_comment, "*/", nested_block_comment ];

    let (_, and, _) = tag("//")(input)?.cont();
    match and.find('\n') {
        Some(ix) => Ok(yes_and(&input[..ix + 3], &input[ix + 3..]).no_ctx()),
        None => Err(go_on(["\n"]).open().no_ctx()),
    }
}

pub fn comma(input: &str) -> ContextlessUpResult<&str> {
    // comma = ws, ",", ws;
    tag(",")(input)
}

pub fn digit(input: &str) -> ContextlessUpResult<&str> {
    // digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
    one_of((
        tag("0"),
        tag("1"),
        tag("2"),
        tag("3"),
        tag("4"),
        tag("5"),
        tag("6"),
        tag("7"),
        tag("8"),
        tag("9"),
    ))
    .parse_contextless(input)
}

pub fn hex_digit(input: &str) -> ContextlessUpResult<&str> {
    // hex_digit = "A" | "a" | "B" | "b" | "C" | "c" | "D" | "d" | "E" | "e" | "F" | "f";
    one_of((
        tag("A"),
        tag("a"),
        tag("B"),
        tag("b"),
        tag("C"),
        tag("c"),
        tag("D"),
        tag("d"),
        tag("E"),
        tag("e"),
        tag("F"),
        tag("f"),
    ))
    .parse_contextless(input)
}

pub fn unsigned(input: &str) -> ContextlessUpResult<&str> {
    // unsigned = (["0", ("b" | "o")], digit, { digit | '_' } |
    //              "0x", (digit | hex_digit), { digit | hex_digit | '_' });
    one_of((
        // lifted: no prefix
        recognize(series((digit, many0(one_of((digit, tag("_"))))))),
        recognize(series((tag("0b"), digit, many0(one_of((digit, tag("_"))))))),
        recognize(series((tag("0o"), digit, many0(one_of((digit, tag("_"))))))),
        recognize(series((
            tag("0x"),
            one_of((digit, hex_digit)),
            many0(one_of((digit, hex_digit, tag("_")))),
        ))),
    ))
    .parse_contextless(input)
}

pub fn signed(input: &str) -> ContextlessUpResult<&str> {
    // signed = ["+" | "-"], unsigned;
    recognize(series((one_of((tag("+"), tag("-"))), unsigned))).parse_contextless(input)
}

pub fn float_exp(input: &str) -> ContextlessUpResult<&str> {
    // float_exp = ("e" | "E"), ["+" | "-"], digit, {digit};
    recognize(series((
        one_of((tag("e"), tag("E"))),
        one_of((
            // lifted: no sign
            recognize(series((digit, many0(digit)))),
            recognize(series((tag("+"), digit, many0(digit)))),
            recognize(series((tag("-"), digit, many0(digit)))),
        )),
    )))
    .parse_contextless(input)
}
pub fn float_frac(input: &str) -> ContextlessUpResult<&str> {
    // float_frac = ".", digit, {digit};
    recognize(series((tag("."), digit, many0(digit)))).parse_contextless(input)
}
pub fn float_std(input: &str) -> ContextlessUpResult<&str> {
    // float_std = digit, { digit }, ".", {digit};
    recognize(series((digit, many0(digit), tag("."), many0(digit)))).parse_contextless(input)
}

pub fn float_int(input: &str) -> ContextlessUpResult<&str> {
    // float_int = digit, { digit };
    recognize(series((digit, many0(digit)))).parse_contextless(input)
}

pub fn float_num(input: &str) -> ContextlessUpResult<&str> {
    // float_num = (float_int | float_std | float_frac), [float_exp];

    let float_int_or_float_std_or_float_frac = one_of((float_frac, float_std, float_int));
    one_of((
        // lifted: no exp
        recognize(float_int_or_float_std_or_float_frac),
        recognize(series((float_int_or_float_std_or_float_frac, float_exp))),
    ))
    .parse_contextless(input)
}

pub fn float(input: &str) -> ContextlessUpResult<&str> {
    // float = ["+" | "-"], ("inf" | "NaN" | float_num);

    let inf_or_nan_or_float_num = one_of((tag("inf"), tag("NaN"), float_num));
    one_of((
        // lifted: no sign
        inf_or_nan_or_float_num,
        recognize(series((tag("+"), inf_or_nan_or_float_num))),
        recognize(series((tag("-"), inf_or_nan_or_float_num))),
    ))
    .parse_contextless(input)
}

pub fn string(input: &str) -> ContextlessUpResult<Cow<str>> {
    // TODO(aatifsyed): real string support
    // string = string_std | string_raw;
    // string_std = "\"", { no_double_quotation_marks | string_escape }, "\"";
    // string_escape = "\\", ("\"" | "\\" | "b" | "f" | "n" | "r" | "t" | ("u", unicode_hex));
    // string_raw = "r" string_raw_content;
    // string_raw_content = ("#", string_raw_content, "#") | "\"", { unicode_non_greedy }, "\"";
    let (rest, (_open, body, _close), _) = series((tag("\""), take_until("\""), tag("\"")))
        .parse_contextless(input)?
        .cont();
    Ok(yes_and(Cow::Borrowed(body), rest).no_ctx())
}

pub fn char(input: &str) -> ContextlessUpResult<char> {
    // char = "'", (no_apostrophe | "\\\\" | "\\'"), "'";
    let (rest, (_open, char, _close), _) = series((
        tag("'"),
        one_of((
            tag("\\\\").map_yes(|_| '\\'),
            tag("\\'").map_yes(|_| '\''),
            no_apostrophe,
        )),
        tag("'"),
    ))
    .parse_contextless(input)?
    .cont();
    Ok(yes_and(char, rest).no_ctx())
}

pub fn no_apostrophe(input: &str) -> ContextlessUpResult<char> {
    let first = input.chars().next();
    let second = input.char_indices().nth(1);
    match (first, second) {
        (Some(c), _) if c == '\'' => Err(oops(input, "apostrophe not allowed here").no_ctx()),
        (Some(c), Some((ix, _))) => Ok(yes_and(c, &input[ix..]).completely_open().no_ctx()),
        (Some(c), None) => Ok(yes_and(c, "").completely_open().no_ctx()),
        (None, Some(_)) => unreachable!(),
        (None, None) => Err(go_on::<&str>([]).open().no_ctx()),
    }
}

pub fn bool(input: &str) -> ContextlessUpResult<bool> {
    // bool = "true" | "false";
    one_of((
        tag("true").map_yes(|_| true),
        tag("false").map_yes(|_| false),
    ))
    .parse_contextless(input)
}

// option = "None" | option_some;
// option_some = "Some", ws, "(", ws, value, ws, ")";

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value<'input> {
    Unsigned(&'input str),
    Signed(&'input str),
    Float(&'input str),
    String(Cow<'input, str>),
    Char(char),
    Bool(bool),
    // Option,
    List,
    Map,
    Tuple,
    Struct,
    EnumVariant,
}

pub fn value(input: &str) -> ContextlessUpResult<Value> {
    // value = unsigned | signed | float | string | char | bool | option | list | map | tuple | struct | enum_variant;
    one_of((
        float.map_yes(Value::Float),
        unsigned.map_yes(Value::Unsigned),
        signed.map_yes(Value::Signed),
        string.map_yes(Value::String),
        char.map_yes(Value::Char),
        bool.map_yes(Value::Bool),
    ))
    .parse_contextless(input)
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn opt_lifting() {
        // ["+" | "-"], digit
        let unlifted = recognize(series((one_of((tag(""), tag("+"), tag("-"))), digit)));

        let lifted = one_of((
            digit,
            recognize(series((tag("+"), digit))),
            recognize(series((tag("-"), digit))),
        ));

        assert_eq!(
            unlifted.parse_contextless(""),
            Err(go_on(0..=9).closed().no_ctx()) // The user doesn't know they can add "+" and "-"
        );
        assert_eq!(
            lifted.parse_contextless(""),
            Err(go_on(
                (0..=9)
                    .map(|it| it.to_string())
                    .chain(["+".into(), "-".into()])
            )
            .closed()
            .no_ctx())
        );
    }
}
