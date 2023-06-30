use crate::{ext::UpParserExt, one_of, tag, UpParser, UpResult};

#[cfg(test)]
use crate::util::{go_on, yes_and};

/// ## Whitespace and comments
///
/// ```ebnf
/// ws = { ws_single | comment };
/// ws_single = "\n" | "\t" | "\r" | " ";
/// comment = ["//", { no_newline }, "\n"] | ["/*", nested_block_comment, "*/"];
/// nested_block_comment = { ? any characters except "/*" or "*/" ? }, [ "/*", nested_block_comment, "*/", nested_block_comment ];
/// ```
///
/// ## Commas
///
/// ```ebnf
/// comma = ws, ",", ws;
/// ```
///
/// ## Extensions
///
/// ```ebnf
/// extensions = { "#", ws, "!", ws, "[", ws, extensions_inner, ws, "]", ws };
/// extensions_inner = "enable", ws, "(", extension_name, { comma, extension_name }, [comma], ws, ")";
/// ```
///
/// For the extension names see the [`extensions.md`][exts] document.
///
/// ## Value
///
/// ```ebnf
/// value = unsigned | signed | float | string | char | bool | option | list | map | tuple | struct | enum_variant;
/// ```
const _: () = ();

/// ## Numbers
///
/// ```ebnf
/// digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
/// hex_digit = "A" | "a" | "B" | "b" | "C" | "c" | "D" | "d" | "E" | "e" | "F" | "f";
/// unsigned = (["0", ("b" | "o")], digit, { digit | '_' } |
///              "0x", (digit | hex_digit), { digit | hex_digit | '_' });
/// signed = ["+" | "-"], unsigned;
/// float = ["+" | "-"], ("inf" | "NaN" | float_num);
/// float_num = (float_int | float_std | float_frac), [float_exp];
/// float_int = digit, { digit };
/// float_std = digit, { digit }, ".", {digit};
/// float_frac = ".", digit, {digit};
/// float_exp = ("e" | "E"), ["+" | "-"], digit, {digit};
/// ```
pub mod numbers {
    use crate::{many_terminated, recognize, series, util::assert_up_parser_fn};

    use super::*;

    pub fn digit(input: &str) -> UpResult<&str> {
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
        .parse_up(input)
    }

    pub fn hex_digit(input: &str) -> UpResult<&str> {
        one_of((
            tag("A"),
            tag("B"),
            tag("C"),
            tag("D"),
            tag("E"),
            tag("F"),
            tag("a"),
            tag("b"),
            tag("c"),
            tag("d"),
            tag("e"),
            tag("f"),
        ))
        .parse_up(input)
    }

    pub fn unsigned_then<'input, TerminalParser, Terminator>(
        mut terminal: TerminalParser,
    ) -> impl FnMut(&'input str) -> UpResult<&str>
    where
        TerminalParser: UpParser<'input, Terminator>,
    {
        // TODO(aatifsyed): only accept binary digits and octal digits after their respective prefixes
        assert_up_parser_fn(move |input| {
            // unsigned = (["0", ("b" | "o")], digit, { digit | '_' } |
            //              "0x", (digit | hex_digit), { digit | hex_digit | '_' });
            let terminal = terminal.shareable();
            one_of((
                // lifted option
                recognize(series((
                    digit,
                    many_terminated(one_of((digit, tag("_"))), terminal.share(), ..),
                ))),
                recognize(series((
                    tag("0b"),
                    digit,
                    many_terminated(one_of((digit, tag("_"))), terminal.share(), ..),
                ))),
                recognize(series((
                    tag("0o"),
                    digit,
                    many_terminated(one_of((digit, tag("_"))), terminal.share(), ..),
                ))),
                recognize(series((
                    tag("0x"),
                    one_of((digit, hex_digit)),
                    many_terminated(one_of((digit, hex_digit, tag("_"))), terminal.share(), ..),
                ))),
            ))
            .parse_up(input)
        })
    }

    #[test]
    fn test_unsigned_then() {
        let mut parser = unsigned_then(tag("!"));
        assert_eq!(
            parser.parse_up(""),
            Err(go_on(0..=9).or(["0b", "0o", "0x"]).closed())
        );
        assert_eq!(
            parser.parse_up("0"),
            Err(go_on(0..=9).or(["_", "!", "b", "o", "x"]).closed())
        );
        assert_eq!(parser.parse_up("0!..."), Ok(yes_and("0!", "...")));
        assert_eq!(
            parser.parse_up("0_"),
            Err(go_on(0..=9).or(["_", "!"]).closed())
        );
        assert_eq!(
            parser.parse_up("0x"),
            Err(go_on(0..=9).or('A'..='F').or('a'..='f').closed())
        );

        assert_eq!(
            parser.parse_up("0xA"),
            Err(go_on(0..=9)
                .or('A'..='F')
                .or('a'..='f')
                .or(["_", "!"])
                .closed())
        );
    }
}

///
/// ## String
///
/// ```ebnf
/// string = string_std | string_raw;
/// string_std = "\"", { no_double_quotation_marks | string_escape }, "\"";
/// string_escape = "\\", ("\"" | "\\" | "b" | "f" | "n" | "r" | "t" | ("u", unicode_hex));
/// string_raw = "r" string_raw_content;
/// string_raw_content = ("#", string_raw_content, "#") | "\"", { unicode_non_greedy }, "\"";
/// ```
///
/// > Note: Raw strings start with an `r`, followed by n `#`s and a quotation mark
///   `"`. They may contain any characters or escapes (except the end sequence).
///   A raw string ends with a quotation mark (`"`), followed by n `#`s. n may be
///   any number, including zero.
///   Example:
///   ```rust
/// r##"This is a "raw string". It can contain quotations or
/// backslashes (\)!"##;
///   ```
/// Raw strings cannot be written in EBNF, as they are context-sensitive.
/// Also see [the Rust document] about context-sensitivity of raw strings.
///
/// [the Rust document]: https://github.com/rust-lang/rust/blob/d046ffddc4bd50e04ffc3ff9f766e2ac71f74d50/src/grammar/raw-string-literal-ambiguity.md
///
/// ## Char
///
/// ```ebnf
/// char = "'", (no_apostrophe | "\\\\" | "\\'"), "'";
/// ```
const _: () = ();

/// ## Boolean
///
/// ```ebnf
/// bool = "true" | "false";
/// ```
pub fn bool(input: &str) -> UpResult<bool> {
    one_of((
        tag("true").map_yes(|_| true),
        tag("false").map_yes(|_| false),
    ))
    .parse_up(input)
}

#[test]
fn test_bool() {
    assert_eq!(bool.parse_up(""), Err(go_on(["true", "false"]).closed()));
    assert_eq!(bool.parse_up("t"), Err(go_on(["rue"]).closed()));
    assert_eq!(bool.parse_up("true..."), Ok(yes_and(true, "...")));
    assert_eq!(bool.parse_up("false..."), Ok(yes_and(false, "...")));
}

/// ## Optional
///
/// ```ebnf
/// option = "None" | option_some;
/// option_some = "Some", ws, "(", ws, value, ws, ")";
/// ```
///
/// ## List
///
/// ```ebnf
/// list = "[", [value, { comma, value }, [comma]], "]";
/// ```
///
/// ## Map
///
/// ```ebnf
/// map = "{", [map_entry, { comma, map_entry }, [comma]], "}";
/// map_entry = value, ws, ":", ws, value;
/// ```
///
/// ## Tuple
///
/// ```ebnf
/// tuple = "(", [value, { comma, value }, [comma]], ")";
/// ```
///
/// ## Struct
///
/// ```ebnf
/// struct = unit_struct | tuple_struct | named_struct;
/// unit_struct = ident | "()";
/// tuple_struct = [ident], ws, tuple;
/// named_struct = [ident], ws, "(", ws, [named_field, { comma, named_field }, [comma]], ")";
/// named_field = ident, ws, ":", ws, value;
/// ```
///
/// ## Enum
///
/// ```ebnf
/// enum_variant = enum_variant_unit | enum_variant_tuple | enum_variant_named;
/// enum_variant_unit = ident;
/// enum_variant_tuple = ident, ws, tuple;
/// enum_variant_named = ident, ws, "(", [named_field, { comma, named_field }, [comma]], ")";
/// ```
///
/// ## Identifier
///
/// ```ebnf
/// ident = ident_std | ident_raw;
/// ident_std = ident_std_first, { ident_std_rest };
/// ident_std_first = "A" | ... | "Z" | "a" | ... | "z" | "_";
/// ident_std_rest = ident_std_first | digit;
/// ident_raw = "r", "#", ident_raw_rest, { ident_raw_rest };
/// ident_raw_rest = ident_std_rest | "." | "+" | "-";
/// ```
const _: () = ();
