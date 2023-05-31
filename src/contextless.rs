// use std::fmt::Display;

// use itertools::Itertools;

// use crate::{
//     util::{chars_needed_to_complete, go_on, oops, yes_and, yes_and_also},
//     UpError, UpParser, UpResult, YesAnd,
// };

// /// Takes the string `tag` from the input.
// /// ```
// /// use parse_up::{tag, util::{yes_and, go_on, oops}};
// ///
// /// let ctx = &mut ();
// ///
// /// assert_eq!(
// ///     tag("hello")("", ctx),
// ///     Err(go_on(["hello"])),
// /// );
// /// assert_eq!(
// ///     tag("hello")("hell", ctx),
// ///     Err(go_on(["o"])),
// /// );
// /// assert_eq!(
// ///     tag("hello")("hello", ctx),
// ///     Ok(yes_and("hello".into(), "")),
// /// );
// /// assert_eq!(
// ///     tag("hello")("hello, world!", ctx),
// ///     Ok(yes_and("hello".into(), ", world!")),
// /// );
// /// assert_eq!(
// ///     tag("hello")("world", ctx),
// ///     Err(oops("world", "expected hello")),
// /// );
// /// ```
// #[allow(clippy::needless_lifetimes)]
// pub fn tag<'tag, Context>(
//     tag: &'tag str,
// ) -> impl for<'input> Fn(&'input str, &mut Context) -> UpResult<'input, String> + 'tag {
//     // the trait solver beat me up and stole my lunch money :(          ^^^^^^
//     move |input, _| match input.strip_prefix(tag) {
//         Some(rest) => Ok(yes_and(String::from(&input[..tag.len()]), rest)),
//         None => match chars_needed_to_complete(tag, input) {
//             Some("") => unreachable!("would've been caught in prefix"),
//             Some(suggestion) => Err(go_on([suggestion])),
//             None => Err(oops(input, format!("expected {tag}"))),
//         },
//     }
// }

// /// ```
// /// use parse_up::{dictionary, util::{yes_and, go_on, oops}};
// ///
// /// let parser = dictionary([
// ///     ("true", true),
// ///     ("false", false),
// ///     ("yes", true),
// ///     ("no", false),
// /// ]);
// ///
// /// let ctx = &mut ();
// ///
// /// assert_eq!(
// ///     parser("true etc", ctx),
// ///     Ok(yes_and(true, " etc")),
// /// );
// /// assert_eq!(
// ///     parser("", ctx),
// ///     Err(go_on(["yes", "true", "no", "false"])),
// /// );
// /// assert_eq!(
// ///     parser("y", ctx),
// ///     Err(go_on(["es"])),
// /// );
// /// assert_eq!(
// ///     parser("yep", ctx),
// ///     Err(oops("yep", "expected one of [yes, true, no, false]")),
// /// );
// /// ```
// pub fn dictionary<KeyT, ValueT, Context>(
//     items: impl IntoIterator<Item = (KeyT, ValueT)>,
// ) -> impl for<'input> Fn(&'input str, &mut Context) -> UpResult<'input, ValueT> + Clone
// where
//     KeyT: Display,
//     ValueT: Clone,
// {
//     let pairs = items
//         .into_iter()
//         .map(|(k, v)| (k.to_string(), v))
//         // largest keys first
//         .sorted_by_key(|(k, _v)| std::cmp::Reverse(k.clone()))
//         .collect::<Vec<_>>();
//     move |input: &str, ctx| {
//         let mut suggestions = vec![];
//         for (k, v) in &pairs {
//             match tag(k)(input, ctx).map(
//                 |YesAnd {
//                      yes,
//                      and,
//                      could_also,
//                  }| YesAnd {
//                     yes: v.clone(),
//                     and,
//                     could_also,
//                 },
//             ) {
//                 Ok(ok) => return Ok(ok),
//                 Err(UpError::Oops { .. }) => continue, // try another key
//                 Err(UpError::GoOn { go_on }) => suggestions.extend(go_on),
//             }
//         }
//         match suggestions.is_empty() {
//             true => Err(oops(
//                 input,
//                 format!(
//                     "expected one of [{}]",
//                     pairs.iter().map(|it| &it.0).join(", ")
//                 ),
//             )),
//             false => Err(go_on(suggestions)),
//         }
//     }
// }

// /// ```
// /// use parse_up::{dictionary, many1, util::{yes_and, go_on}};
// ///
// /// let parser = dictionary([("true", true), ("false", false)]);
// ///
// /// let ctx = &mut ();
// ///
// /// assert_eq!(
// ///     many1(parser.clone())("truefalse...", ctx),
// ///     Ok(yes_and(vec![true, false], "...")),
// /// );
// /// assert_eq!(
// ///     many1(parser)("t", ctx),
// ///     Err(go_on(["rue"])),
// /// );
// /// ```
// pub fn many1<'input, Context, Output>(
//     parser: impl UpParser<'input, Context, Output>,
// ) -> impl Fn(&'input str, &mut Context) -> UpResult<'input, Vec<Output>> {
//     move |input: &str, ctx| {
//         let YesAnd {
//             yes,
//             and,
//             could_also,
//         } = parser.parse(input, ctx)?;
//         let mut yeses = vec![yes];
//         let mut input = and;
//         while let Ok(YesAnd { yes, and, .. }) = parser.parse(input, ctx) {
//             input = and;
//             yeses.push(yes);
//         }
//         Ok(yes_and_also(yeses, input, could_also))
//     }
// }

// /// ```
// /// use parse_up::{whitespace, util::{yes_and, go_on, oops}};
// ///
// /// let ctx = &mut ();
// ///
// /// assert_eq!(
// ///     whitespace(" hello", ctx),
// ///     Ok(yes_and(" ".into(), "hello")),
// /// );
// /// assert_eq!(
// ///     whitespace("    hello", ctx),
// ///     Ok(yes_and("    ".into(), "hello")),
// /// );
// /// assert_eq!(
// ///     whitespace("", ctx),
// ///     Err(go_on([" "])),
// /// );
// /// assert_eq!(
// ///     whitespace("hello", ctx),
// ///     Err(oops("hello", "expected whitespace")),
// /// );
// /// ```
// pub fn whitespace<'input, ContextT>(
//     input: &'input str,
//     context: &mut ContextT,
// ) -> UpResult<'input, String> {
//     if input.is_empty() {
//         return Err(go_on([" "]));
//     }
//     let trimmed = input.trim_start();
//     let bytes_trimmed = input.len() - trimmed.len();
//     match bytes_trimmed {
//         0 => Err(oops(input, "expected whitespace")),
//         _ => Ok(yes_and(String::from(&input[..bytes_trimmed]), trimmed)),
//     }
// }

// /// Returns [`None`] if the inner parser fails, feeding it empty input to get suggestions as required.
// /// ```
// /// use parse_up::{opt, util::{yes_and, yes_and_also}, dictionary};
// ///
// /// let parser = opt(dictionary([("true", true), ("false", false)]));
// ///
// /// let ctx = &mut ();
// ///
// /// assert_eq!(
// ///     parser("true...", ctx),
// ///     Ok(yes_and(Some(true), "...")),
// /// );
// ///
// /// assert_eq!(
// ///     parser("...", ctx),
// ///     Ok(yes_and_also(None, "...", ["true", "false"])),
// /// );
// ///
// /// ```
// pub fn opt<'input, OutT, Context>(
//     parser: impl UpParser<Context, Output = OutT>,
// ) -> impl Fn(&'input str, &mut Context) -> UpResult<'input, Option<OutT>> {
//     move |input, ctx| match parser.parse(input, ctx) {
//         Ok(YesAnd {
//             yes,
//             and,
//             could_also,
//         }) => Ok(YesAnd {
//             yes: Some(yes),
//             and,
//             could_also,
//         }),
//         Err(UpError::GoOn { go_on }) if !go_on.is_empty() => Ok(yes_and_also(None, input, go_on)),
//         Err(_) => {
//             let suggestions = if let Err(UpError::GoOn { go_on }) = parser.parse("", ctx) {
//                 go_on
//             } else {
//                 todo!("is this a user bug?")
//             };
//             Ok(yes_and_also(None, input, suggestions))
//         }
//     }
// }
