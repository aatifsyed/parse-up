// use frunk::hlist::{HCons, HNil};

// use crate::{util::yes_and, UpResult};

// impl<Context> UpParser<Context> for HNil {
//     type Output = ();

//     fn parse<'input>(&self, input: &'input str, _: &mut Context) -> UpResult<'input, Self::Output> {
//         Ok(yes_and((), input))
//     }
// }

// #[cfg(test)]
// mod tests {
//     use frunk::hlist;

//     use crate::{tag, YesAnd};

//     use super::*;
//     #[test]
//     fn fold_two_tags() {
//         let list = hlist![tag("hello"), tag("world")];
//         list.foldl(
//             |acc, el| {
//                 acc.and_then(
//                     |YesAnd {
//                          yes,
//                          and,
//                          could_also,
//                      }| { el(and).map(op) },
//                 )
//             },
//             Ok(yes_and(HNil, "helloworld")),
//         );
//     }
// }
