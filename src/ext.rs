use crate::{UpParser, YesAnd};

pub trait UpParserExt<'input, Out>: UpParser<'input, Out> {
    // TODO(aatifsyed): avoid the allocation by specializing or using impl trait in trait
    fn map_yes<'a, MapFn, Out2>(mut self, mut map_fn: MapFn) -> Box<dyn UpParser<'input, Out2> + 'a>
    where
        Self: Sized + 'a,
        MapFn: FnMut(Out) -> Out2 + 'a,
    {
        Box::new(move |input| match self.parse_up(input) {
            Ok(YesAnd {
                yes,
                and,
                suggestions,
            }) => Ok(YesAnd {
                yes: map_fn(yes),
                and,
                suggestions,
            }),
            Err(e) => Err(e),
        })
    }
}

impl<'input, Out, Parser> UpParserExt<'input, Out> for Parser where Parser: UpParser<'input, Out> {}
