use std::{cell::RefCell, marker::PhantomData, rc::Rc};

use crate::{util::assert_up_parser, UpParser, UpResult, YesAnd};

pub trait UpParserExt<'input, Out>: UpParser<'input, Out> {
    // RUST(aatifsyed): use impl trait in trait
    fn map_yes<MapFn, MappedOut>(self, map_fn: MapFn) -> MapYes<Self, Out, MapFn>
    where
        MapFn: FnMut(Out) -> MappedOut,
        Self: Sized,
    {
        assert_up_parser(MapYes {
            parser: self,
            parser_out: PhantomData,
            map_fn,
        })
    }
    // BUG?(aatifsyed): is this necessary?
    fn borrowed(&mut self) -> Borrowed<'_, Self> {
        // assert_up_parser::<'input, Out, _>(Borrowed(self))
        Borrowed(self)
    }
    fn shareable(&mut self) -> Shareable<'_, Self> {
        Shareable::new(self)
    }
}

impl<'input, Out, Parser> UpParserExt<'input, Out> for Parser where Parser: UpParser<'input, Out> {}

pub struct MapYes<Parser, ParserOut, MapFn> {
    parser: Parser,
    parser_out: PhantomData<ParserOut>,
    map_fn: MapFn,
}

impl<'input, ParserOut, MappedOut, Parser, MapFn> UpParser<'input, MappedOut>
    for MapYes<Parser, ParserOut, MapFn>
where
    Parser: UpParser<'input, ParserOut>,
    MapFn: FnMut(ParserOut) -> MappedOut,
{
    fn parse_up(&mut self, input: &'input str) -> UpResult<'input, MappedOut> {
        match self.parser.parse_up(input) {
            Ok(YesAnd {
                yes,
                and,
                suggestions,
            }) => Ok(YesAnd {
                yes: (self.map_fn)(yes),
                and,
                suggestions,
            }),
            Err(e) => Err(e),
        }
    }
}

pub struct Borrowed<'parser, Parser: ?Sized>(&'parser mut Parser);

impl<'input, Out, Parser> UpParser<'input, Out> for Borrowed<'_, Parser>
where
    Parser: UpParser<'input, Out>,
{
    fn parse_up(&mut self, input: &'input str) -> UpResult<'input, Out> {
        self.0.parse_up(input)
    }
}

pub struct Shareable<'a, T: ?Sized>(Rc<RefCell<&'a mut T>>);

impl<'a, T> Clone for Shareable<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<'a, T: ?Sized> Shareable<'a, T> {
    pub fn new(parser: &'a mut T) -> Self {
        Shareable(Rc::new(RefCell::new(parser)))
    }
    pub fn share<'input, Out>(&self) -> impl FnMut(&'input str) -> UpResult<'input, Out> + 'a
    where
        T: UpParser<'input, Out>,
    {
        let rc = Rc::clone(&self.0);
        move |input| RefCell::borrow_mut(&rc).parse_up(input)
    }
}
