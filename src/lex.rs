use std::borrow::Cow;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    multi::many0,
    Parser,
};

pub enum Item<'input> {
    Plain(&'input str),
    Quoted(QuotedString<'input>),
    Delimited(Delimited<'input>),
}

pub struct QuotedString<'input> {
    open: &'input str,
    pub contents: Cow<'input, str>,
    close: Option<&'input str>,
}

pub struct Delimited<'input> {
    open: &'input str,
    pub contents: Vec<Item<'input>>,
    close: Option<&'input str>,
}

fn items(input: &str) -> nom::IResult<&str, Vec<Item>> {
    many0(item)(input)
}

fn item(input: &str) -> nom::IResult<&str, Item> {
    alt((
        plain.map(Item::Plain),
        delimited.map(Item::Delimited),
        quoted.map(Item::Quoted),
    ))(input)
}

fn plain(input: &str) -> nom::IResult<&str, &str> {
    use std::cmp::Ordering::{Equal, Greater, Less};

    for char in ['"', '(', ')'] {}
    match (input.find('"'), input.find('(')) {
        (Some(q), Some(p)) => match q.cmp(&p) {
            Less => Ok(input.split_at(q)),
            Equal => unreachable!(),
            Greater => Ok(input.split_at(p)),
        },
        (None, None) => Ok((input, "")),
        (None, Some(p)) => Ok(input.split_at(p)),
        (Some(q), None) => Ok(input.split_at(q)),
    }
}

fn quoted(input: &str) -> nom::IResult<&str, QuotedString> {
    todo!()
}

fn delimited(input: &str) -> nom::IResult<&str, Delimited> {
    let (input, open) = tag("(")(input)?;
    todo!()
}
