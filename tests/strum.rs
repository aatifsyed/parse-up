use parse_up::{
    one_of_iter, tag,
    util::{go_on, yes_and},
    ContextlessUpParser as _, ContextlessUpParserExt as _,
};
use strum::IntoEnumIterator as _;

#[derive(strum::EnumIter, strum::IntoStaticStr, Clone, PartialEq, Debug)]
#[strum(serialize_all = "kebab-case")]
enum Options {
    Foo,
    Bar,
}

#[test]
fn one_of_iter_for_strum() {
    let parser = one_of_iter(
        Options::iter()
            .zip(Options::iter())
            .map(|(left, right)| tag(left.into()).map_yes(move |_| right.clone())),
    );
    assert_eq!(
        parser.parse_contextless("f"),
        Err(go_on(["oo"]).closed().no_ctx())
    );

    assert_eq!(
        parser.parse_contextless("bar..."),
        Ok(yes_and(Options::Bar, "...").no_ctx())
    );
}
