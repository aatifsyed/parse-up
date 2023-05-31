use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::quote;
use syn::{parse::Nothing, parse_macro_input};

#[doc(hidden)]
#[proc_macro]
pub fn _impl_series_for_tuples(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _ = parse_macro_input!(item as Nothing);

    (1..=100)
        .map(|num_tuples| {
            // Parser0, Parser1 ...
            let member_idents = (0..num_tuples)
                .map(|n| Ident::new(&format!("Parser{n}"), Span::call_site()))
                .collect::<Vec<_>>();
            let tuple_ix = (0..num_tuples).map(Literal::usize_unsuffixed);
            quote! {
                impl<
                    Context,
                    #(#member_idents,)*
                    > Series<Context> for (
                        #(#member_idents,)*
                    )
                where
                    #(
                        #member_idents: UpParser<Context>,
                    )*
                {
                    type SequenceOut = (
                        #(#member_idents ::Output,)*
                    );

                    fn sequence<'input>(
                        &self,
                        mut input: &'input str,
                        context: &mut Context,
                    ) -> UpResult<'input, Self::SequenceOut> {
                        let parsed = (
                            #(
                                {
                                    let (rest, t) = (self.#tuple_ix).parse(input, context)?.cont();
                                    input = rest;
                                    t
                                },
                            )*
                        );
                        Ok(yes_and(parsed, input))
                    }
                }

            }
        })
        .collect::<TokenStream>()
        .into()
}

#[doc(hidden)]
#[proc_macro]
pub fn _impl_one_of_for_tuples(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _ = parse_macro_input!(item as Nothing);
    (1..=100)
        .map(|num_tuples| {
            // Parser0, Parser1 ...
            let member_idents = (0..num_tuples)
                .map(|n| Ident::new(&format!("Parser{n}"), Span::call_site()))
                .collect::<Vec<_>>();
            let tuple_ix = (0..num_tuples).map(Literal::usize_unsuffixed);

            quote! {
                impl<
                    Context,
                    Out,
                    #(#member_idents,)*
                    > OneOf<Context> for (
                        #(#member_idents,)*
                    )
                where
                    #(
                        #member_idents: UpParser<Context, Output = Out>,
                    )*
                {
                    type Output = Out;

                    fn one_of<'input>(
                        &self,
                        input: &'input str,
                        context: &mut Context,
                    ) -> UpResult<'input, Self::Output> {
                        let mut winning_yes = None;
                        let mut winning_and = input;
                        let mut suggestions = Vec::new();
                        #(
                            match self.#tuple_ix.parse(input, context) {
                                Ok(YesAnd {
                                    yes,
                                    and,
                                    could_also,
                                }) => {
                                    if winning_yes.is_none() {
                                        winning_yes = Some(yes);
                                        winning_and = and;
                                    }
                                    suggestions.extend(could_also)
                                }
                                Err(UpError::GoOn { go_on }) => suggestions.extend(go_on),
                                Err(UpError::Oops { input, message }) => {}
                            }
                        )*

                        match winning_yes {
                            Some(yes) => Ok(yes_and_also(yes, winning_and, suggestions)),
                            None if !suggestions.is_empty() => Err(go_on(suggestions)),
                            None => Err(oops(input, "no branches matched")),
                        }
                    }
                }
            }
        })
        .collect::<TokenStream>()
        .into()
}

#[cfg(test)]
mod tests {
    #[test]
    fn trybuild() {
        let t = trybuild::TestCases::new();
        t.pass("trybuild/pass/**/*.rs");
        t.compile_fail("trybuild/fail/**/*.rs")
    }
}
