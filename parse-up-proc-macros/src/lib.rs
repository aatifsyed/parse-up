use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::quote;
use syn::{parse::Nothing, parse_macro_input};

#[proc_macro]
pub fn impl_series_for_tuples(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _ = parse_macro_input!(item as Nothing);

    let impl_blocks = (1..=100)
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
                    >
                    Series<Context> for (
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
        .collect::<TokenStream>();

    impl_blocks.into()
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
