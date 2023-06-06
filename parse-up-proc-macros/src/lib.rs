use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::Index;

/// (
///     [Parser0, Parser1, ... ],
///     [Out0, Out1, ...],
///     [0, 1, ...],
/// )
fn vars(num_tuples: usize) -> (Vec<Ident>, Vec<Ident>, Vec<Index>) {
    // Parser0, Parser1 ...
    let parser_ty_param = (0..num_tuples)
        .map(|n| Ident::new(&format!("Parser{n}"), Span::call_site()))
        .collect();
    let out_ty_param = (0..num_tuples)
        .map(|n| Ident::new(&format!("Out{n}"), Span::call_site()))
        .collect();
    let tuple_ix = (0..num_tuples)
        .map(|index| Index {
            index: index.try_into().unwrap(),
            span: Span::call_site(),
        })
        .collect();
    (parser_ty_param, out_ty_param, tuple_ix)
}

#[doc(hidden)]
#[proc_macro]
pub fn _impl_contextless_one_of_parser_sequence_for_tuples(
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = match args::parse(item.into()) {
        Ok(args) => args,
        Err(e) => return e.into_compile_error().into(),
    };
    args.map(|num_tuples| {
        let (parser_ty_param, _, tuple_ix) = vars(num_tuples);
        quote! {
            impl<'input, Out,
                    #(#parser_ty_param,)*
                > ContextlessOneOfParserSequence<'input, Out>
                for (
                    #(#parser_ty_param,)*
                )
            where
                #(#parser_ty_param: ContextlessUpParser<'input, Out>,)*
            {
                fn contextless_one_of(
                    &self,
                    input: &'input str,
                ) -> ContextlessUpResult<'input, Out> {
                    let mut all_suggestions = Vec::new();
                    let mut open = false;
                    let mut error = true;
                    #(
                        match self.#tuple_ix.parse_contextless(input) {
                            Ok(o) => return Ok(o),
                            Err(UpError::GoOn { go_on, .. }) => fold_suggestions(
                                go_on,
                                &mut all_suggestions,
                                &mut open,
                                &mut error,
                            ),
                            Err(UpError::Oops { .. }) => {}
                        }
                    )*
                    Err(finalise_suggestions(
                        input,
                        all_suggestions,
                        error,
                        open,
                        (),
                    ))
                }
            }
        }
    })
    .collect::<TokenStream>()
    .into()
}

#[doc(hidden)]
#[proc_macro]
pub fn _impl_contextual_one_of_parser_sequence_for_tuples(
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = match args::parse(item.into()) {
        Ok(args) => args,
        Err(e) => return e.into_compile_error().into(),
    };
    args.map(|num_tuples| {
        let (parser_ty_param, _, tuple_ix) = vars(num_tuples);
        quote! {
            impl<'input, Out, Ctx,
                #(#parser_ty_param,)*
                >
                ContextualOneOfParserSequence<'input, Out, Ctx>
                for (
                    #(#parser_ty_param,)*
                )
            where
                #(#parser_ty_param: ContextualUpParser<'input, Out, Ctx>,)*
            {
                fn contextual_one_of(
                    &self,
                    input: &'input str,
                    mut ctx: Ctx,
                ) -> UpResult<'input, Out, Ctx> {
                    let mut all_suggestions = Vec::new();
                    let mut open = false;
                    let mut error = true;
                    #(
                        match self.#tuple_ix.parse_contextual(input, ctx) {
                            Ok(o) => return Ok(o),
                            Err(UpError::GoOn {
                                go_on,
                                ctx: new_ctx,
                            }) => {
                                fold_suggestions(
                                    go_on,
                                    &mut all_suggestions,
                                    &mut open,
                                    &mut error,
                                );
                                ctx = new_ctx
                            }
                            Err(UpError::Oops { ctx: new_ctx, .. }) => ctx = new_ctx,
                        }
                    )*
                    Err(finalise_suggestions(
                        input,
                        all_suggestions,
                        error,
                        open,
                        ctx,
                    ))
                }
            }
        }
    })
    .collect::<TokenStream>()
    .into()
}

#[doc(hidden)]
#[proc_macro]
pub fn _impl_contextless_series_parser_sequence_for_tuples(
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = match args::parse(item.into()) {
        Ok(args) => args,
        Err(e) => return e.into_compile_error().into(),
    };
    args.map(|num_tuples| {
        let (parser_ty_param, out_ty_param, tuple_ix) = vars(num_tuples);
        quote! {
            impl<'input,
                #(#parser_ty_param,)*
                #(#out_ty_param,)*
                >
                ContextlessSeriesParserSequence<'input, (
                    #(#out_ty_param,)*
                )> for (
                    #(#parser_ty_param,)*
                )
            where
                #(#parser_ty_param: ContextlessUpParser<'input, #out_ty_param>,)*
            {
                fn contextless_series(
                    &self,
                    mut input: &'input str,
                ) -> ContextlessUpResult<'input, (
                    #(#out_ty_param,)*
                )> {
                    let mut ctx = ();
                    let mut final_suggestions;
                    let yeses = (
                        #({
                            let YesAnd {
                                yes,
                                and,
                                could_also,
                                ctx: new_ctx,
                            } = self.#tuple_ix.parse_contextless(input)?;
                            input = and;
                            final_suggestions = could_also;
                            ctx = new_ctx;
                            yes
                        },)*
                    );
                    Ok(finalize_suggestions(final_suggestions, yeses, input, ctx))
                }
            }
        }
    })
    .collect::<TokenStream>()
    .into()
}

#[doc(hidden)]
#[proc_macro]
pub fn _impl_contextual_series_parser_sequence_for_tuples(
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = match args::parse(item.into()) {
        Ok(args) => args,
        Err(e) => return e.into_compile_error().into(),
    };
    args.map(|num_tuples| {
        let (parser_ty_param, out_ty_param, tuple_ix) = vars(num_tuples);
        quote! {
            impl<'input, Ctx,
                #(#parser_ty_param,)*
                #(#out_ty_param,)*
                >
                ContextualSeriesParserSequence<'input, (
                    #(#out_ty_param,)*
                ), Ctx>
                for (
                    #(#parser_ty_param,)*
                )
            where
                #(#parser_ty_param: ContextualUpParser<'input, #out_ty_param, Ctx>,)*
            {
                fn contextual_series(
                    &self,
                    mut input: &'input str,
                    mut ctx: Ctx,
                ) -> UpResult<'input, (
                    #(#out_ty_param,)*
                ), Ctx> {
                    let mut final_suggestions;
                    let yeses = (
                        #({
                            let YesAnd {
                                yes,
                                and,
                                could_also,
                                ctx: new_ctx,
                            } = self.#tuple_ix.parse_contextual(input, ctx)?;
                            input = and;
                            final_suggestions = could_also;
                            ctx = new_ctx;
                            yes
                        },)*
                    );
                    Ok(finalize_suggestions(final_suggestions, yeses, input, ctx))
                }
            }
        }
    })
    .collect::<TokenStream>()
    .into()
}

fn yes_var(ix: usize) -> Ident {
    Ident::new(&format!("yes{ix}"), Span::call_site())
}

#[doc(hidden)]
#[proc_macro]
pub fn _impl_contextless_permute_parser_sequence_for_tuples(
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = match args::parse(item.into()) {
        Ok(args) => args,
        Err(e) => return e.into_compile_error().into(),
    };
    args.map(|num_tuples| {
        let (parser_ty_param, out_ty_param, _) = vars(num_tuples);
        let inner = (0..num_tuples).map(|tuple_ix| {
            let this = Index::from(tuple_ix);
            let rest = (0..num_tuples)
                .filter(|it| *it != tuple_ix)
                .map(Index::from)
                .collect::<Vec<_>>();
            let this_var_name = yes_var(tuple_ix);
            let rest_var_names = rest.iter().map(|it| yes_var(it.index.try_into().unwrap()));
            let all_var_names = (0..num_tuples).map(yes_var);
            quote! {
                series((
                    self.#this.borrowed(),
                    permute((#(self.#rest.borrowed(),)*)),
                ))
                .map_yes(|(#this_var_name, (#(#rest_var_names,)*))|{
                    (#(#all_var_names,)*)
                })
            }
        });
        quote! {
            impl<'input,
                #(#out_ty_param,)*
                #(#parser_ty_param,)*
                >
                ContextlessPermuteParserSequence<'input, (
                    #(#out_ty_param,)*
                )>
                for (
                    #(#parser_ty_param,)*
                )
            where
                #(#parser_ty_param: ContextlessUpParser<'input, #out_ty_param>,)*
            {
                fn contextless_permute(
                    &self,
                    input: &'input str,
                ) -> ContextlessUpResult<'input, (
                    #(#out_ty_param,)*
                )> {
                    one_of((
                        #(#inner,)*
                    ))
                    .parse_contextless(input)
                }
            }
        }
    })
    .collect::<TokenStream>()
    .into()
}

mod args {
    use std::ops::{Range, RangeInclusive};

    use proc_macro2::{Spacing, Span, TokenStream, TokenTree};

    /// Parse comma-separated ranges and integers
    pub fn parse(tokens: TokenStream) -> syn::Result<impl Iterator<Item = usize>> {
        let tokens = tokens.into_iter().collect::<Vec<_>>();
        let inputs = parse_all(&tokens)?;
        Ok(inputs
            .into_iter()
            .flat_map(|each| -> Box<dyn Iterator<Item = usize>> {
                match each {
                    Input::Range(r) => Box::new(r),
                    Input::RangeInclusive(r) => Box::new(r),
                    Input::Just(j) => Box::new(std::iter::once(j)),
                }
            }))
    }

    fn usize(input: &[TokenTree]) -> syn::Result<(&[TokenTree], usize)> {
        match input.get(0) {
            Some(TokenTree::Literal(literal)) => literal
                .to_string()
                .parse::<usize>()
                .map(|u| (&input[1..], u))
                .map_err(|e| {
                    syn::Error::new_spanned(
                        literal,
                        format!("couldn't parse usize from literal: {e}"),
                    )
                }),
            Some(other) => Err(syn::Error::new_spanned(other, "expected literal")),
            None => Err(eof("usize")),
        }
    }

    fn eof(expected: &str) -> syn::Error {
        syn::Error::new(
            Span::call_site(),
            format!("unexpected end of input, expected {expected}"),
        )
    }

    fn pop_comma(input: &[TokenTree]) -> (&[TokenTree], Option<()>) {
        match input.get(0) {
            Some(TokenTree::Punct(punct))
                if punct.as_char() == ',' && punct.spacing() == Spacing::Alone =>
            {
                (&input[1..], Some(()))
            }
            _ => (input, None),
        }
    }

    fn dot2(input: &[TokenTree]) -> syn::Result<(&[TokenTree], ())> {
        match (input.get(0), input.get(1)) {
            (Some(TokenTree::Punct(punct0)), Some(TokenTree::Punct(punct1)))
                if (punct0.as_char(), punct1.as_char()) == ('.', '.')
                    && (punct0.spacing(), punct1.spacing()) == (Spacing::Joint, Spacing::Alone) =>
            {
                Ok((&input[2..], ()))
            }
            (Some(other), _) => Err(syn::Error::new_spanned(other, "expected `..`")),
            (None, _) => Err(eof("`..`")),
        }
    }

    fn dot2eq(input: &[TokenTree]) -> syn::Result<(&[TokenTree], ())> {
        match (input.get(0), input.get(1), input.get(2)) {
            (
                Some(TokenTree::Punct(punct0)),
                Some(TokenTree::Punct(punct1)),
                Some(TokenTree::Punct(punct2)),
            ) if (punct0.as_char(), punct1.as_char(), punct2.as_char()) == ('.', '.', '=')
                && (punct0.spacing(), punct1.spacing(), punct2.spacing())
                    == (Spacing::Joint, Spacing::Joint, Spacing::Alone) =>
            {
                Ok((&input[3..], ()))
            }
            (Some(other), _, _) => Err(syn::Error::new_spanned(other, "expected `..=`")),
            (None, _, _) => Err(eof("`..=`")),
        }
    }

    fn range(input: &[TokenTree]) -> syn::Result<(&[TokenTree], Range<usize>)> {
        let (input, start) = usize(input)?;
        let (input, _) = dot2(input)?;
        let (input, end) = usize(input)?;
        Ok((input, start..end))
    }

    fn range_inclusive(input: &[TokenTree]) -> syn::Result<(&[TokenTree], RangeInclusive<usize>)> {
        let (input, start) = usize(input)?;
        let (input, _) = dot2eq(input)?;
        let (input, end) = usize(input)?;
        Ok((input, start..=end))
    }

    fn parse_one(input: &[TokenTree]) -> syn::Result<(&[TokenTree], Input)> {
        match (range_inclusive(input), range(input), usize(input)) {
            (Ok((r, o)), _, _) => Ok((r, Input::RangeInclusive(o))),
            (_, Ok((r, o)), _) => Ok((r, Input::Range(o))),
            (_, _, Ok((r, o))) => Ok((r, Input::Just(o))),
            (Err(e0), Err(e1), Err(e2)) => {
                let mut e = syn::Error::new(Span::call_site(), "couldn't parse input");
                e.combine(e0);
                e.combine(e1);
                e.combine(e2);
                Err(e)
            }
        }
    }

    #[derive(Debug)]
    enum Input {
        Range(Range<usize>),
        RangeInclusive(RangeInclusive<usize>),
        Just(usize),
    }

    fn parse_all(mut input: &[TokenTree]) -> syn::Result<Vec<Input>> {
        let mut all = Vec::new();
        let (new, first) = parse_one(input)?;
        all.push(first);
        input = new;

        while let (new, Some(_comma)) = pop_comma(input) {
            input = new;
            match input.is_empty() {
                false => {
                    let (new, next) = parse_one(input)?;
                    input = new;
                    all.push(next)
                }
                true => break,
            }
        }

        if !input.is_empty() {
            return Err(syn::Error::new_spanned(&input[0], "unexpected input"));
        }

        Ok(all)
    }

    #[cfg(test)]
    mod tests {
        use super::parse;
        use proc_macro2::TokenStream;
        use quote::quote;

        #[track_caller]
        fn do_test(tokens: TokenStream, expected: impl IntoIterator<Item = usize>) {
            let s = tokens.to_string();
            let expected = expected.into_iter().collect::<Vec<_>>();
            let parsed =
                parse(tokens).unwrap_or_else(|e| panic!("parsing failed, tokens: {s}; error: {e}"));
            let actual = parsed.collect::<Vec<_>>();
            assert_eq!(expected, actual, "tokens: {s}");
        }

        #[test]
        fn parsing() {
            do_test(quote!(1), [1]);
            do_test(quote!(1,), [1]);
            do_test(quote!(1..1), []);
            do_test(quote!(1..=1), [1]);
            do_test(quote!(1..2), [1]);
            do_test(quote!(1..=2), [1, 2]);
            do_test(quote!(1, 1..=2), [1, 1, 2]);
        }
    }
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
