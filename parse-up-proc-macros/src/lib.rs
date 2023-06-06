use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Expr, ExprLit, ExprRange, Lit, RangeLimits,
};

struct RangeArg {
    start: usize,
    limits: RangeLimits,
    end: usize,
}

impl IntoIterator for RangeArg {
    type Item = usize;

    type IntoIter = Box<dyn Iterator<Item = usize>>;

    fn into_iter(self) -> Self::IntoIter {
        match self.limits {
            RangeLimits::HalfOpen(_) => Box::new(self.start..self.end),
            RangeLimits::Closed(_) => Box::new(self.start..=self.end),
        }
    }
}

fn get_usize(expr: &Expr) -> syn::Result<usize> {
    match expr {
        Expr::Lit(ExprLit {
            attrs,
            lit: Lit::Int(lit_int),
        }) => match attrs.is_empty() {
            true => lit_int.base10_parse(),
            false => Err(syn::Error::new_spanned(
                expr,
                "attributes are not supported",
            )),
        },
        _ => Err(syn::Error::new_spanned(expr, "must be an integer literal")),
    }
}

impl Parse for RangeArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let expr_range = ExprRange::parse(input)?;
        let ExprRange {
            attrs,
            start,
            limits: _,
            end,
        } = &expr_range;
        if !attrs.is_empty() {
            return Err(syn::Error::new_spanned(
                expr_range,
                "attributes are not supported",
            ));
        }
        let (Some(start), Some(end)) = (start, end) else {
            return Err(syn::Error::new_spanned(expr_range, "range must have a concrete start and end"))
        };
        Ok(Self {
            start: get_usize(start)?,
            limits: expr_range.limits,
            end: get_usize(end)?,
        })
    }
}

/// (
///     [Parser0, Parser1, ... ],
///     [Out0, Out1, ...],
///     [0, 1, ...],
/// )
fn vars(num_tuples: usize) -> (Vec<Ident>, Vec<Ident>, Vec<Literal>) {
    // Parser0, Parser1 ...
    let parser_ty_param = (0..num_tuples)
        .map(|n| Ident::new(&format!("Parser{n}"), Span::call_site()))
        .collect();
    let out_ty_param = (0..num_tuples)
        .map(|n| Ident::new(&format!("Out{n}"), Span::call_site()))
        .collect();
    let tuple_ix = (0..num_tuples).map(Literal::usize_unsuffixed).collect();
    (parser_ty_param, out_ty_param, tuple_ix)
}

#[doc(hidden)]
#[proc_macro]
pub fn _impl_contextless_one_of_parser_sequence_for_tuples(
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let arg = parse_macro_input!(item as RangeArg);

    arg.into_iter()
        .map(|num_tuples| {
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
    let arg = parse_macro_input!(item as RangeArg);

    arg.into_iter()
        .map(|num_tuples| {
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
    let arg = parse_macro_input!(item as RangeArg);

    arg.into_iter()
        .map(|num_tuples| {
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
    let arg = parse_macro_input!(item as RangeArg);

    arg.into_iter()
        .map(|num_tuples| {
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

mod args {
    use std::ops::{Range, RangeInclusive};

    use proc_macro2::{Literal, Spacing, Span, TokenStream, TokenTree};
    use syn::{
        parse::{Parse, ParseStream},
        punctuated::Punctuated,
        Expr, ExprLit, ExprRange, Lit, LitInt, RangeLimits, Token,
    };

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

        while let (new, Some(_comman)) = pop_comma(input) {
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

        Ok(dbg!(all))
    }

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

    #[derive(Debug)]
    pub struct RequestedTupleLengths {
        each: Vec<RequestedTupleLength>,
    }

    impl Parse for RequestedTupleLengths {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Self {
                each: Punctuated::<_, Token![,]>::parse_terminated(input)?
                    .into_iter()
                    .collect(),
            })
        }
    }

    impl IntoIterator for RequestedTupleLengths {
        type Item = usize;

        type IntoIter = Box<dyn Iterator<Item = usize>>;

        fn into_iter(self) -> Self::IntoIter {
            Box::new(
                self.each
                    .into_iter()
                    .flat_map(|each| -> Box<dyn Iterator<Item = usize>> {
                        match each {
                            RequestedTupleLength::Just(u) => Box::new(std::iter::once(u)),
                            RequestedTupleLength::Range { start, limits, end } => match limits {
                                RangeLimits::HalfOpen(_) => Box::new(start..end),
                                RangeLimits::Closed(_) => Box::new(start..=end),
                            },
                        }
                    }),
            )
        }
    }

    #[derive(Debug)]
    enum RequestedTupleLength {
        Just(usize),
        Range {
            start: usize,
            limits: RangeLimits,
            end: usize,
        },
    }

    fn parse_usize(input: ParseStream) -> syn::Result<usize> {
        LitInt::parse(input)?.base10_parse()
    }

    fn parse_range(input: ParseStream) -> syn::Result<(usize, RangeLimits, usize)> {
        fn usize_expr(expr: &Expr) -> syn::Result<usize> {
            match expr {
                Expr::Lit(ExprLit {
                    attrs,
                    lit: Lit::Int(lit_int),
                }) => match attrs.is_empty() {
                    true => lit_int.base10_parse(),
                    false => Err(syn::Error::new_spanned(
                        expr,
                        "attributes are not supported",
                    )),
                },
                _ => Err(syn::Error::new_spanned(expr, "must be an integer literal")),
            }
        }

        let expr_range = ExprRange::parse(input)?;
        let ExprRange {
            attrs,
            start,
            limits: _,
            end,
        } = &expr_range;
        if !attrs.is_empty() {
            return Err(syn::Error::new_spanned(
                expr_range,
                "attributes are not supported",
            ));
        }
        let (Some(start), Some(end)) = (start, end) else {
            return Err(syn::Error::new_spanned(expr_range, "range must have a concrete start and end"))
        };
        Ok((usize_expr(start)?, expr_range.limits, usize_expr(end)?))
    }

    impl Parse for RequestedTupleLength {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let lookahead = input.lookahead1();
            match (parse_usize(input), parse_range(input)) {
                (Ok(_), Ok(_)) => unreachable!(),
                (Err(a), Err(b)) => {
                    let mut err = input.error("must be a single number or a range");
                    err.combine(a);
                    err.combine(b);
                    Err(err)
                }
                (_, Ok((start, limits, end))) => Ok(Self::Range { start, limits, end }),
                (Ok(u), _) => Ok(Self::Just(u)),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::parse;
        use proc_macro2::TokenStream;
        use quote::quote;
        use syn::parse_quote;

        use crate::args::RequestedTupleLengths;

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
        fn parse_() {
            do_test(quote!(1), [1]);
            do_test(quote!(1,), [1]);
            do_test(quote!(1..1), []);
            do_test(quote!(1..=1), [1]);
            do_test(quote!(1..2), [1]);
            do_test(quote!(1..=2), [1, 2]);
            do_test(quote!(1, 1..=2), [1, 1, 2]);
        }
        #[test]
        fn test() {
            for token in quote!(9990, 1usize, 1..2, 3..=4) {
                println!("{token:?}");
            }
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
