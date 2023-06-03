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

#[doc(hidden)]
#[proc_macro]
pub fn _impl_contextless_one_of_parser_sequence_for_tuples(
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let arg = parse_macro_input!(item as RangeArg);

    arg.into_iter()
        .map(|num_tuples| {
            // Parser0, Parser1 ...
            let parser_ty_param = (0..num_tuples)
                .map(|n| Ident::new(&format!("Parser{n}"), Span::call_site()))
                .collect::<Vec<_>>();
            let tuple_ix = (0..num_tuples).map(Literal::usize_unsuffixed);
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
                        let mut all_go_ons = Vec::new();
                        let mut error = true;
                        #(
                            match self.#tuple_ix.parse_contextless(input) {
                                Ok(o) => return Ok(o),
                                Err(UpError::GoOn { go_on, .. }) => {
                                    all_go_ons.extend(go_on);
                                    error = false;
                                },
                                Err(UpError::Oops { .. }) => {}
                            }
                        )*
                        match error {
                            true => Err(oops(input, "no branches could continue").no_ctx()),
                            false => Err(go_on(all_go_ons).no_ctx()),
                        }
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
            // Parser0, Parser1 ...
            let parser_ty_param = (0..num_tuples)
                .map(|n| Ident::new(&format!("Parser{n}"), Span::call_site()))
                .collect::<Vec<_>>();
            let tuple_ix = (0..num_tuples).map(Literal::usize_unsuffixed);
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
                        let mut all_go_ons = Vec::new();
                        let mut error = true;
                        #(
                            match self.#tuple_ix.parse_contextual(input, ctx) {
                                Ok(o) => return Ok(o),
                                Err(UpError::GoOn {
                                    go_on,
                                    ctx: new_ctx,
                                }) => {
                                    all_go_ons.extend(go_on);
                                    error = false;
                                    ctx = new_ctx
                                }
                                Err(UpError::Oops { ctx: new_ctx, .. }) => ctx = new_ctx,
                            }
                        )*
                        match error {
                            true => Err(oops(input, "no branches could continue").ctx(ctx)),
                            false => Err(go_on(all_go_ons).ctx(ctx)),
                        }
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
            // Parser0, Parser1 ...
            let parser_ty_param = (0..num_tuples)
                .map(|n| Ident::new(&format!("Parser{n}"), Span::call_site()))
                .collect::<Vec<_>>();
            // Out0, Out1 ...
            let out_ty_param = (0..num_tuples)
                .map(|n| Ident::new(&format!("Out{n}"), Span::call_site()))
                .collect::<Vec<_>>();
            let parser_bound = parser_ty_param.iter().zip(out_ty_param.iter()).map(|(parser_ty_param, out_ty_param)|{
                quote!{ #parser_ty_param: ContextlessUpParser<'input, #out_ty_param> }
            });
            let tuple_ix = (0..num_tuples).map(Literal::usize_unsuffixed);
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
                    #(#parser_bound,)*
                {
                    fn contextless_series(
                        &self,
                        mut input: &'input str,
                    ) -> ContextlessUpResult<'input, (
                        #(#out_ty_param,)*
                    )> {
                        let mut final_could_also = Vec::new();
                        let yeses = (
                            #({
                                let YesAnd {
                                    yes, and, could_also, ctx: _
                                } = self.#tuple_ix.parse_contextless(input)?;
                                input = and;
                                final_could_also = could_also;
                                yes
                            },)*
                        );
                        Ok(yes_and(yeses, input).also(final_could_also).no_ctx())
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
            // Parser0, Parser1 ...
            let parser_ty_param = (0..num_tuples)
                .map(|n| Ident::new(&format!("Parser{n}"), Span::call_site()))
                .collect::<Vec<_>>();
            // Out0, Out1 ...
            let out_ty_param = (0..num_tuples)
                .map(|n| Ident::new(&format!("Out{n}"), Span::call_site()))
                .collect::<Vec<_>>();
            let parser_bound = parser_ty_param.iter().zip(out_ty_param.iter()).map(|(parser_ty_param, out_ty_param)|{
                quote!{ #parser_ty_param: ContextualUpParser<'input, #out_ty_param, Ctx> }
            });
            let tuple_ix = (0..num_tuples).map(Literal::usize_unsuffixed);
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
                    #(#parser_bound,)*
                {
                    fn contextual_series(
                        &self,
                        mut input: &'input str,
                        mut ctx: Ctx,
                    ) -> UpResult<'input, (
                        #(#out_ty_param,)*
                    ), Ctx> {
                        let mut final_could_also = Vec::new();
                        let yeses = (#({
                            let YesAnd {
                                yes,
                                and,
                                could_also,
                                ctx: new_ctx,
                            } = self.#tuple_ix.parse_contextual(input, ctx)?;
                            input = and;
                            ctx = new_ctx;
                            final_could_also = could_also;
                            yes
                        },)*);
                        Ok(yes_and(yeses, input).also(final_could_also).ctx(ctx))
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
