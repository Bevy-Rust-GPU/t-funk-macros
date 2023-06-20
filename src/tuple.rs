use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, punctuated::Punctuated, token::Comma, Ident};

pub struct Input(Punctuated<Ident, Comma>);

impl Parse for Input {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let punctuated = Punctuated::parse_separated_nonempty(input)?;
        Ok(Input(punctuated))
    }
}

pub fn impl_tuple_foldl(Input(inputs): Input) -> TokenStream {
    let inputs = inputs.into_iter().collect::<Vec<_>>();

    let mut out_type = quote!(_Zero);
    for input in inputs.iter() {
        out_type = quote!(OutputT<_Function, (#out_type, #input)>);
    }

    let mut out_expr = quote!(z);
    for input in inputs.iter() {
        out_expr = quote!(f.clone().call((#out_expr, #input)));
    }

    let mut out_where = quote!(Clone);
    for (i, input) in inputs.iter().rev().enumerate() {
        let mut out_line = quote!(_Zero);
        for input in inputs.iter().take(inputs.len() - i - 1) {
            out_line = quote!(OutputT<_Function, (#out_line, #input)>);
        }
        out_line = quote!((#out_line, #input));
        out_where = quote!(
            #out_where + Closure<#out_line>
        );
    }

    let out = quote!(
        impl<_Function, _Zero, #(#inputs,)*> Foldl<_Function, _Zero> for (#(#inputs,)*)
        where
            _Function: #out_where
        {
            type Foldl = #out_type;

            #[allow(non_snake_case)]
            fn foldl(self, f: _Function, z: _Zero) -> Self::Foldl {
                let (#(#inputs,)*) = self;
                #out_expr
            }
        }
    );

    //panic!("{out:}");

    out.into()
}

pub fn impl_tuple_foldr(Input(inputs): Input) -> TokenStream {
    let inputs = inputs.into_iter().collect::<Vec<_>>();

    let mut out_type = quote!(_Zero);
    for input in inputs.iter().rev() {
        out_type = quote!(OutputT<_Function, (#input, #out_type)>);
    }

    let mut out_expr = quote!(z);
    for input in inputs.iter().rev() {
        out_expr = quote!(f.clone().call((#input, #out_expr)));
    }

    let mut out_where = quote!(Clone);
    for (i, input) in inputs.iter().enumerate() {
        let mut out_line = quote!(_Zero);
        for input in inputs.iter().rev().take(inputs.len() - i - 1) {
            out_line = quote!(OutputT<_Function, (#input, #out_line)>);
        }
        out_line = quote!((#input, #out_line));
        out_where = quote!(
            #out_where + Closure<#out_line>
        );
    }

    let out = quote!(
        impl<_Function, _Zero, #(#inputs,)*> Foldr<_Function, _Zero> for (#(#inputs,)*)
        where
            _Function: #out_where
        {
            type Foldr = #out_type;

            #[allow(non_snake_case)]
            fn foldr(self, f: _Function, z: _Zero) -> Self::Foldr {
                let (#(#inputs,)*) = self;
                #out_expr
            }
        }
    );

    //panic!("{out:}");

    out.into()
}

