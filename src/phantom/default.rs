use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_phantom_default(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let out = quote!(impl<#(#tys),*> Default for #ident < #(#tys),* > {
        fn default() -> Self {
            Self(Default::default())
        }
    });

    //panic!("{out:}");

    out.into()
}

