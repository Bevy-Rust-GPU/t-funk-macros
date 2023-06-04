use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_phantom_clone(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let out = quote!(impl<#(#tys),*> Clone for #ident < #(#tys),* > {
        fn clone(self) -> Self {
            Self(Default::default())
        }
    });

    //panic!("{out:}");

    out.into()
}

