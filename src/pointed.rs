use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_pointed(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let pointed = match tys.len() {
        0 => panic!("Can't derive Pointed for a type with no inner."),
        1 => quote!(#(#tys),*),
        _ => quote!((#(#tys),*)),
    };

    let out = quote!(
        impl<#(#tys),*> t_funk::typeclass::pointed::Pointed for #ident<#(#tys),*> {
            type Pointed = #pointed;

            #[allow(non_snake_case)]
            fn point(unit: Self::Pointed) -> Self {
                let #pointed = unit;
                #ident(#(#tys),*)
            }
        }
    );

    //panic!("{out:}");

    out.into()
}
