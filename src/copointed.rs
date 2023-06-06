use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_copointed(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let copointed = match tys.len() {
        0 => {
            return quote!(
                impl t_funk::typeclass::copointed::Copointed for #ident {
                    type Copointed = Self;

                    fn copoint(self) -> Self::Copointed {
                        self
                    }
                }
            )
            .into()
        }
        1 => quote!(#(#tys),*),
        _ => quote!((#(#tys),*)),
    };

    let out = quote!(
        impl<#(#tys),*> t_funk::typeclass::copointed::Copointed for #ident<#(#tys),*> {
            type Copointed = #copointed;

            #[allow(non_snake_case)]
            fn copoint(self) -> Self::Copointed {
                let #ident(#(#tys),*) = self;
                #copointed
            }
        }
    );

    //panic!("{out:}");

    out.into()
}
