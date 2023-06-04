use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Data, DeriveInput, Ident};

pub fn impl_lenses(input: DeriveInput) -> TokenStream {
    let mut tokens = TokenStream2::default();

    let Data::Struct(data_struct) = input.data else {
        panic!("Lenses may not be derived for non-Struct types");
    };

    let ident = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    for field in data_struct.fields.iter() {
        let vis = &field.vis;

        let path = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        let path_pascal = path.to_string().to_case(Case::Pascal);

        let getter_name = "_Get".to_string() + &path_pascal;
        let getter = Ident::new(&getter_name, path.span());

        let setter_name = "_Set".to_string() + &path_pascal;
        let setter = Ident::new(&setter_name, path.span());

        tokens = quote! {
            #tokens

            #[derive(
                Debug,
                Default,
                Copy,
                Clone,
                PartialEq,
                Eq,
                PartialOrd,
                Ord,
                Hash,
                type_fields::t_funk::macros::Closure,
                type_fields::t_funk::macros::category::Id,
                type_fields::t_funk::macros::category::Compose,
            )]
            #vis struct #getter;

            impl type_fields::t_funk::function::Function<#ident> for #getter {
                type Output = #ty;

                fn call(t: #ident) -> Self::Output {
                    t.#path
                }
            }

            #[derive(
                Debug,
                Default,
                Copy,
                Clone,
                PartialEq,
                Eq,
                PartialOrd,
                Ord,
                Hash,
                type_fields::t_funk::macros::Closure,
                type_fields::t_funk::macros::category::Id,
                type_fields::t_funk::macros::category::Compose,
            )]
            #vis struct #setter;

            impl type_fields::t_funk::function::Function<(#ident, #ty)> for #setter {
                type Output = #ident;

                fn call((this, #path): (#ident, #ty)) -> Self::Output {
                    #ident {
                        #path,
                        ..this
                    }
                }
            }

            impl #impl_generics #ident #ty_generics #where_clause {
                #[allow(non_upper_case_globals)]
                #vis const #path: type_fields::t_funk::lens::Lens<
                    #getter,
                    #setter,
                > = type_fields::t_funk::lens::lens(
                    #getter,
                    #setter,
                );
            }
        };
    }

    //panic!("{tokens:}");

    tokens.into()
}
