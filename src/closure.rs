use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_closure(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let out = quote!(impl<_Input, #(#tys),*> t_funk::closure::Closure<_Input> for #ident < #(#tys),* > where #ident < #(#tys),* >: t_funk::function::Function<_Input> {
        type Output = <#ident < #(#tys),* > as t_funk::function::Function<_Input>>::Output;

        fn call(self, input: _Input) -> Self::Output {
            <#ident < #(#tys),* > as t_funk::function::Function<_Input>>::call(input)
        }
    });

    //panic!("{out:}");

    out.into()
}
