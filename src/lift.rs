use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::quote;
use syn::{FnArg, Ident, ItemFn};

pub fn impl_lift(item_fn: ItemFn) -> TokenStream {
    let out = quote!(#item_fn);

    let vis = item_fn.vis;
    let sig = item_fn.sig;

    let fn_ident = sig.ident;
    let fn_span = fn_ident.span();
    let struct_name = fn_ident.to_string().replace("r#", "").to_case(Case::Pascal);
    let struct_ident = Ident::new(&struct_name, fn_span);

    let (impl_generics, _, where_clause) = sig.generics.split_for_impl();

    let (input_pats, input_tys) = sig
        .inputs
        .into_iter()
        .flat_map(|arg| {
            let FnArg::Typed(pat_ty) = arg else { return None };
            Some((pat_ty.pat, pat_ty.ty))
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    let output = match sig.output {
        syn::ReturnType::Default => quote!(()),
        syn::ReturnType::Type(_, ty) => quote!(#ty),
    };

    let out = quote!(
        #out

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
            t_funk::macros::Closure,
            t_funk::macros::category::Category,
            t_funk::macros::arrow::Arrow,
        )]
        #vis struct #struct_ident;

        impl #impl_generics t_funk::function::Function<(#(#input_tys),*)> for #struct_ident #where_clause {
            type Output = #output;

            #[allow(non_snake_case)]
            fn call((#(#input_pats),*): (#(#input_tys),*)) -> Self::Output {
                #fn_ident(#(#input_pats),*)
            }
        }
    );

    //panic!("{out:}");

    out.into()
}
