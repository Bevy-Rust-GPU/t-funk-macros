use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_quote, token::Where, visit_mut::VisitMut, GenericParam, Ident, ItemTrait, Pat, PatIdent,
    Path, PathArguments, PathSegment, TraitItemFn, Type, TypePath, WhereClause, WherePredicate,
};

struct FunctionsVisitor;

impl VisitMut for FunctionsVisitor {
    fn visit_path_segment_mut(&mut self, path_segment: &mut PathSegment) {
        if path_segment.ident.to_string() == "Self" {
            path_segment.ident = Ident::new("_Type", Span::call_site())
        }

        syn::visit_mut::visit_path_segment_mut(self, path_segment)
    }
}

pub fn impl_functions(input: ItemTrait) -> TokenStream {
    let ident_trait = &input.ident;
    let gen = input.generics.params.iter().cloned().collect::<Vec<_>>();
    let (_, _, _where_clause) = input.generics.split_for_impl();

    let impls = input
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Fn(f) => Some(f),
            _ => None,
        })
        .flat_map(|f| impl_function(ident_trait, gen.clone(), f))
        .collect::<Vec<_>>();

    let out = quote! {
        #input

        #(
            #impls
        )*
    }
    .into();

    //panic!("{out:}");

    out
}

fn impl_function(
    ident_trait: &Ident,
    gen: Vec<GenericParam>,
    f: &TraitItemFn,
) -> Option<TokenStream2> {
    let ident_fn = f.sig.ident.to_string();
    let ident_struct = ident_fn.to_case(Case::Pascal) + "F";
    let ident_struct = Ident::new(&ident_struct, f.sig.ident.span());
    let ident_fn = Ident::new(&ident_fn, f.sig.ident.span());

    let mut sig = f.sig.clone();
    FunctionsVisitor.visit_signature_mut(&mut sig);

    let mut input_pats = vec![];
    let mut input_tys = vec![];

    for input in sig.inputs.iter() {
        match input {
            syn::FnArg::Receiver(_) => {
                input_pats.push(Pat::Ident(PatIdent {
                    ident: Ident::new("_type", Span::call_site()),
                    attrs: vec![],
                    by_ref: None,
                    mutability: None,
                    subpat: None,
                }));

                input_tys.push(Type::Path(TypePath {
                    qself: None,
                    path: Path {
                        leading_colon: None,
                        segments: vec![PathSegment {
                            ident: Ident::new("_Type", Span::call_site()),
                            arguments: PathArguments::None,
                        }]
                        .into_iter()
                        .collect(),
                    },
                }))
            }
            syn::FnArg::Typed(ty) => {
                input_pats.push(*ty.pat.clone());
                input_tys.push(*ty.ty.clone());
            }
        }
    }

    let gen_types = gen
        .iter()
        .filter_map(|gen| match gen {
            syn::GenericParam::Type(type_param) => Some(type_param.ident.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    let mut phantom_types = gen_types
        .iter()
        .cloned()
        .filter(|ident| {
            input_tys.iter().all(|ty| match ty {
                Type::Path(type_path) => {
                    (type_path.path.get_ident() != Some(ident)) || input_tys.len() == 0
                }
                _ => false,
            })
        })
        .collect::<Vec<_>>();

    if input_tys.len() == 0 {
        phantom_types = vec![Ident::new("_Type", Span::call_site())];
    }

    let struct_body = if phantom_types.len() == 0 {
        quote!()
    } else {
        quote!(<#(#phantom_types,)*>(core::marker::PhantomData<(#(#phantom_types,)*)>))
    };

    let input_vals = match input_pats.len() {
        0 => quote!(),
        _ => quote!(#(#input_pats),*),
    };

    let input_pats = match input_pats.len() {
        0 => quote!(_),
        1 => quote!(#(#input_pats),*),
        _ => quote!((#(#input_pats),*)),
    };

    let input_tys = match input_tys.len() {
        0 => quote!(()),
        1 => quote!(#(#input_tys),*),
        _ => quote!((#(#input_tys),*)),
    };

    let outputs = match &sig.output {
        syn::ReturnType::Default => syn::Type::Verbatim(quote!(())),
        syn::ReturnType::Type(_, ty) => *ty.clone(),
    };

    let (_, _, where_clause) = f.sig.generics.split_for_impl();

    let predicate: WherePredicate = parse_quote!(_Type: #ident_trait<#(#gen),*>);
    let mut predicates = vec![predicate];
    if let Some(where_clause) = where_clause.cloned() {
        predicates.extend(where_clause.predicates.into_iter().map(|mut predicate| {
            match &mut predicate {
                WherePredicate::Type(ty) => {
                    FunctionsVisitor.visit_predicate_type_mut(ty);
                    predicate
                }
                _ => predicate,
            }
        }));
    }

    let where_clause = WhereClause {
        where_token: Where(Span::call_site()),
        predicates: predicates.into_iter().collect(),
    };

    let fn_generics = quote!();

    let derives = if struct_body.is_empty() {
        quote!(
            #[derive(
                Debug,
                Default,
                Copy,
                Clone, PartialEq,
                Eq,
                PartialOrd,
                Ord,
                Hash,
                type_fields::t_funk::macros::Closure,
                type_fields::t_funk::macros::category::Category,
                type_fields::t_funk::macros::arrow::Arrow,
            )]
        )
    } else {
        quote!(
            #[derive(
                Debug,
                type_fields::t_funk::macros::Closure,
                type_fields::t_funk::macros::category::Category,
                type_fields::t_funk::macros::arrow::Arrow,
            )]
        )
    };

    let phantom_derives = if struct_body.is_empty() {
        quote!()
    } else {
        quote! {
            impl<_Type> Default for #ident_struct<_Type> {
                fn default() -> Self {
                    #ident_struct(core::marker::PhantomData)
                }
            }

            impl<_Type> Clone for #ident_struct<_Type> {
                fn clone(&self) -> Self {
                    #ident_struct(core::marker::PhantomData)
                }
            }

            impl<_Type> Copy for #ident_struct<_Type> {}
        }
    };

    Some(quote!(
        #derives
        pub struct #ident_struct #struct_body;

        #phantom_derives

        impl<#(#gen,)* _Type> type_fields::t_funk::function::Function<#input_tys> for #ident_struct < #(#phantom_types),* >
        #where_clause
        {
            type Output = #outputs;

            #[allow(non_snake_case)]
            fn call(#input_pats : #input_tys) -> Self::Output {
                <_Type as #ident_trait<#(#gen),*>>::#ident_fn #fn_generics (#input_vals)
            }
        }

        //type_fields::derive_closure!(#ident_struct < #(#phantom_types),* >);
    ))
}
