use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{visit::Visit, Ident, ItemTrait, TraitItemType};

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct TypesVisitor {
    types: Vec<TraitItemType>,
}

impl Visit<'_> for TypesVisitor {
    fn visit_trait_item_type(&mut self, trait_item_type: &TraitItemType) {
        self.types.push(trait_item_type.clone());

        syn::visit::visit_trait_item_type(self, trait_item_type)
    }
}

pub fn impl_types(input: ItemTrait) -> TokenStream {
    let mut out = quote!(#input);

    let mut visitor = TypesVisitor::default();
    visitor.visit_item_trait(&input);

    let trait_ident = input.ident;
    let trait_generics = input.generics.params;

    for ty in visitor.types {
        let ty_ident = ty.ident;

        let alias_name = ty_ident.to_string().to_case(Case::Pascal) + "T";
        let alias_ident = Ident::new(&alias_name, ty_ident.span());

        let ty_params = ty.generics.params;

        let self_ty = Ident::new("_T", Span::call_site());

        let sep_comma_a = if trait_generics.len() > 0 {
            quote!(,)
        } else {
            quote!()
        };

        let sep_comma_b = if ty_params.len() > 0 {
            quote!(,)
        } else {
            quote!()
        };

        out = quote! {
            #out

            pub type #alias_ident < #self_ty #sep_comma_a #trait_generics #sep_comma_b #ty_params > = < #self_ty as #trait_ident < #trait_generics > > :: #ty_ident < #ty_params >;
        }
    }

    //panic!("{out:}");

    out.into()
}
