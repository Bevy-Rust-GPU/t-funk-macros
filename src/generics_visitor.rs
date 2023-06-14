use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{visit::Visit, GenericParam, Generics, Ident};

/// Visit generics and use them to construct a list of token streams,
/// comparing against an ident and replacing matches with a provided token stream
#[derive(Debug, Clone)]
pub struct GenericsVisitor {
    pub from: Ident,
    pub to: TokenStream2,
    pub generics: Vec<TokenStream2>,
}

impl GenericsVisitor {
    pub fn visit(mut self, t: &Generics) -> Vec<TokenStream2> {
        self.visit_generics(t);
        self.generics
    }
}

impl Visit<'_> for GenericsVisitor {
    fn visit_generic_param(&mut self, generic_param: &GenericParam) {
        match &generic_param {
            GenericParam::Type(ref ty) => {
                if ty.ident == self.from {
                    self.generics.push(self.to.clone())
                } else {
                    self.generics.push(quote!(#generic_param))
                }
            }
            _ => self.generics.push(quote!(#generic_param)),
        }

        syn::visit::visit_generic_param(self, generic_param)
    }
}

