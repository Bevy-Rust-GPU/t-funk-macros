use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::Parse, punctuated::Punctuated, Attribute, FieldsUnnamed, Generics, Ident, Token,
    Visibility,
};

pub struct StructArg {
    pub ident: Ident,
    pub generics: Generics,
    pub fields: Option<FieldsUnnamed>,
}

impl Parse for StructArg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        let generics = input.parse()?;
        let fields = input.parse().ok();
        Ok(StructArg {
            ident,
            generics,
            fields,
        })
    }
}

pub struct AdtInput {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub struct_token: Token![struct],
    pub target_tys: Punctuated<StructArg, Token![|]>,
    pub semi_token: Token![;],
}

impl Parse for AdtInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse()?;
        let struct_token = input.parse()?;
        let target_tys = Punctuated::parse_separated_nonempty(input)?;
        let semi_token = input.parse()?;

        Ok(AdtInput {
            attrs,
            vis,
            struct_token,
            target_tys,
            semi_token,
        })
    }
}

pub fn impl_define_adt(
    AdtInput {
        attrs,
        vis,
        target_tys,
        ..
    }: AdtInput,
) -> TokenStream {
    let mut out = quote::quote!();

    for StructArg {
        ident,
        generics,
        fields,
    } in target_tys
    {
        out = quote!(
            #out

            #(#attrs)*
            #vis struct #ident #generics #fields;
        );
    }

    //panic!("{out:}");

    out.into()
}
