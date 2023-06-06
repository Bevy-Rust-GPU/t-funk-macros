use proc_macro::TokenStream;
use syn::{
    fold::Fold, parse::Parse, punctuated::Punctuated, AngleBracketedGenericArguments,
    Block, Ident, Token, WhereClause,
};

pub struct IdentArgs {
    ident: Ident,
    args: Option<AngleBracketedGenericArguments>,
}

impl Parse for IdentArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        let args = input.parse().ok();
        Ok(IdentArgs { ident, args })
    }
}

pub struct AdtInput {
    _impl_token: Token![impl],
    generics: Option<AngleBracketedGenericArguments>,
    trait_ty: IdentArgs,
    _for_token: Token![for],
    target_tys: Punctuated<IdentArgs, Token![|]>,
    where_clause: Option<WhereClause>,
    block: Block,
}

impl Parse for AdtInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _impl_token = input.parse()?;
        let generics = input.parse().ok();
        let trait_ty = input.parse()?;
        let _for_token = input.parse()?;
        let target_tys = Punctuated::parse_separated_nonempty(input)?;
        let where_clause = input.parse().ok();
        let block = input.parse()?;

        Ok(AdtInput {
            _impl_token,
            generics,
            trait_ty,
            _for_token,
            target_tys,
            where_clause,
            block,
        })
    }
}

struct ThisVisitor {
    ident: Ident,
}

impl Fold for ThisVisitor {
    fn fold_path_segment(&mut self, mut i: syn::PathSegment) -> syn::PathSegment {
        if i.ident.to_string() == "This" {
            i.ident = self.ident.clone();
        }
        i
    }
}

pub fn impl_impl_adt(
    AdtInput {
        trait_ty:
            IdentArgs {
                ident: trait_ident,
                args: trait_args,
            },
        generics,
        target_tys,
        where_clause,
        block,
        ..
    }: AdtInput,
) -> TokenStream {
    let generics = if let Some(generics) = generics {
        generics.args.iter().cloned().collect::<Vec<_>>()
    } else {
        vec![]
    };

    let trait_args = if let Some(trait_args) = trait_args {
        trait_args.args.iter().cloned().collect::<Vec<_>>()
    } else {
        vec![]
    };

    let mut out = quote::quote!();

    for IdentArgs {
        ident: ty_ident,
        args: ty_args,
    } in target_tys
    {
        let ty_args = if let Some(ty_args) = ty_args {
            ty_args.args.iter().cloned().collect::<Vec<_>>()
        } else {
            vec![]
        };

        let generics = generics
            .iter()
            .cloned()
            .filter(|t| trait_args.contains(t) || ty_args.contains(t))
            .collect::<Vec<_>>();

        let block = ThisVisitor {
            ident: ty_ident.clone(),
        }
        .fold_block(block.clone());

        out = quote::quote!(
            #out

            impl < #(#generics),* > #trait_ident < #(#trait_args),* > for #ty_ident < #(#ty_args),* >
            #where_clause
            #block
        );
    }

    //panic!("{out:}");

    out.into()
}
