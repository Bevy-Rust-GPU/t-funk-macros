use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    visit_mut::VisitMut, Expr, ExprAssign, Field, GenericArgument, Ident, ItemStruct, Path,
    PathArguments, Type, TypePath,
};

use crate::{
    field_attr_visitor::{AttrConstructor, FieldAttrVisitor},
    generics_visitor::GenericsVisitor,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct NewtypeAttr {
    pub field_ident: Ident,
    pub newtype_path: Path,
    pub type_path: Path,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SetAttr {
    pub field_ident: Ident,
    pub set_path: Path,
    pub newtype_path: Path,
    pub type_path: Path,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Attr {
    Newtype(NewtypeAttr),
    Set(SetAttr),
}

fn attr_newtype(field: &Field, type_path: &TypePath, value: &Expr) -> Vec<Attr> {
    let Expr::Path(expr_path) = value else {
        panic!("Newtypes must be path expressions");
    };

    let field_ident = field.ident.clone().expect("Field is unnamed");
    let newtype_path = type_path.path.clone();
    let type_path = expr_path.path.clone();

    vec![Attr::Newtype(NewtypeAttr {
        field_ident,
        newtype_path,
        type_path,
    })]
}

fn attr_set(field: &Field, type_path: &TypePath, value: &Expr) -> Vec<Attr> {
    let Expr::Array(expr_array) = value else {
        panic!("Sets must be array expressions");
    };

    let type_paths = expr_array
        .elems
        .iter()
        .map(|expr| {
            let Expr::Assign(ExprAssign {
                                        left, right, ..
                                    }) = expr else {
                                        panic!("Newtypes must be assignment expressions");
                                    };

            let Expr::Path(key) = *left.clone() else {
                                        panic!("Assignment LHS must be a path");
                                    };

            let Expr::Path(value) = *right.clone() else {
                                        panic!("Assignment LHS must be a path");
                                    };

            (key.path.clone(), value.path.clone())
        })
        .collect::<Vec<_>>();

    let field_ident = field.ident.as_ref().expect("Field is unnamed");
    let set_path = &type_path.path;

    type_paths
        .into_iter()
        .map(move |(newtype_path, type_path)| {
            let field_ident = field_ident.clone();
            let set_path = set_path.clone();
            Attr::Set(SetAttr {
                field_ident,
                set_path,
                newtype_path,
                type_path,
            })
        })
        .collect::<Vec<_>>()
}

pub fn impl_set(mut item_struct: ItemStruct) -> TokenStream {
    let mut field_visitor = FieldAttrVisitor {
        constructors: [
            ("newtype", Box::new(attr_newtype) as AttrConstructor<Attr>),
            ("set", Box::new(attr_set) as AttrConstructor<Attr>),
        ]
        .into_iter()
        .collect(),
        ..Default::default()
    };
    field_visitor.visit_item_struct_mut(&mut item_struct);
    let set_attrs = field_visitor.outputs;

    let struct_ident = item_struct.ident.clone();
    let struct_generics = item_struct.generics.clone();

    // Vec of struct generics for quote destructuring
    let struct_generics_list = struct_generics.params.iter().cloned().collect::<Vec<_>>();

    // Vec of generics replaced with () for Empty impl
    let empty_generics = struct_generics_list
        .iter()
        .map(|_| quote!(()))
        .collect::<Vec<_>>();

    // List of field idents
    let field_idents = item_struct
        .fields
        .iter()
        .map(|field| field.ident.as_ref().unwrap().clone())
        .collect::<Vec<_>>();

    // List of field types
    let field_tys = item_struct
        .fields
        .iter()
        .map(|field| field.ty.clone())
        .collect::<Vec<_>>();

    let mut out = quote!(#item_struct);

    let t_ident = Ident::new("_T", Span::call_site());

    for attr in &set_attrs {
        let (field_ident, newtype_path, type_path) = match attr {
            Attr::Newtype(NewtypeAttr {
                field_ident,
                newtype_path,
                type_path,
            })
            | Attr::Set(SetAttr {
                field_ident,
                newtype_path,
                type_path,
                ..
            }) => (field_ident, newtype_path, type_path),
        };

        // Struct generics with current field T replaced with Newtype<T>
        let struct_newtype_generics = GenericsVisitor {
            from: newtype_path.get_ident().unwrap().clone(),
            to: quote!(#type_path),
            generics: Default::default(),
        }
        .visit(&struct_generics);

        // Struct generics with current field T replaced with ()
        let struct_removed_generics = GenericsVisitor {
            from: newtype_path.get_ident().unwrap().clone(),
            to: quote!(()),
            generics: Default::default(),
        }
        .visit(&struct_generics);

        // Struct generics with current field T replaced with Newtype<_T>
        let t_value = {
            let mut t_value = type_path.clone();
            let PathArguments::AngleBracketed(args) = &mut t_value.segments[0].arguments else {
                        panic!("Arguments are not angle-bracketed");
                    };

            let GenericArgument::Type(Type::Path(type_path)) = &mut args.args[0] else {
                        panic!("Generic argument is not a type path");
                    };

            let seg = &mut type_path.path.segments[0];
            seg.ident = t_ident.clone();

            t_value
        };

        let struct_t_generics = GenericsVisitor {
            from: newtype_path.get_ident().unwrap().clone(),
            to: quote!(#t_value),
            generics: Default::default(),
        }
        .visit(&struct_generics);

        // List of field idents minus the current field
        let field_idents_filtered = field_idents
            .iter()
            .filter(|a| *a != field_ident)
            .collect::<Vec<_>>();

        // Get / Insert / Remove impls
        match attr {
            Attr::Newtype(NewtypeAttr { .. }) => {
                out = quote!(
                    #out

                    impl < #(#struct_generics_list),* > t_funk::collection::set::Get<#type_path> for #struct_ident < #(#struct_newtype_generics),* > {
                        fn get(self) -> #type_path {
                            self.#field_ident
                        }
                    }

                    impl < #t_ident, #(#struct_generics_list),* > t_funk::collection::set::Insert<#t_value> for #struct_ident <#(#struct_generics_list),*> {
                        type Insert = #struct_ident < #(#struct_t_generics),* >;

                        fn insert(self, #field_ident: #t_value) -> Self::Insert {
                            let #struct_ident { #(#field_idents_filtered,)* .. } = self;

                            #struct_ident {
                                #(#field_idents),*
                            }
                        }
                    }

                    impl < #(#struct_generics_list),* > t_funk::collection::set::Remove<#type_path> for #struct_ident < #(#struct_newtype_generics),* > {
                        type Remove = #struct_ident < #(#struct_removed_generics),* >;

                        fn remove(self) -> (Self::Remove, #type_path) {
                            let #struct_ident { #(#field_idents),* } = self;
                            (#struct_ident { #field_ident: (), #(#field_idents_filtered),* }, #field_ident)
                        }
                    }
                );
            }
            Attr::Set(SetAttr { .. }) => {
                out = quote!(
                    #out

                    impl < #(#struct_generics_list),* > t_funk::collection::set::Get<#type_path> for #struct_ident < #(#struct_newtype_generics),* > {
                        fn get(self) -> #type_path {
                            self.#field_ident.get()
                        }
                    }

                    impl < #t_ident, #(#struct_generics_list),* > t_funk::collection::set::Insert<#t_value> for #struct_ident <#(#struct_generics_list),*> {
                        type Insert = #struct_ident < #(#struct_t_generics),* >;

                        fn insert(self, val: #t_value) -> Self::Insert {
                            let #struct_ident {
                                #(#field_idents),*
                            } = self;

                            #struct_ident {
                                #field_ident: #field_ident.insert(val),
                                #(#field_idents_filtered),*
                            }
                        }
                    }

                    impl < #(#struct_generics_list),* > t_funk::collection::set::Remove<#type_path> for #struct_ident < #(#struct_newtype_generics),* > {
                        type Remove = #struct_ident < #(#struct_removed_generics),* >;

                        fn remove(self) -> (Self::Remove, #type_path) {
                            let #struct_ident {
                                #(#field_idents),*
                            } = self;

                            let (#field_ident, val) = #field_ident.remove();
                            (#struct_ident {
                                #field_ident,
                                #(#field_idents_filtered),*
                            }, val)
                        }
                    }
                );
            }
        }
    }

    // Empty impl
    let mut empties = Vec::<TokenStream2>::default();
    for field in &field_idents {
        let attr = set_attrs
            .iter()
            .find(|attr| match attr {
                Attr::Newtype(NewtypeAttr { field_ident, .. }) => return field_ident == field,
                Attr::Set(SetAttr { field_ident, .. }) => return field_ident == field,
            })
            .unwrap();
        match attr {
            Attr::Newtype(NewtypeAttr { field_ident, .. }) => {
                empties.push(quote!(#field_ident: ()))
            }
            Attr::Set(SetAttr {
                field_ident,
                set_path,
                ..
            }) => empties
                .push(quote!(#field_ident: <#set_path as t_funk::collection::set::Empty>::empty())),
        }
    }

    let out = quote!(
        #out

        impl < #(#struct_generics_list),* > t_funk::collection::set::Empty for #struct_ident < #(#struct_generics_list),* > {
            type Empty = #struct_ident < #(#empty_generics),* >;

            fn empty() -> Self::Empty {
                #struct_ident {
                    #(#empties),*
                }
            }
        }
    );

    // UnionWith impl
    let mut where_clause = quote!(where);
    let mut target = quote!(_U);
    for ty in field_tys.iter() {
        let bound = quote!(t_funk::collection::set::Insert<#ty>);

        where_clause = quote!(#where_clause #target: #bound,);

        target = quote!(t_funk::collection::set::InsertT<#target, #ty>);
    }

    let out = quote!(
        #out

        impl < _U, #(#struct_generics_list),* > t_funk::collection::set::UnionWith<_U> for #struct_ident < #(#struct_generics_list),* > #where_clause {
            type UnionWith = #target;

            fn union_with(self, u: _U) -> Self::UnionWith {
                let out = u;
                #(
                    let out = t_funk::collection::set::Insert::<#field_tys>::insert(out, self.#field_idents);
                )*
                out
            }
        }
    );

    // SubtractFrom impl
    let mut where_clause = quote!(where);
    let mut target = quote!(_U);
    for attr in set_attrs.iter() {
        let type_path = match attr {
            Attr::Newtype(NewtypeAttr { newtype_path, .. }) => newtype_path,
            Attr::Set(SetAttr { newtype_path, .. }) => newtype_path,
        };

        let bound = quote!(t_funk::collection::set::Drop<#type_path>);

        where_clause = quote!(#where_clause #target: #bound,);

        target = quote!(t_funk::collection::set::DropT<#target, #type_path>);
    }

    let mut drops = quote!(u);
    for attr in set_attrs.iter() {
        let type_path = match attr {
            Attr::Newtype(NewtypeAttr { newtype_path, .. }) => newtype_path,
            Attr::Set(SetAttr { newtype_path, .. }) => newtype_path,
        };
        drops = quote!( t_funk::collection::set::Drop::<#type_path>::drop(#drops) );
    }

    let out = quote!(
        #out

        impl < _U, #(#struct_generics_list),* > t_funk::collection::set::SubtractFrom<_U> for #struct_ident < #(#struct_generics_list),* > #where_clause {
            type SubtractFrom = #target;

            fn subtract_from(self, u: _U) -> Self::SubtractFrom {
                #drops
            }
        }
    );

    //panic!("{out:}");

    out.into()
}
