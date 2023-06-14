use std::collections::BTreeMap;

use syn::{visit_mut::VisitMut, Attribute, Expr, Field, Meta, MetaNameValue, Type, TypePath};

pub type AttrConstructor<O> = Box<dyn Fn(&Field, &TypePath, &Expr) -> Vec<O>>;

// Visit the fields of the provided AST and convert matching attributes
// into a list of data via constructor function
pub struct FieldAttrVisitor<O> {
    pub constructors: BTreeMap<&'static str, AttrConstructor<O>>,
    pub outputs: Vec<O>,
}

impl<O> Default for FieldAttrVisitor<O> {
    fn default() -> Self {
        Self {
            constructors: Default::default(),
            outputs: Default::default(),
        }
    }
}

impl<O> VisitMut for FieldAttrVisitor<O> {
    fn visit_field_mut(&mut self, field: &mut Field) {
        field.attrs = field
            .attrs
            .iter()
            .cloned()
            .filter(
                |Attribute {
                     pound_token: _,
                     style: _,
                     bracket_token: _,
                     meta,
                 }: &Attribute| {
                    let Meta::NameValue(MetaNameValue {
                    path,
                    eq_token: _,
                    value,
                }) = meta else {
                    return true;
                };

                    let Some(ident) = path.get_ident() else {
                    return true;
                };

                    let key = ident.to_string();

                    let Type::Path(type_path) = &field.ty else {
                    panic!("Set types must be path types");
                };

                    if let Some(c) = self.constructors.get(key.as_str()) {
                        self.outputs.extend(c(field, type_path, value));
                        return false;
                    }

                    true
                },
            )
            .collect();

        syn::visit_mut::visit_field_mut(self, field)
    }
}

