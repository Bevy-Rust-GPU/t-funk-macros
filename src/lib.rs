extern crate proc_macro;

mod closure;
mod copointed;
mod functions;
mod lenses;
mod phantom;
mod pointed;

use proc_macro::TokenStream;
use syn::parse_macro_input;

macro_rules! newtype_derive {
    ($derive:ident :: $fn:ident (#$ident:ident, #$ty:ident) => { $($tt:tt)* }) => {
        #[proc_macro_derive($derive)]
        pub fn $fn(input: TokenStream) -> TokenStream {
            let input: syn::DeriveInput = parse_macro_input!(input);
            let $ident = input.ident;

            let $ty = input
                .generics
                .type_params()
                .map(|type_param| type_param.ident.clone())
                .collect::<Vec<_>>();

            let $ty = quote::quote!(#(#$ty),*);

            let out = quote::quote!(
                $($tt)*
            );

            //panic!("{out:}");

            out.into()
        }
    }
}

/// For each field in the annotated struct:
/// - Implement getter / setter Function types
/// - Define a const representing the lens over said getter / setter
#[proc_macro_derive(Lenses)]
pub fn lenses(input: TokenStream) -> TokenStream {
    lenses::impl_lenses(parse_macro_input!(input))
}

/// Implements Default for a PhantomData wrapper
#[proc_macro_derive(PhantomDefault)]
pub fn phantom_default(input: TokenStream) -> TokenStream {
    phantom::default::impl_phantom_default(parse_macro_input!(input))
}

/// Implements Clone for a PhantomData wrapper
#[proc_macro_derive(PhantomClone)]
pub fn phantom_clone(input: TokenStream) -> TokenStream {
    phantom::clone::impl_phantom_clone(parse_macro_input!(input))
}

/// Implements Copy for a PhantomData wrapper
#[proc_macro_derive(PhantomCopy)]
pub fn phantom_copy(input: TokenStream) -> TokenStream {
    phantom::copy::impl_phantom_copy(parse_macro_input!(input))
}

/// Create a struct and corresponding `Function` implementation
/// for each function in the  provided trait.
/// Created structs are named by Pascal-casing their function name,
/// and appending F.
/// ex. my_func becomes MyFuncF.
#[proc_macro_attribute]
pub fn functions(_: TokenStream, input: TokenStream) -> TokenStream {
    functions::impl_functions(parse_macro_input!(input))
}

/// Derive `Closure<T>` for a type that implements `Function<T>`.
#[proc_macro_derive(Closure)]
pub fn closure(input: TokenStream) -> TokenStream {
    closure::impl_closure(parse_macro_input!(input))
}

/// Derive `Pointed` for a newtype.
#[proc_macro_derive(Pointed)]
pub fn pointed(input: TokenStream) -> TokenStream {
    pointed::impl_pointed(parse_macro_input!(input))
}

/// Derive `Copointed` for a newtype.
#[proc_macro_derive(Copointed)]
pub fn copointed(input: TokenStream) -> TokenStream {
    copointed::impl_copointed(parse_macro_input!(input))
}

#[proc_macro_derive(Functor)]
pub fn functor(input: TokenStream) -> TokenStream {
    let fmap = fmap(input.clone());
    let replace = replace(input);
    fmap.into_iter().chain(replace.into_iter()).collect()
}

// Derive `Fmap` for a newtype.
newtype_derive! {
    Fmap::fmap(#ident, #ty) => {
        impl<_Function, #ty> t_funk::typeclass::functor::Fmap<_Function> for #ident<#ty>
        where
            _Function: t_funk::closure::Closure<#ty>,
        {
            type Fmap = #ident<_Function::Output>;

            #[allow(non_snake_case)]
            fn fmap(self, f: _Function) -> Self::Fmap {
                let #ident(#ty) = self;
                #ident(f.call(#ty))
            }
        }
    }
}

// Derive `Replace` for a newtype.
newtype_derive! {
    Replace::replace(#ident, #ty) => {
        impl<#ty, U> t_funk::typeclass::functor::Replace<U> for #ident<#ty>
        where
            #ident<#ty>: t_funk::typeclass::functor::Fmap<t_funk::closure::Curry2A<t_funk::function::Const, U>>,
        {
            type Replace = <#ident<#ty> as t_funk::typeclass::functor::Fmap<t_funk::closure::Curry2A<t_funk::function::Const, U>>>::Fmap;

            fn replace(self, t: U) -> Self::Replace {
                t_funk::typeclass::functor::Fmap::fmap(self, t_funk::closure::Curry2::prefix2(t_funk::function::Const, t))
            }
        }
    }
}

#[proc_macro_derive(Applicative)]
pub fn applicative(input: TokenStream) -> TokenStream {
    let pure = pure(input.clone());
    let apply = apply(input);
    pure.into_iter().chain(apply.into_iter()).collect()
}

// Derive `Pure` for a newtype.
newtype_derive! {
    Pure::pure(#ident, #ty) => {
        impl<#ty> t_funk::typeclass::applicative::Pure for #ident<#ty>
        {
            type Pure<U> = #ident<U>;

            fn pure<U>(t: U) -> Self::Pure<U> {
                #ident(t)
            }
        }
    }
}

// Derive `Apply` for a newtype.
newtype_derive! {
    Apply::apply(#ident, #ty) => {
        impl<#ty, _Value> t_funk::typeclass::applicative::Apply<#ident<_Value>> for #ident<#ty>
        where
            #ty: t_funk::closure::Closure<_Value>,
        {
            type Apply = #ident<#ty::Output>;

            #[allow(non_snake_case)]
            fn apply(self, a: #ident<_Value>) -> Self::Apply
            where
                #ty: t_funk::closure::Closure<_Value>,
            {
                let #ident(#ty) = self;
                let #ident(_Value) = a;
                #ident(
                    #ty.call(_Value),
                )
            }
        }
    }
}

#[proc_macro_derive(Monad)]
pub fn monad(input: TokenStream) -> TokenStream {
    let chain = chain(input.clone());
    let then = then(input);
    chain.into_iter().chain(then.into_iter()).collect()
}

// Derive `Chain` for a newtype.
newtype_derive! {
    Chain::chain(#ident, #ty) => {
        impl<#ty, _Function> t_funk::typeclass::monad::Chain<_Function> for #ident<#ty>
        where
            _Function: t_funk::closure::Closure<#ty>,
        {
            type Chain = _Function::Output;

            #[allow(non_snake_case)]
            fn chain(self, f: _Function) -> Self::Chain {
                let #ident(#ty) = self;
                f.call(#ty)
            }
        }
    }
}

// Derive `Then` for a newtype.
/*
newtype_derive! {
    Then::then(#ident, #ty) => {
        impl<#ty, _Function> t_funk::typeclass::monad::Then<_Function> for #ident<#ty>
        where
            #ident<#ty>: t_funk::typeclass::functor::Replace<t_funk::function::Id>,
            <#ident<#ty> as t_funk::typeclass::functor::Replace<t_funk::function::Id>>::Replace: t_funk::typeclass::applicative::Apply<_Function>,
        {
            type Then = <<#ident<#ty> as t_funk::typeclass::functor::Replace<t_funk::function::Id>>::Replace as t_funk::typeclass::applicative::Apply<_Function>>::Apply;

            fn then(self, f: _Function) -> Self::Then {
               t_funk::typeclass::applicative::Apply::apply(t_funk::typeclass::functor::Replace::replace(self, t_funk::function::Id), f)
            }
        }
    }
}
*/
newtype_derive! {
    Then::then(#ident, #ty) => {
        impl<#ty, _Function> t_funk::typeclass::monad::Then<_Function> for #ident<#ty> where #ident<#ty>: t_funk::typeclass::monad::Chain<t_funk::closure::Curry2A<t_funk::function::Const, _Function>>
        {
            type Then = <#ident<#ty> as t_funk::typeclass::monad::Chain<t_funk::closure::Curry2A<t_funk::function::Const, _Function>>>::Chain;

            fn then(self, f: _Function) -> Self::Then {
               t_funk::typeclass::monad::Chain::<_>::chain(self, t_funk::closure::Curry2::prefix2(t_funk::function::Const, f))
            }
        }
    }
}

#[proc_macro_derive(Semigroup)]
pub fn semigroup(input: TokenStream) -> TokenStream {
    let mappend = mappend(input.clone());
    mappend.into()
}

// Derive `Mappend` for a newtype.
newtype_derive! {
    Mappend::mappend(#ident, #ty) => {
        impl<#ty, _Type> t_funk::typeclass::semigroup::Mappend<#ident<_Type>> for #ident<#ty>
        where
            #ty: t_funk::typeclass::semigroup::Mappend<_Type>,
        {
            type Mappend = #ident<#ty::Mappend>;

            #[allow(non_snake_case)]
            fn mappend(self, t: #ident<_Type>) -> Self::Mappend {
                let #ident(#ty) = self;
                let #ident(_Type) = t;
                #ident(
                    #ty.mappend(_Type),
                )
            }
        }

        impl<#ty> t_funk::typeclass::semigroup::Mappend<t_funk::collection::hlist::Nil> for #ident<#ty>
        {
            type Mappend = #ident<#ty>;

            fn mappend(self, _: t_funk::collection::hlist::Nil) -> Self::Mappend {
                self
            }
        }
    }
}

#[proc_macro_derive(Monoid)]
pub fn monoid(input: TokenStream) -> TokenStream {
    let mempty = mempty(input.clone());
    let mconcat = mconcat(input.clone());
    mempty.into_iter().chain(mconcat.into_iter()).collect()
}

// Derive `Mempty` for a newtype.
newtype_derive! {
    Mempty::mempty(#ident, #ty) => {
        impl<#ty> t_funk::typeclass::monoid::Mempty for #ident<#ty>
        where
            #ty: t_funk::typeclass::monoid::Mempty,
        {
            type Mempty = #ident<#ty::Mempty>;

            fn mempty() -> Self::Mempty {
                #ident(#ty::mempty())
            }
        }
    }
}

// Derive `Mconcat` for a newtype.
newtype_derive! {
    Mconcat::mconcat(#ident, #ty) => {
        impl<T> t_funk::typeclass::monoid::Mconcat for #ident<#ty>
        where
            T: t_funk::typeclass::monoid::Mempty + t_funk::typeclass::foldable::Foldr<t_funk::typeclass::semigroup::MappendF, <#ident<#ty> as t_funk::typeclass::monoid::Mempty>::Mempty>,
        {
            type Mconcat = <#ident<#ty> as t_funk::typeclass::foldable::Foldr<t_funk::typeclass::semigroup::MappendF, <#ident<#ty> as t_funk::typeclass::monoid::Mempty>::Mempty>>::Foldr;

            fn mconcat(self) -> Self::Mconcat {
                t_funk::typeclass::foldable::Foldr::foldr(self, t_funk::typeclass::semigroup::MappendF::default(), <#ident<#ty> as t_funk::typeclass::monoid::Mempty>::mempty())
            }
        }
    }
}

#[proc_macro_derive(Foldable)]
pub fn foldable(input: TokenStream) -> TokenStream {
    let fold_map = fold_map(input.clone());
    let foldl = foldl(input.clone());
    let foldr = foldr(input);
    fold_map
        .into_iter()
        .chain(foldl.into_iter())
        .chain(foldr.into_iter())
        .collect()
}

// Derive `FoldMap` for a newtype.
newtype_derive! {
    FoldMap::fold_map(#ident, #ty) => {
        impl<#ty, _Function> t_funk::typeclass::foldable::FoldMap<_Function> for #ident<#ty>
        where
            #ident<#ty>: t_funk::typeclass::functor::Fmap<_Function>,
            <#ident<#ty> as t_funk::typeclass::functor::Fmap<_Function>>::Fmap: t_funk::typeclass::monoid::Mconcat,
        {
            type FoldMap = <<#ident<#ty> as t_funk::typeclass::functor::Fmap<_Function>>::Fmap as t_funk::typeclass::monoid::Mconcat>::Mconcat;

            fn fold_map(self, f: _Function) -> Self::FoldMap {
                t_funk::typeclass::monoid::Mconcat::mconcat(t_funk::typeclass::functor::Fmap::fmap(self, f))
            }
        }
    }
}

// Derive `Foldr` for a newtype.
newtype_derive! {
    Foldr::foldr(#ident, #ty) => {
        impl<#ty, _Function, _Acc> t_funk::typeclass::foldable::Foldr<_Function, _Acc> for #ident<#ty>
        where
            #ty: t_funk::typeclass::foldable::Foldr<_Function, _Acc>,
        {
            type Foldr = #ident<<#ty as t_funk::typeclass::foldable::Foldr<_Function, _Acc>>::Foldr>;

            #[allow(non_snake_case)]
            fn foldr(self, f: _Function, z: _Acc) -> Self::Foldr {
                let #ident(#ty) = self;
                #ident(
                    t_funk::typeclass::foldable::Foldr::foldr(
                        #ty,
                        f,
                        z
                    )
                )
            }
        }
    }
}

// Derive `Foldl` for a newtype.
newtype_derive! {
    Foldl::foldl(#ident, #ty) => {
        impl<#ty, _Function, _Acc> t_funk::typeclass::foldable::Foldl<_Function, _Acc> for #ident<#ty>
        where
            #ty: t_funk::typeclass::foldable::Foldl<_Function, _Acc>,
        {
            type Foldl = #ident<<#ty as t_funk::typeclass::foldable::Foldl<_Function, _Acc>>::Foldl>;

            #[allow(non_snake_case)]
            fn foldl(self, f: _Function, z: _Acc) -> Self::Foldl {
                let #ident(#ty) = self;
                #ident(
                    t_funk::typeclass::foldable::Foldl::foldl(
                        #ty,
                        f,
                        z
                    )
                )
            }
        }
    }
}

// Derive `Fold` for a newtype.
newtype_derive! {
    Fold::fold(#ident, #ty) => {
        impl<#ty> t_funk::typeclass::foldable::Fold for #ident<#ty>
        where
            #ident<#ty>: t_funk::FoldMap<t_funk::function::Id>,
        {
            type Fold = <#ident<#ty> as t_funk::typeclass::foldable::FoldMap<t_funk::function::Id>>::FoldMap;

            fn fold(self) -> Self::Fold {
                t_funk::typeclass::foldable::FoldMap::<t_funk::function::Id>::fold_map(self, t_funk::function::Id)
            }
        }
    }
}

#[proc_macro_derive(Category)]
pub fn category(input: TokenStream) -> TokenStream {
    let id = id(input.clone());
    let compose = compose(input.clone());
    id.into_iter().chain(compose.into_iter()).collect()
}

// Derive `Id` for a `Function`.
newtype_derive! {
    Id::id(#ident, #ty) => {
        impl<#ty> t_funk::typeclass::category::Id for #ident<#ty> where #ident<#ty>: Default {
            type Id = t_funk::function::Id;

            fn id() -> Self::Id {
                t_funk::function::Id
            }
        }
    }
}

// Derive `Compose` for a `Function`.
newtype_derive! {
    Compose::compose(#ident, #ty) => {
        impl<_Function, #ty> t_funk::typeclass::category::Compose<_Function> for #ident<#ty> {
            type Compose = t_funk::closure::Composed<Self, _Function>;
            fn compose(self, f: _Function) -> Self::Compose {
                t_funk::closure::Composed(self, f)
            }
        }
    }
}

#[proc_macro_derive(Arrow)]
pub fn arrow(input: TokenStream) -> TokenStream {
    let arr = arr(input.clone());
    let split = split(input.clone());
    let fanout = fanout(input.clone());
    let first = arrow_first(input.clone());
    let second = arrow_second(input.clone());
    arr.into_iter()
        .chain(split.into_iter())
        .chain(fanout.into_iter())
        .chain(first.into_iter())
        .chain(second.into_iter())
        .collect()
}

// Derive `Arr` for a `Function`.
newtype_derive! {
    Arr::arr(#ident, #ty) => {
        impl<_Function, #ty> t_funk::typeclass::arrow::Arr<_Function> for #ident<#ty>
        {
            type Arr = _Function;

            fn arr(f: _Function) -> Self::Arr {
                f
            }
        }
    }
}

// Derive `arrow::First` for a `Function`.
newtype_derive! {
    ArrowFirst::arrow_first(#ident, #ty) => {
        impl<#ty> t_funk::typeclass::arrow::First for #ident<#ty>
        {
            type First = t_funk::typeclass::arrow::Firsted<#ident<#ty>>;

            fn first(self) -> Self::First {
                t_funk::typeclass::arrow::Firsted(self)
            }
        }
    }
}

// Derive `arrow::Second` for a `Function`.
newtype_derive! {
    ArrowSecond::arrow_second(#ident, #ty) => {
        impl<#ty> t_funk::typeclass::arrow::Second for #ident<#ty>
        {
            type Second = t_funk::typeclass::arrow::Seconded<#ident<#ty>>;

            fn second(self) -> Self::Second {
                t_funk::typeclass::arrow::Seconded(self)
            }
        }
    }
}

// Derive `Split` for a `Function`.
newtype_derive! {
    Split::split(#ident, #ty) => {
        impl<_Arrow, #ty> t_funk::typeclass::arrow::Split<_Arrow> for #ident<#ty> {
            type Split = t_funk::typeclass::arrow::Splitted<Self, _Arrow>;

            fn split(self, a: _Arrow) -> Self::Split {
                t_funk::typeclass::arrow::Splitted(self, a)
            }
        }
    }
}

// Derive `Fanout` for a `Split` implementor.
newtype_derive! {
    Fanout::fanout(#ident, #ty) => {
        impl<_Arrow, #ty> t_funk::typeclass::arrow::Fanout<_Arrow> for #ident<#ty>
        {
            type Fanout = t_funk::typeclass::arrow::Fanouted<Self, _Arrow>;

            fn fanout(self, f: _Arrow) -> Self::Fanout {
                t_funk::typeclass::arrow::Fanouted(self, f)
            }
        }
    }
}
