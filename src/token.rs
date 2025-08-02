use crate::Span;
use crate::lexer::Cursor;

pub trait Spanned {
    fn span(&self) -> Span;
}

pub trait Lex {
    fn lex(cursor: &mut Cursor) -> Self;
}

macro_rules! impl_spanned {
    ($struct:ident) => {
        impl Spanned for $struct {
            fn span(&self) -> Span {
                self.span
            }
        }
    }
}

macro_rules! define_operators {
    ($($token:literal pub struct $name:ident)*) => {
        $(
            #[derive(Debug, Clone, Copy)]
            pub struct $name {
                pub span: Span,
            }

            impl_spanned!($name);
        )*
    };
}

macro_rules! Token {
    [+] => { $crate::token::Plus };
    [-] => { $crate::token::Dash };
    [*] => { $crate::token::Star };
    [/] => { $crate::token::Slash };
}

define_operators! {
    "-"     pub struct Dash
    "+"     pub struct Plus
    "*"     pub struct Star
    "/"     pub struct Slash
}

#[derive(Debug, Clone)]
pub struct Literal {
    repr: f64,
    span: Span,
}

impl_spanned! { Literal }

#[derive(Debug, Clone)]
pub struct Ident {
    repr: String,
    span: Span,
}

impl_spanned! { Ident }
