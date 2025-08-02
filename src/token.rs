use crate::Span;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    message: String,
    span: Span
}

impl Error {
    pub fn new(message: String, span: Span) -> Self {
        Error { message, span }
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
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

macro_rules! Token {
    [+] => { $crate::token::Plus };
    [-] => { $crate::token::Dash };
    [*] => { $crate::token::Star };
    [/] => { $crate::token::Slash };
}
pub(crate) use Token;

use crate::lexer::TokenStream;

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

define_operators! {
    "="     pub struct Eq
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
