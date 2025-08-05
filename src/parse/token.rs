use super::parser::ParseStream;
use super::lexer::{LexToken, TokenKind};

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub trait Spanned {
    fn span(&self) -> Span;
}

pub trait Parse: Sized {
    fn parse(input: ParseStream) -> Result<Self, ()>;
}

pub trait Peek {
    fn peek(input: ParseStream) -> bool;
}

macro_rules! impl_spanned {
    ($($struct:ident),*) => {
        $(
            impl Spanned for $struct {
                fn span(&self) -> Span {
                    self.span
                }
            }
        )*
    }
}

macro_rules! define_punctuation {
    ($($token:literal pub struct $name:ident)*) => {
        $(
            #[derive(Debug, Clone, Copy)]
            pub struct $name {
                pub span: Span,
            }

            impl_spanned!($name);

            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self, ()> {
                    match input.next_token() {
                        LexToken { kind: TokenKind::$name, span } => {
                            Ok($name { span: *span })
                        },
                        _ => Err(())
                    }
                }
            }

            impl Peek for $name {
                fn peek(input: ParseStream) -> bool {
                    match input.next_token().kind {
                        TokenKind::$name => true,
                        _ => false,
                    }
                }
            }

        )*
    };
}

#[macro_export]
macro_rules! Token {
    [=] => { super::token::Eq };
    [+] => { super::token::Plus };
    [-] => { super::token::Minus };
    [*] => { super::token::Star };
    [/] => { super::token::Slash };
}

define_punctuation! {
    "="     pub struct Eq
    "+"     pub struct Plus
    "-"     pub struct Minus
    "*"     pub struct Star
    "/"     pub struct Slash
    "("     pub struct LParen
    ")"     pub struct RParen
}

#[derive(Debug, Clone)]
pub(super) struct Literal {
    repr: f64,
    span: Span,
}

#[derive(Debug, Clone)]
pub(super) struct Ident {
    repr: String,
    span: Span,
}

impl_spanned! { Literal, Ident }

