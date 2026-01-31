use crate::{
    parsing::{
        lexer::LexToken,
        parser::{ParseError, ParseStream},
    },
    symbol::Symbol
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<(usize, usize)> for Span {
    fn from(value: (usize, usize)) -> Self {
        Span { start: value.0, end: value.1 }
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

pub trait Parse: Sized {
    fn parse(input: ParseStream) -> Result<Self, ParseError>;
}

pub trait Token {
    fn peek(input: ParseStream) -> bool;
    fn display() -> &'static str;
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

macro_rules! define_tokens {
    ($($token:literal pub struct $name:ident)*) => {
        $(
            #[derive(Debug, Clone, Copy)]
            pub struct $name {
                pub span: Span,
            }

            impl_spanned!($name);

            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self, ParseError> {

                    if let $crate::parsing::lexer::LexToken::$name(token) = input.next_token() {
                        Ok(Self { span: token.span })
                    } else {
                        Err(input.error(&format!("Expected {}", stringify!($name))))
                    }
                }
            }

            impl Token for $name {
                fn peek(input: ParseStream) -> bool {
                    matches!(input.peek_token(), $crate::parsing::lexer::LexToken::$name(..))
                }

                fn display() -> &'static str { $token }
            }
        )*
    };
}

macro_rules! Token {
    [+] => { $crate::parsing::token::Plus };
    [-] => { $crate::parsing::token::Minus };
    [*] => { $crate::parsing::token::Star };
    [/] => { $crate::parsing::token::Slash };
    [^] => { $crate::parsing::token::Caret };
    [=] => { $crate::parsing::token::Equals };
    [,] => { $crate::parsing::token::Comma };
}

define_tokens! {
    "+"     pub struct Plus
    "-"     pub struct Minus
    "*"     pub struct Star
    "/"     pub struct Slash
    "^"     pub struct Caret
    "="     pub struct Equals
    ","     pub struct Comma
    "("     pub struct LParen
    ")"     pub struct RParen
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub num: f64,
    pub span: Span,
}

impl PartialEq<Literal> for Literal {
    fn eq(&self, other: &Literal) -> bool {
        self.num == other.num
    }
}

impl Token for Literal {
    fn peek<'s>(input: ParseStream) -> bool {
        matches!(input.peek_token(), LexToken::Literal(_))
    }

    fn display() -> &'static str { "literal" }
}

impl Parse for Literal {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        if let LexToken::Literal(literal) = input.next_token() {
            Ok(literal.clone())
        } else {
            Err(input.error("Expected literal"))
        }
    }
}

impl_spanned! { Literal }

#[derive(Debug, Clone)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: Span,
}

impl PartialEq<Ident> for Ident {
    fn eq(&self, other: &Ident) -> bool {
        self.symbol == other.symbol
    }
}

impl Token for Ident {
    fn peek<'s>(input: ParseStream) -> bool {
        matches!(input.next_token(), LexToken::Ident(_))
    }

    fn display() -> &'static str { "ident" }
}

impl Parse for Ident {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        if let LexToken::Ident(ident) = input.next_token() {
            Ok(ident.clone())
        } else {
            Err(input.error("Expected ident"))
        }
    }
}

impl_spanned! { Ident }

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DelimKind {
    /// ( ... )
    Parenthesis,
}

#[derive(Debug, Clone)]
pub struct Delim {
    pub kind: DelimKind,
    pub span: Span
}

impl_spanned! { Delim }

