use crate::parse::parser::ParseError;

use super::parser::ParseStream;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
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

macro_rules! define_punctuation {
    ($($token:literal pub struct $name:ident)*) => {
        $(
            #[derive(Debug, Clone, Copy)]
            pub struct $name {
                pub span: Span,
            }

            impl_spanned!($name);

            impl $name {
                pub fn from_span(span: Span) -> Self {
                    Self { span }
                }
            }

            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self, ParseError> {
                    if let LexToken::Punct(punct) = input.next_token() {
                        if punct.repr.to_string() == $token {
                            Ok(Self { span: punct.span })
                        } else {
                            Err(input.error("Expected punct"))
                        }
                    } else {
                        Err(input.error("Not a punct"))
                    }
                }
            }

            impl Token for $name {
                fn peek(input: ParseStream) -> bool {
                    if let LexToken::Punct(punct) = input.peek_token() {
                        punct.repr.to_string() == $token
                    } else {
                        false
                    }
                }

                fn display() -> &'static str { $token }
            }

        )*
    };
}

#[macro_export]
macro_rules! Token {
    [=] => { $crate::parse::token::Equals };
    [+] => { $crate::parse::token::Plus };
    [-] => { $crate::parse::token::Minus };
    [*] => { $crate::parse::token::Star };
    [/] => { $crate::parse::token::Slash };
}

define_punctuation! {
    "+"     pub struct Plus
    "-"     pub struct Minus
    "*"     pub struct Star
    "/"     pub struct Slash
    "="     pub struct Equals
}

#[derive(Debug, Clone)]
pub enum LexToken {
    Literal(Literal),
    Ident(Ident),
    Punct(Punct),
    End(End)
}

impl Spanned for LexToken {
    fn span(&self) -> Span {
        match self {
            Self::Literal(literal) => literal.span(),
            Self::Ident(ident) => ident.span(),
            Self::Punct(punct) => punct.span(),
            Self::End(end) => end.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub num: f64,
    pub span: Span,
}

impl Token for Literal {
    fn peek<'s>(input: ParseStream) -> bool {
        match input.peek_token() {
            LexToken::Literal(_) => true,
            _ => false
        }
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
    pub repr: Box<str>,
    pub span: Span,
}

impl Token for Ident {
    fn peek<'s>(input: ParseStream) -> bool {
        match input.peek_token() {
            LexToken::Ident(_) => true,
            _ => false
        }
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

#[derive(Debug, Clone)]
pub struct Punct {
    pub repr: Box<str>,
    pub span: Span,
}
impl_spanned! { Punct }

#[derive(Debug, Clone, Copy)]
pub struct End {
    pub span: Span
}

impl_spanned!{ End }

impl Token for End {
    fn peek(input: ParseStream) -> bool {
        input.is_eof()
    }

    fn display() -> &'static str { "EOF" }
}
