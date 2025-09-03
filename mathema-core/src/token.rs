use crate::{
    lexer::LexToken,
    parser::{ParseStream, ParseError},
};

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

            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self, ParseError> {
                    if let $crate::lexer::LexToken::Punct(punct) = input.next_token() {
                        if punct.repr.to_string() == $token {
                            Ok(Self { span: punct.span })
                        } else {
                            Err(input.error(&format!("Expected {}", stringify!($name))))
                        }
                    } else {
                        Err(input.error("Expected punct"))
                    }
                }
            }

            impl Token for $name {
                fn peek(input: ParseStream) -> bool {
                    if let $crate::lexer::LexToken::Punct(punct) = input.peek_token() {
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

macro_rules! define_delimiters {
    ($($variant:ident pub struct $name:ident $string:literal)*) => {
        $(
            #[allow(unused)]
            #[derive(Debug)]
            pub struct $name {
                span: $crate::token::Span
            }

            impl_spanned! { $name }

            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self, ParseError> {
                    if let $crate::lexer::LexToken::Group(group, _) = input.next_token() {
                        if matches!(group.delim, Delimiter::$variant) {
                            Ok(Self { span: group.span() })
                        } else {
                            Err(input.error(&format!("Expected {}", $string)))
                        }
                    } else {
                        Err(input.error("Expected group"))
                    }
                }
            }

            impl Token for $name {
                fn peek<'s>(input: ParseStream) -> bool {
                    match input.peek_token() {
                        $crate::lexer::LexToken::Group(g, _) => matches!(g.delim, Delimiter::$variant),
                        _ => false
                    }
                }

                fn display() -> &'static str { $string }
            }
        )*
    };
}

#[macro_export]
macro_rules! Token {
    [+] => { $crate::token::Plus };
    [-] => { $crate::token::Minus };
    [*] => { $crate::token::Star };
    [/] => { $crate::token::Slash };
    [^] => { $crate::token::Caret };
    [=] => { $crate::token::Equals };
    [,] => { $crate::token::Comma };
}

define_punctuation! {
    "+"     pub struct Plus
    "-"     pub struct Minus
    "*"     pub struct Star
    "/"     pub struct Slash
    "^"     pub struct Caret
    "="     pub struct Equals
    ","     pub struct Comma
}

define_delimiters! {
    Parenthesis pub struct Paren    "paren"
    Brace       pub struct Brace    "brace"
    Bracket     pub struct Bracket  "bracket"
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub num: f64,
    pub span: Span,
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
    pub repr: Box<str>,
    pub span: Span,
}

impl Token for Ident {
    fn peek<'s>(input: ParseStream) -> bool {
        matches!(input.peek_token(), LexToken::Ident(_))
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delimiter {
    /// { ... }
    Parenthesis,
    /// { ... }
    Brace,
    /// \[ ... \]
    Bracket,
}

#[derive(Debug, Clone)]
pub struct Group {
    pub delim: Delimiter,
    pub span: Span
}

impl_spanned! { Group }

#[derive(Debug, Clone)]
pub struct End;

impl Token for End {
    fn peek(input: ParseStream) -> bool {
        matches!(input.peek_token(), LexToken::End(..))
    }

    fn display() -> &'static str { "end" }
}
