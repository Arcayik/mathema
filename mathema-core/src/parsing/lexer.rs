use std::{error::Error, ops::Deref};

use super::token::*;

#[derive(Debug, Clone)]
pub enum LexToken {
    Literal(Literal),
    Ident(Ident),

    Plus(Token![+]),
    Minus(Token![-]),
    Star(Token![*]),
    Slash(Token![/]),
    Caret(Token![^]),
    Equals(Token![=]),
    Comma(Token![,]),

    LParen(LParen),
    RParen(RParen),
}

impl std::fmt::Display for LexToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            LexToken::Literal(lit) => &lit.num.to_string(),
            LexToken::Ident(i) => i.symbol.as_str(),
            LexToken::Plus(_) => crate::parsing::token::Plus::display(),
            LexToken::Minus(_) => crate::parsing::token::Minus::display(),
            LexToken::Star(_) => crate::parsing::token::Star::display(),
            LexToken::Slash(_) => crate::parsing::token::Slash::display(),
            LexToken::Caret(_) => crate::parsing::token::Caret::display(),
            LexToken::Equals(_) => crate::parsing::token::Equals::display(),
            LexToken::Comma(_) => crate::parsing::token::Comma::display(),

            LexToken::LParen(..) => crate::parsing::token::LParen::display(),
            LexToken::RParen(..) => crate::parsing::token::RParen::display(),
        };
        write!(f, "{}", str)
    }
}

impl Spanned for LexToken {
    fn span(&self) -> Span {
        match self {
            LexToken::Literal(literal) => literal.span(),
            LexToken::Ident(ident) => ident.span(),

            LexToken::Plus(tok) => tok.span,
            LexToken::Minus(tok) => tok.span,
            LexToken::Star(tok) => tok.span,
            LexToken::Slash(tok) => tok.span,
            LexToken::Caret(tok) => tok.span,
            LexToken::Equals(tok) => tok.span,
            LexToken::Comma(tok) => tok.span,

            LexToken::LParen(tok) => tok.span(),
            LexToken::RParen(tok) => tok.span(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LexError {
    kind: LexErrorKind,
    span: Span,
}

impl LexError {
    pub fn new(kind: LexErrorKind, span: Span) -> Self {
        LexError { span, kind }
    }
}

#[derive(Clone, Debug)]
pub enum LexErrorKind {
    UnknownChar(char),
    NumParseError,
}

impl Error for LexError {}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            LexErrorKind::UnknownChar(ch) => write!(f, "Unknown character: {}", ch),
            LexErrorKind::NumParseError => write!(f, "Number parse error"),
        }
    }
}

impl Spanned for LexError {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug)]
pub struct TokenBuffer(Box<[LexToken]>);

impl TokenBuffer {
    pub fn empty() -> Self {
        TokenBuffer(Box::new([]))
    }

    pub fn from_slice(input: impl AsRef<[LexToken]>) -> Self {
        let slice = input.as_ref();
        TokenBuffer(Box::from(slice))
    }
}

impl From<Box<[LexToken]>> for TokenBuffer {
    fn from(value: Box<[LexToken]>) -> Self {
        TokenBuffer(value)
    }
}

impl From<Vec<LexToken>> for TokenBuffer {
    fn from(value: Vec<LexToken>) -> Self {
        TokenBuffer(value.into())
    }
}

impl FromIterator<LexToken> for TokenBuffer {
    fn from_iter<T: IntoIterator<Item = LexToken>>(iter: T) -> Self {
        let inner = iter.into_iter().collect();
        TokenBuffer(inner)
    }
}

impl Deref for TokenBuffer {
    type Target = [LexToken];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct Lexer<'s> {
    /// Trimmed input
    source: &'s str,
    /// Iterator over input chars
    input: std::str::Chars::<'s>,
    /// Peeked character and its byte offset
    peeked: Option<(char, usize)>,
    /// Current byte offset
    offset: usize,
}

impl Iterator for Lexer<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((ch, byte_offset)) = self.peeked.take() {
            self.update_position(ch, byte_offset);
            Some(ch)
        } else {
            let ch = self.input.next()?;
            let byte_offset = self.offset;
            self.update_position(ch, byte_offset);
            Some(ch)
        }
    }
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Self {
        let input = source.chars();
        Lexer {
            source,
            input,
            peeked: None,
            offset: 0,
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        if self.peeked.is_none() {
            let next_char = self.input.next()?;
            let byte_offset = self.offset;
            self.peeked = Some((next_char, byte_offset));
        }
        self.peeked.map(|(ch, _)| ch)
    }

    fn update_position(&mut self, ch: char, byte_offset: usize) {
        self.offset = byte_offset + ch.len_utf8();
    }

    pub fn current_position(&self) -> usize {
        self.offset
    }

    pub fn is_eof(&self) -> bool {
        self.current_position() == self.source.len()
    }

    pub fn mark(&self) -> usize {
        self.current_position()
    }

    pub fn span_from(&self, start: usize) -> Span {
        Span {
            start,
            end: self.current_position()
        }
    }

    pub fn span_char(&self) -> Span {
        let here = self.current_position();
        Span {
            start: here,
            end: here + 1
        }
    }

    pub fn eat_while<F: Fn(char) -> bool>(&mut self, f: F) -> &str {
        let start = self.offset;
        while let Some(&ch) = self.peek().as_ref() {
            if f(ch) {
                self.next();
            } else {
                break;
            }
        }
        &self.source[start..self.offset]
    }
}

pub fn tokenize(input: &str) -> Result<TokenBuffer, Vec<LexError>> {
    let mut lexer = Lexer::new(input);
    let mut errors = Vec::new();
    let mut tokens = Vec::new();

    while !lexer.is_eof() {
        if let Some(token) = parse_token(&mut lexer, &mut errors) { // self.lex_token
            tokens.push(token);
        }
    }

    Ok(tokens.into())
}

fn parse_token(lexer: &mut Lexer, errors: &mut Vec<LexError>) -> Option<LexToken> {
    let result = loop {
        let ch = lexer.peek()?;
        let idx = lexer.current_position();
        let span = Span { start: idx, end: idx + 1 };
        match ch {
            ch if ch.is_whitespace() => {
                lexer.next()
            },

            '+' => break Some(LexToken::Plus(Plus { span })),
            '-' => break Some(LexToken::Minus(Minus { span })),
            '*' => break Some(LexToken::Star(Star { span })),
            '/' => break Some(LexToken::Slash(Slash { span })),
            '^' => break Some(LexToken::Caret(Caret { span })),
            '=' => break Some(LexToken::Equals(Equals { span })),
            ',' => break Some(LexToken::Comma(Comma { span })),

            '(' => break Some(LexToken::LParen(LParen { span })),
            ')' => break Some(LexToken::RParen(RParen { span })),

            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                return Some(lexing::ident(lexer))
            },

            ch if ch.is_ascii_digit() => {
                match lexing::literal(lexer) {
                    Ok(tok) => return Some(tok),
                    Err(e) => {
                        errors.push(e);
                        return None
                    }
                }
            },

            _ => {
                let kind = LexErrorKind::UnknownChar(ch);
                let idx = lexer.current_position();
                let span = (idx, idx+1).into();
                let error = LexError::new(kind, span);
                errors.push(error);
                break None
            }
        };
    };

    lexer.next();
    result
}

mod lexing {
    use crate::{
        parsing::{
            lexer::{Ident, LexError, LexErrorKind, LexToken, Lexer},
            token::Literal
        },
        symbol::Symbol
    };

    pub fn ident(lexer: &mut Lexer) -> LexToken {
        let start = lexer.mark();
        let ident_str = lexer.eat_while(|ch| ch.is_alphanumeric() || ch == '_');
        let symbol = Symbol::intern(ident_str);
        let span = lexer.span_from(start);
        LexToken::Ident(Ident { symbol, span })
    }

    pub fn literal(lexer: &mut Lexer) -> Result<LexToken, LexError> {
        let start = lexer.mark();
        let number_str = lexer.eat_while(|ch| ch.is_ascii_digit() || ch == '.');
        let result = number_str.parse::<f64>();
        let span = lexer.span_from(start);

        if let Ok(num) = result {
            Ok(LexToken::Literal(Literal { num, span }))
        } else {
            Err(LexError::new(LexErrorKind::NumParseError, span))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{LexToken, tokenize};

    macro_rules! lex {
        ($input:literal) => {
            tokenize($input).unwrap()
        }
    }

    macro_rules! toks_eq {
        ($input:literal, [ $($tok:ident),* ]) => {{
            let buffer = lex!($input);
            let mut iter = buffer.into_iter();
            $(
                assert!(matches!(iter.next().unwrap(), LexToken::$tok(..)));
            )*
        }}
    }

    #[test]
    fn lexing() {
        toks_eq!("1 + 1", [Literal, Plus, Literal]);
        toks_eq!("2*3 + 6/7", [Literal, Star, Literal, Plus, Literal, Slash, Literal]);
        toks_eq!("(3.14)", [LParen, Literal, RParen]);
        toks_eq!("f(x,y) = 2x + 3y^2", [Ident, LParen, Ident, Comma, Ident, RParen, Equals, Literal, Ident, Plus, Literal, Ident, Caret, Literal]);
    }
}
