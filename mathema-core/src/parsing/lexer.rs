use std::{error::Error, ops::Deref};

use super::token::*;

#[derive(Debug, Clone)]
pub enum LexToken {
    Literal(Literal),
    Ident(Ident),
    Punct(Punct),
    OpenDelim(Delim),
    CloseDelim(Delim)
}

impl std::fmt::Display for LexToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            LexToken::Literal(_) => Literal::display(),
            LexToken::Ident(i) => i.symbol.as_str(),
            LexToken::Punct(p) => p.symbol.as_str(),
            LexToken::OpenDelim(d) => &format!("{:?}", d.kind),
            LexToken::CloseDelim(d) => &format!("{:?}", d.kind),

        };
        write!(f, "{}", str)
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
    loop {
        let ch = lexer.peek()?;
        match ch {
            ch if ch.is_whitespace() => {
                lexer.next()
            },

            '+' | '-' | '*' | '/' | '^' | '=' | ',' | '|' => {
                return Some(lexing::punct(lexer))
            },

            '(' => {
                return Some(lexing::open_delim(lexer))
            },
            ')' => {
                return Some(lexing::close_delim(lexer))
            },

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
                return None
            }
        };
    }
}

mod lexing {
    use crate::{
        parsing::{
            lexer::{Ident, LexError, LexErrorKind, LexToken, Lexer, Punct},
            token::{Delim, DelimKind, Literal}
        },
        symbol::Symbol
    };

    pub fn punct(lexer: &mut Lexer) -> LexToken {
        let start = lexer.mark();
        let ch = lexer.next().expect("calling code must ensure remaining characters exist");
        let span = lexer.span_from(start);
        LexToken::Punct(Punct { symbol: Symbol::intern(&ch.to_string()), span })
    }

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

    pub fn open_delim(lexer: &mut Lexer) -> LexToken {
        let ch = lexer.next().expect("a delimiter");
        let idx = lexer.current_position();
        let kind = match ch {
            '(' => DelimKind::Parenthesis,
            '|' => DelimKind::Bar,
            _ => unreachable!()
        };
        LexToken::OpenDelim(Delim { kind, span: (idx, idx+1).into() })
    }

    pub fn close_delim(lexer: &mut Lexer) -> LexToken {
        let ch = lexer.next().expect("a delimiter");
        let idx = lexer.current_position();
        let kind = match ch {
            ')' => DelimKind::Parenthesis,
            '|' => DelimKind::Bar,
            _ => unreachable!()
        };
        LexToken::CloseDelim(Delim { kind, span: (idx, idx+1).into() })
    }
}
