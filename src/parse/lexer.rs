use crate::parse::token::{End, LexToken, Spanned};

use super::token::Span;

#[derive(Clone, Debug)]
pub enum LexError {
    UnknownChar(char, Span),
    NumParseError(Span),
    UnclosedDelim(Span),
    TrailingDelim(Span)
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownChar(ch, _) => write!(f, "Unknown character: {}", ch),
            Self::NumParseError(_) => write!(f, "Number parse error"),
            Self::UnclosedDelim(_) => write!(f, "Unclosed delimiter"),
            Self::TrailingDelim(_) => write!(f, "Trailing delimiter"),
        }
    }
}

impl Spanned for LexError {
    fn span(&self) -> Span {
        match self {
            Self::UnknownChar(_, span) => *span,
            Self::NumParseError(span) => *span,
            Self::UnclosedDelim(span) => *span,
            Self::TrailingDelim(span) => *span
        }
    }
}

pub struct Lexer<'s> {
    source: &'s str,
    input: std::str::Chars::<'s>,
    peeked: Option<(char, usize)>,
    pos: usize,
    errors: Vec<LexError>
}

impl<'s> Lexer<'s> {
    pub fn new (source: &'s str) -> Self {
        Lexer {
            source,
            input: source.chars(),
            peeked: None,
            pos: 0,
            errors: Vec::new()
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        if self.peeked.is_none() {
            let next_char = self.input.next()?;
            let byte_offset = self.pos;
            self.peeked = Some((next_char, byte_offset));
        }
        self.peeked.map(|(ch, _)| ch)
    }

    pub fn next(&mut self) -> Option<char> {
        if let Some((ch, byte_offset)) = self.peeked.take() {
            self.update_position(ch, byte_offset);
            Some(ch)
        } else {
            let ch = self.input.next()?;
            let byte_offset = self.pos;
            self.update_position(ch, byte_offset);
            Some(ch)
        }
    }

    fn update_position(&mut self, ch: char, byte_offset: usize) {
        self.pos = byte_offset + ch.len_utf8();
    }

    pub fn current_position(&self) -> usize {
        self.pos
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

    pub fn eat_while<F: Fn(char) -> bool>(&mut self, f: F) -> &'s str {
        let start = self.pos;
        while let Some(&ch) = self.peek().as_ref() {
            if f(ch) {
                self.next();
            } else {
                break;
            }
        }
        &self.source[start..self.pos]
    }

    pub fn is_eof(&mut self) -> bool {
        self.peek().is_none()
    }

    pub fn unknown_char(&mut self) {
        let span = self.span_char();
        let error = LexError::UnknownChar(self.next().expect("No char"), span);
        self.errors.push(error);
    }

    pub fn num_parse_error(&mut self) {
        let span = self.span_char();
        let error = LexError::NumParseError(span);
        self.errors.push(error);
    }

    pub fn unclosed_delim(&mut self) {
        let span = self.span_char();
        let error = LexError::UnclosedDelim(span);
        self.errors.push(error);
    }

    pub fn trailing_delim(&mut self) {
        let span = self.span_char();
        let error = LexError::TrailingDelim(span);
        self.errors.push(error);
    }

    pub fn all_errors(&self) -> &[LexError] {
        &self.errors
    }
}

pub fn tokenize(input: &str) -> (Box<[LexToken]>, Vec<LexError>) {
    let mut lexer = Lexer::new(input);
    let mut tokens: Vec<LexToken> = Vec::new();

    while let Some(ch) = lexer.peek() {
        let tok = match ch {
            ch if ch.is_whitespace() => {
                lexer.next();
                continue;
            },

            '+' | '-' | '*' | '/' | '=' => {
                lex::punct(&mut lexer, ch)
            },

            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                lex::ident(&mut lexer)
            },
            ch if ch.is_ascii_digit() => {
                lex::literal(&mut lexer)
            },
            _ => {
                lexer.unknown_char();
                continue;
            },
        };
        tokens.push(tok);
    }
    (tokens.into_boxed_slice(), lexer.all_errors().to_vec())
}

mod lex {
    use crate::parse::token::*;

    use super::{Lexer, LexToken};

    pub fn punct(lexer: &mut Lexer, ch: char) -> LexToken {
        let start = lexer.mark();
        lexer.next();
        let span = lexer.span_from(start);
        LexToken::Punct(Punct { repr: ch.to_string().into(), span })
    }

    fn is_ident_char(ch: char) -> bool {
        ch.is_alphanumeric() || ch == '_'
    }

    pub fn ident(lexer: &mut Lexer) -> LexToken {
        let start = lexer.mark();
        let ident_str = lexer.eat_while(is_ident_char);
        let span = lexer.span_from(start);
        LexToken::Ident(Ident { repr: ident_str.into(), span })
    }

    pub fn literal(lexer: &mut Lexer) -> LexToken {
        let start = lexer.mark();
        let number_str = lexer.eat_while(|ch| ch.is_ascii_digit() || ch == '.');
        let span = lexer.span_from(start);
        let number_result = number_str.parse::<f64>();
        LexToken::Literal(Literal { num: number_result.unwrap(), span })
    }
}
