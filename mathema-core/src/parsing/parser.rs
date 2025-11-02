use std::{cell::Cell, error::Error};

use crate::error::Diagnostic;

use super::{
    lexer::{LexToken, TokenBuffer}, token::{Parse, Span, Spanned, Token}
};

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    span: Span,
}

impl Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Diagnostic for ParseError {
    fn message(&self) -> String {
        self.to_string()
    }

    fn spans(&self) -> Option<Box<dyn Iterator<Item = Span>>> {
        Some(Box::new(std::iter::once(self.span)))
    }

    fn source_code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        None
    }
}

impl ParseError {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy)]
pub struct ParserPosition(usize);

impl From<usize> for ParserPosition {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

pub type ParseStream<'p> = &'p ParseBuffer;

pub struct ParseBuffer {
    src: TokenBuffer,
    pos: Cell<usize>,
}

impl ParseBuffer {
    pub fn new(src: TokenBuffer) -> Self {
        ParseBuffer { src, pos: Cell::new(0) }
    }

    pub fn peek_token(&self) -> &LexToken {
        &self.src[self.pos.get()]
    }

    pub fn debug(&self) {
        let line = self.src.iter()
            .enumerate()
            .map(|(i,t)| {
                if i == self.pos.get() {
                    format!("{{{}}} ->", t)
                } else {
                    t.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join(" ");
        println!("{}", line);
    }

    pub fn next_token(&self) -> &LexToken {
        let token = self.peek_token();
        if !self.is_eof() {
            self.pos.update(|pos| pos + 1);
        }
        token
    }

    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    pub fn peek<T: Token>(&self) -> bool {
        T::peek(self)
    }

    pub fn peek2<T: Token>(&self) -> bool {
        let old_idx = self.pos.get();
        self.next_token();
        let found = self.peek::<T>();
        self.pos.set(old_idx);
        found
    }

    /// Peek next token while skipping tokens within a group, but not ignoring the group itself.
    pub fn peek_ignore_group<T: Token>(&self) -> bool {
        if let LexToken::Group(_, offset) = self.peek_token() {
            let begin = self.save_pos();
            self.pos.update(|pos| pos + offset + 1);

            if self.is_eof() {
                return false
            }
            let peeked = self.peek::<T>();
            self.restore_pos(begin);

            peeked
        } else {
            // nothing to skip, peek as usual
            self.peek::<T>()
        }
    }

    pub fn pos(&self) -> usize {
        self.pos.get()
    }

    pub fn save_pos(&self) -> ParserPosition {
        self.pos.get().into()
    }

    pub fn restore_pos(&self, pos: ParserPosition) {
        self.pos.set(pos.0);
    }

    pub fn is_eof(&self) -> bool {
        self.pos.get() == self.src.len() - 1
    }

    pub fn get_span(&self, token: &LexToken) -> Span {
        match token {
            LexToken::Literal(literal) => literal.span(),
            LexToken::Ident(ident) => ident.span(),
            LexToken::Punct(punct) => punct.span(),
            LexToken::Group(group, _offset) => group.span(),
            LexToken::End(offset) => {
                let group_pos = self.pos.get() - offset.unsigned_abs();
                let group = &self.src[group_pos];
                self.get_span(group)
            },
        }
    }

    pub fn error(&self, msg: &str) -> ParseError {
        let span = self.get_span(self.peek_token());
        ParseError { msg: msg.to_string(), span }
    }

    pub fn spanned_error(&self, msg: &str, span: Span) -> ParseError {
        ParseError { msg: msg.to_string(), span }
    }
}
