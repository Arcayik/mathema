use std::cell::Cell;

use crate::parse::{expr::Expr, lexer::UnknownChar, token::{LexToken, Span, Spanned}};

use super::*;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    span: Span,
}

impl ToString for ParseError {
    fn to_string(&self) -> String {
        format!("{} at ({}..{})", self.msg, self.span.start, self.span.end)
    }
}

impl From<UnknownChar> for ParseError {
    fn from(value: UnknownChar) -> Self {
        let msg = format!("Unknown character: '{}'", value.0);
        let span = value.1;
        ParseError { msg, span }
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
    src: Box<[LexToken]>,
    pos: Cell<usize>,
}

impl ParseBuffer {
    pub fn new(src: Box<[LexToken]>) -> Self {
        ParseBuffer { src, pos: Cell::new(0) }
    }

    pub fn peek_token(&self) -> &LexToken {
        &self.src[self.pos.get()]
    }

    pub fn next_token(&self) -> &LexToken {
        let token = self.peek_token();
        if !self.is_eof() {
            self.pos.update(|pos| pos + 1);
        }
        token
    }

    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(&self)
    }

    pub fn peek<T: Token>(&self) -> bool {
        T::peek(&self)
    }

    #[allow(unused)]
    pub fn peek2<T: Token>(&self) -> bool {
        let old_idx = self.pos.get();
        self.pos.update(|p| p+2);

        let mut peek = false;
        if !self.is_eof() {
            peek = T::peek(&self);
        }
        self.pos.set(old_idx);

        peek
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

    pub fn error(&self, msg: &'static str) -> ParseError {
        let span = self.peek_token().span();
        ParseError { msg: msg.to_string(), span }
    }
}

pub fn parse_expr(src: Box<[LexToken]>) -> Result<Expr> {
    let parser = ParseBuffer::new(src);
    let expr = parser.parse();
    expr
}
