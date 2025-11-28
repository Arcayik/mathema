use std::{cell::Cell, error::Error, fmt::Display};

use super::{
    lexer::{LexToken, TokenBuffer},
    token::{Parse, Span, Spanned, Token}
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

impl ParseError {
    pub fn new<T: Display>(msg: T, span: Span) -> Self {
        let msg = msg.to_string();
        ParseError { msg, span }
    }

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

    pub fn lookahead(&self) -> Lookahead<'_> {
        Lookahead {
            input: self,
            peeked: Vec::new()
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

pub struct Lookahead<'s> {
    input: ParseStream<'s>,
    peeked: Vec<&'static str>
}

impl Lookahead<'_> {
    pub fn peek<T: Token>(&mut self) -> bool {
        if self.input.peek::<T>() {
            return true;
        }
        self.peeked.push(T::display());
        false
    }

    pub fn error(&self) -> ParseError {
        let span = self.input.get_span(self.input.peek_token());
        match self.peeked.len() {
            0 => {
                if self.input.is_eof() {
                    ParseError::new("unexpected end of input", span)
                }  else {
                    ParseError::new("unexpected token", span)
                }
            },
            1 => {
                let msg = format!("expected {}", self.peeked[0]);
                ParseError::new(msg, span)
            },
            2 => {
                let msg = format!("expected {} or {}", self.peeked[0], self.peeked[1]);
                ParseError::new(msg, span)
            },
            _ => {
                let list = self.peeked.join(", ");
                let msg = format!("expected one of: {}", list);
                ParseError::new(msg, span)
            }
        }
    }
}
