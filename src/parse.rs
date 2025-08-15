#[macro_use]
mod lexer;
mod token;
mod expr;

pub use token::{Token, Parse};
pub use lexer::tokenize;
pub use parser::parse_expr;

mod parser {
    use std::cell::Cell;

    use crate::parse::{expr::Expr, token::{LexToken, Span, Spanned}};

    use super::*;

    #[derive(Debug)]
    pub struct ParseError {
        msg: &'static str,
        span: Span,
    }

    impl Into<String> for ParseError {
        fn into(self) -> String {
            format!("{} at ({}..{})", self.msg, self.span.start, self.span.end)
        }
    }

    impl ParseError {
        pub fn span(&self) -> Span {
            self.span
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

        pub fn parse<T: Parse>(&self) -> Result<T, ParseError> {
            T::parse(&self)
        }

        pub fn peek<T: Token>(&self) -> bool {
            T::peek(&self)
        }

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


        pub fn is_eof(&self) -> bool {
            self.pos.get() == self.src.len() - 1
        }

        pub fn error(&self, msg: &'static str) -> ParseError {
            let span = self.peek_token().span();
            ParseError { msg, span }
        }
    }

    pub fn parse_expr(src: Box<[LexToken]>) -> Result<Expr, ParseError> {
        let parser = ParseBuffer::new(src);
        let expr = parser.parse();
        expr
    }
}
