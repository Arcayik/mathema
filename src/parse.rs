#[macro_use]
mod lexer;
mod token;
mod item;

use lexer::*;

pub use token::{Peek, Parse};
pub use lexer::tokenize;

mod parser {
    use super::*;

    pub type ParseStream<'p> = &'p ParseBuffer;

    pub struct ParseBuffer {
        src: Box<[LexToken]>,
        pos: std::cell::Cell<usize>,
    }

    impl From<TokenBuffer> for ParseBuffer {
        fn from(value: TokenBuffer) -> Self {
            ParseBuffer { src: value.tokens, pos: 0.into() }
        }
    }

    impl ParseBuffer {
        fn advance(&self) {
            if self.pos.get() < self.src.len() {
                self.pos.replace(self.pos.get() + 1);
            }
        }

        pub fn next_token(&self) -> &LexToken {
            let token = &self.src[self.pos.get()];
            self.advance();
            token
        }

        pub fn peek_token(&self) -> &LexToken {
            &self.src[self.pos.get()]
        }

        pub fn parse<T: Parse>(&self) -> Result<T, ()> {
            T::parse(&self)
        }

        pub fn peek<T: Peek>(&self) -> bool {
            T::peek(&self)
        }

        pub fn is_empty(&self) -> bool {
            self.pos.get() >= self.src.len()
        }
    }
}
