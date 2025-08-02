use crate::token::{Token, Literal, Ident};

#[derive(Debug, Clone, Copy)]
pub struct Span {
    start: usize,
    len: usize,
}

struct Cursor<'s> {
    source: &'s str,
    input: std::str::Chars::<'s>,
    peeked: Option<(char, usize)>,
    pos: usize,
}

impl<'s> Cursor<'s> {
    pub fn new (source: &'s str) -> Self {
        Cursor {
            source,
            input: source.chars(),
            peeked: None,
            pos: 0
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
            len: self.current_position() - start
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
}

enum LexToken {
    Plus(Token![+]),
    Dash(Token![-]),
    Star(Token![*]),
    Slash(Token![/]),
    Literal(Literal),
    Ident(Ident),
}

pub struct TokenStream {
    inner: Vec<LexToken>
}

impl TokenStream {
    pub fn new() -> Self {
        TokenStream { inner: Vec::new() }
    }
}

pub fn tokenize(input: String) -> TokenStream {
    let mut cursor = Cursor::new(&input);
    return TokenStream::new();
}

fn lex_number(cursor: &mut Cursor) -> (f64, Span) {
    let start = cursor.mark();
    let number_str = cursor.eat_while(|c| c.is_ascii_digit() || c == '.');
    let number = number_str.parse::<f64>().unwrap();
    let span = cursor.span_from(start);
    (number, span)
}
