use std::num::ParseFloatError;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    start: usize,
    len: usize,
}

pub struct Cursor<'s> {
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

#[derive(Debug)]
enum TokenKind {
    Plus,
    Dash,
    Star,
    Slash,
    Literal(Result<f64, ParseFloatError>),
    Ident(String),
    Unknown(char),
}

#[derive(Debug)]
pub struct LexToken {
    kind: TokenKind,
    span: Span,
}

#[derive(Debug)]
pub struct TokenStream {
    inner: Vec<LexToken>
}

impl TokenStream {
    pub fn new() -> Self {
        TokenStream { inner: Vec::new() }
    }
    
    pub fn push(&mut self, token: LexToken) {
        self.inner.push(token);
    }

    pub fn print(&self) {
        let mut first = true;
        for token in self.inner.iter() {
            if !first {
                print!(", ");
            }
            print!("{:?} ({},{})", token.kind, token.span.start, token.span.len);
            first = false;
        }
    }
}

pub fn tokenize(input: String) -> TokenStream {
    let mut cursor = Cursor::new(&input);
    let mut token_stream = TokenStream::new();

    while let Some(ch) = cursor.peek() {
        let tok = match ch {
            ch if ch.is_whitespace() => {
                cursor.next();
                continue;
            },
            '+' => lex::single_char(&mut cursor, TokenKind::Plus),
            '-' => lex::single_char(&mut cursor, TokenKind::Dash),
            '*' => lex::single_char(&mut cursor, TokenKind::Star),
            '/' => lex::single_char(&mut cursor, TokenKind::Slash),
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                lex::ident(&mut cursor)
            },
            ch if ch.is_ascii_digit() => {
                lex::literal(&mut cursor)
            },
            _ => lex::single_char(&mut cursor, TokenKind::Unknown(ch)),
        };
        token_stream.push(tok);
    }

    return token_stream;
}

mod lex {
    use super::{Span, Cursor, LexToken, TokenKind};

    pub fn single_char(cursor: &mut Cursor, kind: TokenKind) -> LexToken {
        let start = cursor.current_position();
        let span = Span { start, len: 1 };
        cursor.next();
        LexToken { kind, span }
    }

    pub fn ident(cursor: &mut Cursor) -> LexToken {
        let start = cursor.mark();
        let ident_str = cursor.eat_while(|ch| ch.is_ascii_alphabetic());
        let span = cursor.span_from(start);
        LexToken { kind: TokenKind::Ident(ident_str.to_string()), span }
    }

    pub fn literal(cursor: &mut Cursor) -> LexToken {
        let start = cursor.mark();
        let number_str = cursor.eat_while(|ch| ch.is_ascii_digit() || ch == '.');
        let span = cursor.span_from(start);
        let number_result = number_str.parse::<f64>();
        LexToken { kind: TokenKind::Literal(number_result), span }
    }
}
