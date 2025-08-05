use std::num::ParseFloatError;
use super::token::Span;

pub struct Lexer<'s> {
    source: &'s str,
    input: std::str::Chars::<'s>,
    peeked: Option<(char, usize)>,
    pos: usize,
}

impl<'s> Lexer<'s> {
    pub fn new (source: &'s str) -> Self {
        Lexer {
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
            end: self.current_position()
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

#[derive(Debug, Clone)]
pub enum TokenKind {
    Eq,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Literal(Result<f64, ParseFloatError>),
    Ident(Box<str>),
    Unknown(char),
    End,
}

#[derive(Debug, Clone)]
pub struct LexToken {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct TokenBuffer {
    pub tokens: Box<[LexToken]>,
}

impl TokenBuffer {
    pub fn new(tokens: Vec<LexToken>) -> Self {
        TokenBuffer { tokens: tokens.into_boxed_slice() }
    }

    pub fn print_all(&self) {
        let mut first = true;
        for token in self.tokens.iter() {
            if !first {
                print!(", ");
            }
            print!("{:?} ({},{})", token.kind, token.span.start, token.span.end);
            first = false;
        }
    }
}

pub fn tokenize(input: String) -> TokenBuffer {
    let mut lexer = Lexer::new(&input);
    let mut tokens: Vec<LexToken> = Vec::new();

    while let Some(ch) = lexer.peek() {
        let tok = match ch {
            ch if ch.is_whitespace() => {
                lexer.next();
                continue;
            },
            '+' => lex::single_char(&mut lexer, TokenKind::Plus),
            '-' => lex::single_char(&mut lexer, TokenKind::Minus),
            '*' => lex::single_char(&mut lexer, TokenKind::Star),
            '/' => lex::single_char(&mut lexer, TokenKind::Slash),
            '(' => lex::single_char(&mut lexer, TokenKind::LParen),
            ')' => lex::single_char(&mut lexer, TokenKind::RParen),
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                lex::ident(&mut lexer)
            },
            ch if ch.is_ascii_digit() => {
                lex::literal(&mut lexer)
            },
            _ => lex::single_char(&mut lexer, TokenKind::Unknown(ch)),
        };
        tokens.push(tok);
    }
    let end_token = LexToken {
        kind: TokenKind::End,
        span: Span { start: lexer.current_position(), end: lexer.current_position() }
    };
    tokens.push(end_token);
    return TokenBuffer::new(tokens);
}

mod lex {
    use super::{Span, Lexer, LexToken, TokenKind};

    pub fn single_char(lexer: &mut Lexer, kind: TokenKind) -> LexToken {
        let start = lexer.current_position();
        let span = Span { start, end: start + 1 };
        lexer.next();
        LexToken { kind, span }
    }

    pub fn ident(lexer: &mut Lexer) -> LexToken {
        let start = lexer.mark();
        let ident_str = lexer.eat_while(|ch| ch.is_ascii_alphabetic());
        let span = lexer.span_from(start);
        LexToken { kind: TokenKind::Ident(ident_str.into()), span }
    }

    pub fn literal(lexer: &mut Lexer) -> LexToken {
        let start = lexer.mark();
        let number_str = lexer.eat_while(|ch| ch.is_ascii_digit() || ch == '.');
        let span = lexer.span_from(start);
        let number_result = number_str.parse::<f64>();
        LexToken { kind: TokenKind::Literal(number_result), span }
    }
}
