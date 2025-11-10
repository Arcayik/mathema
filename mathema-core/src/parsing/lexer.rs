use std::{error::Error, fmt::Display, ops::Deref, rc::Rc};

use crate::error::Diagnostic;

use super::token::*;

#[derive(Debug, Clone)]
pub enum LexToken {
    Literal(Literal),
    Ident(Ident),
    Punct(Punct),
    Group(Group, usize),
    End(isize)
}

impl std::fmt::Display for LexToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            LexToken::Literal(_) => Literal::display(),
            LexToken::Ident(i) => i.symbol.as_str(),
            LexToken::Punct(p) => p.symbol.as_str(),
            LexToken::Group(g, o) => &format!("{:?}({})", g.delim, o),
            LexToken::End(o) => &format!("end({})", o),

        };
        write!(f, "{}", str)
    }
}

#[derive(Clone, Debug)]
pub struct LexError {
    src: Rc<str>,
    span: Span,
    kind: LexErrorKind
}

#[derive(Clone, Debug)]
pub enum LexErrorKind {
    UnknownChar(char),
    NumParseError,
    UnclosedDelim,
    TrailingDelim
}

impl Error for LexError {}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            LexErrorKind::UnknownChar(ch) => write!(f, "Unknown character: {}", ch),
            LexErrorKind::NumParseError => write!(f, "Number parse error"),
            LexErrorKind::UnclosedDelim => write!(f, "Unclosed delimiter"),
            LexErrorKind::TrailingDelim => write!(f, "Trailing delimiter"),
        }
    }
}

impl Diagnostic for LexError {
    fn message(&self) -> String {
        self.to_string()
    }

    fn source_code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(self.src.as_ref()))
    }

    fn spans(&self) -> Option<Box<dyn Iterator<Item = Span>>> {
        Some(Box::new(std::iter::once(self.span)))
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
    source: Rc<str>,
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
        let source = Rc::from(source);
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

struct Tokenizer<'s> {
    lexer: Lexer<'s>,
    tokens: Vec<LexToken>,
    errors: Vec<LexError>,
    delim_stack: Vec<usize>
}

impl<'s> Tokenizer<'s> {
    fn new(lexer: Lexer<'s>) -> Self {
        Tokenizer {
            lexer,
            tokens: Vec::new(),
            errors: Vec::new(),
            delim_stack: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) {
        while let Some(token) = self.lex_token() {
            self.tokens.push(token);
        }
        self.tokens.push(LexToken::End(self.tokens.len() as isize));
    }

    fn lex_token(&mut self) -> Option<LexToken> {
        loop {
            let ch = self.lexer.peek()?;
            match ch {
                ch if ch.is_whitespace() => {
                    self.lexer.next();
                },

                '+' | '-' | '*' | '/' | '^' | '=' | ',' => {
                    return Some(lexing::punct(self))
                },

                '(' | '{' | '[' => {
                    return Some(self.open_group())
                },

                ')' | '}' | ']' => {
                    let end = self.close_group();
                    if end.is_some() {
                        return end
                    }
                }

                ch if ch.is_ascii_alphabetic() || ch == '_' => {
                    return Some(lexing::ident(self))
                },
                ch if ch.is_ascii_digit() => {
                    let literal = lexing::literal(self);
                    if literal.is_some() {
                        return literal;
                    }
                },
                _ => {
                    self.unknown_char();
                },
            }
        }
    }

    fn open_group(&mut self) -> LexToken {
        let span = self.lexer.span_char();
        let ch = self.lexer.next()
            .expect("calling code must ensure remaining characters exist");

        let delim = match ch {
            '(' => Delimiter::Parenthesis,
            '{' => Delimiter::Brace,
            '[' => Delimiter::Bracket,
            _ => panic!("not a valid delimiter"),
        };

        let group = Group { delim, span };
        self.push_group();
        LexToken::Group(group, 0)
    }

    fn close_group(&mut self) -> Option<LexToken> {
        let top_idx = match self.pop_group() {
            Some(i) => i,
            None => {
                self.trailing_delim();
                self.lexer.next();
                return None;
            }
        };

        let ch = self.lexer.next()
            .expect("calling code must ensure remaining characters exist");

        let delim = match ch {
            ')' => Delimiter::Parenthesis,
            '}' => Delimiter::Brace,
            ']' => Delimiter::Bracket,
            _ => panic!("not a valid delimiter"),
        };

        let num_tokens = self.tokens.len();

        let top_group = self.tokens.get_mut(top_idx)
            .expect("delim stack must have valid group start index");

        // Get the delimiter kind of most recently opened group
        if let LexToken::Group(group, offset) = top_group {
            if group.delim != delim {
                self.trailing_delim();
                return None
            }

            // modify original group token
            *offset = num_tokens - top_idx;
            group.span.end = self.lexer.offset;

            // create new group end token
            let offset = top_idx as isize - num_tokens as isize;
            return Some(LexToken::End(offset))
        }

        None
    }

    fn push_group(&mut self) {
        self.delim_stack.push(self.tokens.len());
    }

    fn pop_group(&mut self) -> Option<usize> {
        self.delim_stack.pop()
    }

    pub fn handle_unclosed_groups(&mut self) {
        while let Some(unclosed_idx) = self.delim_stack.pop() {
            let token = self.tokens.get(unclosed_idx)
                .expect("delim stack must have valid group start index");
            let group = match token {
                LexToken::Group(g, _) => g,
                _ => panic!("delim stack index does not point to group token")
            };
            self.unclosed_delim(group.clone());
        }
    }

    fn unknown_char(&mut self) {
        let span = self.lexer.span_char();
        let kind = LexErrorKind::UnknownChar(self.lexer.next().expect("no char"));
        let error = LexError {
            src: self.lexer.source.clone(),
            span,
            kind,
        };
        self.errors.push(error);
    }

    fn num_parse_error(&mut self, span: Span) {
        let kind = LexErrorKind::NumParseError;
        let error = LexError {
            src: self.lexer.source.clone(),
            span,
            kind
        };
        self.errors.push(error);
    }

    fn unclosed_delim(&mut self, group: Group) {
        let span = group.span();
        let kind = LexErrorKind::UnclosedDelim;
        let error = LexError {
            src: self.lexer.source.clone(),
            span,
            kind
        };
        self.errors.push(error);
    }

    fn trailing_delim(&mut self) {
        let span = self.lexer.span_char();
        let kind = LexErrorKind::TrailingDelim;
        let error = LexError {
            src: self.lexer.source.clone(),
            span,
            kind
        };
        self.errors.push(error);
    }
}

pub fn tokenize(input: &str) -> (TokenBuffer, Vec<LexError>) {
    let lexer = Lexer::new(input);
    let mut tokenizer = Tokenizer::new(lexer);

    tokenizer.tokenize();
    tokenizer.handle_unclosed_groups();

    let tokens = tokenizer.tokens.into();
    let errors = tokenizer.errors;
    (tokens, errors)
}

mod lexing {
    use crate::{
        parsing::{lexer::{Ident, LexToken, Punct, Tokenizer}, token::Literal},
        symbol::Symbol
    };

    pub fn punct(tokenizer: &mut Tokenizer) -> LexToken {
        let start = tokenizer.lexer.mark();
        let ch = tokenizer.lexer.next().expect("calling code must ensure remaining characters exist");
        let span = tokenizer.lexer.span_from(start);
        LexToken::Punct(Punct { symbol: Symbol::intern(&ch.to_string()), span })
    }

    pub fn ident(tokenizer: &mut Tokenizer) -> LexToken {
        let start = tokenizer.lexer.mark();
        let ident_str = tokenizer.lexer.eat_while(|ch| ch.is_alphanumeric() || ch == '_');
        let symbol = Symbol::intern(ident_str);
        let span = tokenizer.lexer.span_from(start);
        LexToken::Ident(Ident { symbol, span })
    }

    pub fn literal(tokenizer: &mut Tokenizer) -> Option<LexToken> {
        let start = tokenizer.lexer.mark();
        let number_str = tokenizer.lexer.eat_while(|ch| ch.is_ascii_digit() || ch == '.');
        let result = number_str.parse::<f64>();
        let span = tokenizer.lexer.span_from(start);

        if let Ok(num) = result {
            Some(LexToken::Literal(Literal { num, span }))
        } else {
            tokenizer.num_parse_error(span);
            None
        }
    }
}
