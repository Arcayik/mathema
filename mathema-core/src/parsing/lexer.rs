use std::ops::Deref;

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
            LexToken::Ident(i) => i.name.as_ref(),
            LexToken::Punct(p) => p.name.as_ref(),
            LexToken::Group(g, o) => &format!("{:?}({})", g.delim, o),
            LexToken::End(o) => &format!("end({})", o),

        };
        write!(f, "{}", str)
    }
}

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

pub struct TokenBuffer(Box<[LexToken]>);

impl TokenBuffer {
    pub fn empty() -> Self {
        TokenBuffer(Box::new([]))
    }

    pub fn from_boxed_slice(slice: Box<[LexToken]>) -> Self {
        TokenBuffer(slice)
    }

    pub fn from_vec(vec: Vec<LexToken>) -> Self {
        TokenBuffer(vec.into_boxed_slice())
    }

    pub fn from_iter<T: Iterator<Item = LexToken>>(iter: T) -> Self {
        let inner = iter.collect();
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
    source: &'s str,
    /// Iterator over input chars
    input: std::str::Chars::<'s>,
    /// Peeked character and its byte offset
    peeked: Option<(char, usize)>,
    /// Current byte offset
    offset: usize,
}

impl<'s> Lexer<'s> {
    pub fn new (source: &'s str) -> Self {
        let source = source.trim();
        Lexer {
            source,
            input: source.chars(),
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

    pub fn next(&mut self) -> Option<char> {
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

    pub fn eat_while<F: Fn(char) -> bool>(&mut self, f: F) -> &'s str {
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
        let error = LexError::UnknownChar(self.lexer.next().expect("no char"), span);
        self.errors.push(error);
    }

    fn num_parse_error(&mut self, span: Span) {
        let error = LexError::NumParseError(span);
        self.errors.push(error);
    }

    fn unclosed_delim(&mut self, group: Group) {
        let span = group.span();
        let error = LexError::UnclosedDelim(span);
        self.errors.push(error);
    }

    fn trailing_delim(&mut self) {
        let span = self.lexer.span_char();
        let error = LexError::TrailingDelim(span);
        self.errors.push(error);
    }
}

pub fn tokenize(input: &str) -> (TokenBuffer, Vec<LexError>) {
    let lexer = Lexer::new(input);
    let mut tokenizer = Tokenizer::new(lexer);

    tokenizer.tokenize();
    tokenizer.handle_unclosed_groups();

    let tokens = TokenBuffer::from_vec(tokenizer.tokens);
    let errors = tokenizer.errors;
    (tokens, errors)
}

mod lexing {
    use crate::{name::Name, parsing::{lexer::{Ident, LexToken, Punct, Tokenizer}, token::Literal}};

    pub fn punct(tokenizer: &mut Tokenizer) -> LexToken {
        let start = tokenizer.lexer.mark();
        let ch = tokenizer.lexer.next().expect("calling code must ensure remaining characters exist");
        let span = tokenizer.lexer.span_from(start);
        LexToken::Punct(Punct { name: Name::new(&ch.to_string()), span })
    }

    pub fn ident(tokenizer: &mut Tokenizer) -> LexToken {
        let start = tokenizer.lexer.mark();
        let ident_str = tokenizer.lexer.eat_while(|ch| ch.is_alphanumeric() || ch == '_');
        let span = tokenizer.lexer.span_from(start);
        LexToken::Ident(Ident { name: Name::new(ident_str), span })
    }

    pub fn literal(tokenizer: &mut Tokenizer) -> Option<LexToken> {
        let start = tokenizer.lexer.mark();
        let number_str = tokenizer.lexer.eat_while(|ch| ch.is_ascii_digit() || ch == '.');
        let span = tokenizer.lexer.span_from(start);
        if let Ok(num) = number_str.parse::<f64>() {
            Some(LexToken::Literal(Literal { num, span }))
        } else {
            tokenizer.num_parse_error(span);
            None
        }
    }
}
