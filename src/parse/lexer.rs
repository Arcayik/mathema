use crate::parse::token::*;

use super::token::Span;

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

    pub fn process(&mut self) {
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

                '+' | '-' | '*' | '/' | '=' => {
                    return Some(self.punct())
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
                    return Some(self.ident())
                },
                ch if ch.is_ascii_digit() => {
                    let literal = self.literal();
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

    pub fn process_unclosed_groups(&mut self) {
        while let Some(unclosed_idx) = self.delim_stack.pop() {
            let token = self.tokens.get(unclosed_idx)
                .expect("delim stack must have valid group start index");
            let group = match token {
                LexToken::Group(g) => g,
                _ => panic!("delim stack index does not point to group token")
            };
            self.unclosed_delim(group.clone());
        }
    }

    fn punct(&mut self) -> LexToken {
        let start = self.lexer.mark();
        let ch = self.lexer.next().expect("calling code must ensure remaining characters exist");
        let span = self.lexer.span_from(start);
        LexToken::Punct(Punct { repr: ch.to_string().into(), span })
    }

    fn ident(&mut self) -> LexToken {
        let start = self.lexer.mark();
        let ident_str = self.lexer.eat_while(|ch| ch.is_alphanumeric() || ch == '_');
        let span = self.lexer.span_from(start);
        LexToken::Ident(Ident { repr: ident_str.into(), span })
    }

    fn literal(&mut self) -> Option<LexToken> {
        let start = self.lexer.mark();
        let number_str = self.lexer.eat_while(|ch| ch.is_ascii_digit() || ch == '.');
        let span = self.lexer.span_from(start);
        if let Ok(num) = number_str.parse::<f64>() {
            Some(LexToken::Literal(Literal { num, span }))
        } else {
            self.num_parse_error(span);
            None
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
        LexToken::Group(group)
    }

    fn close_group(&mut self) -> Option<LexToken> {
        let ch = self.lexer.next()
            .expect("calling code must ensure remaining characters exist");

        let top_idx = self.pop_group()?;
        let top_group = self.tokens.get(top_idx)
            .expect("delim stack must have valid group start index");
        let top_delim = match top_group {
            LexToken::Group(g) => g.delim,
            _ => return None
        };

        let delim = match ch {
            ')' => Delimiter::Parenthesis,
            '}' => Delimiter::Brace,
            ']' => Delimiter::Bracket,
            _ => panic!("not a valid delimiter"),
        };

        if top_delim == delim {
            let offset = top_idx as isize - self.tokens.len() as isize;
            Some(LexToken::End(offset))
        } else {
            self.trailing_delim();
            None
        }

    }

    fn push_group(&mut self) {
        self.delim_stack.push(self.tokens.len())
    }

    fn pop_group(&mut self) -> Option<usize> {
        self.delim_stack.pop()
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

    pub fn trailing_delim(&mut self) {
        let span = self.lexer.span_char();
        let error = LexError::TrailingDelim(span);
        self.errors.push(error);
    }
}

pub struct Lexer<'s> {
    /// Trimmed input
    source: &'s str,
    /// Iterator over input
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

pub fn tokenize(input: &str) -> (Box<[LexToken]>, Vec<LexError>) {
    let lexer = Lexer::new(input);
    let mut tokenizer = Tokenizer::new(lexer);

    tokenizer.process();
    tokenizer.process_unclosed_groups();

    (tokenizer.tokens.into(), tokenizer.errors)
}
