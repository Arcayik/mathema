use mathema_core::{
    parsing::{
        token::{Span, Spanned},
        lexer::LexError,
        parser::ParseError,
    },
};

#[derive(Debug)]
pub struct Diagnostic {
    pub(crate) msg: String,
    pub(crate) spans: Vec<Span>
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{}", self.msg)
    }
}

impl From<LexError> for Diagnostic {
    fn from(value: LexError) -> Self {
        Diagnostic {
            msg: value.to_string(),
            spans: vec![value.span()]
        }
    }
}

impl From<ParseError> for Diagnostic {
    fn from(value: ParseError) -> Self {
        Diagnostic {
            msg: value.to_string(),
            spans: vec![value.span()] }
    }
}

impl Diagnostic {
    pub fn from_vec<T: Into<Diagnostic>>(vec: Vec<T>) -> Vec<Self> {
        vec.into_iter().map(|d| d.into()).collect()
    }

    pub fn highlight_span(&self, offset: usize) {
        let mut last_end = 0;
        for span in self.spans.iter() {
            let start = span.start + offset;
            let end = span.end + offset;

            (last_end..start).for_each(|_| print!(" "));
            (start..end).for_each(|_| print!("^"));

            last_end = end;
        }
        println!();
    }
}
