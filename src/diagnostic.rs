use crate::parse::{EvalError, ParseError, Span, UnknownChar};

#[derive(Debug)]
pub struct Diagnostic {
    msg: String,
    spans: Vec<Span>
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{}", self.msg)
    }
}

impl From<UnknownChar> for Diagnostic {
    fn from(value: UnknownChar) -> Self {
        Diagnostic {
            msg: value.to_string(),
            spans: vec![value.1]
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

impl From<EvalError> for Diagnostic {
    fn from(value: EvalError) -> Self {
        Diagnostic {
            msg: value.to_string(),
            spans: Vec::new(),
        }
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
    }
}
