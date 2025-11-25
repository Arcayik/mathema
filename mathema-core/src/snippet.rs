use std::collections::{hash_map::{Values, ValuesMut}, HashMap};

use crate::{algebra::AlgExpr, parsing::token::Span};

pub struct SnippetLine {
    pub(crate) source: String,
    pub(crate) span_map: HashMap<*const AlgExpr, Span>
}

impl SnippetLine {
    pub fn empty() -> Self {
        SnippetLine { 
            source: String::new(),
            span_map: HashMap::new()
        }
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn get_span(&self, key: &AlgExpr) -> Option<Span> {
        let key = key as *const AlgExpr;
        self.span_map.get(&key).copied()
    }

    pub fn spans(&self) -> Values<'_, *const AlgExpr, Span> {
        self.span_map.values()
    }

    pub fn spans_mut(&mut self) -> ValuesMut<'_, *const AlgExpr, Span> {
        self.span_map.values_mut()
    }
}
