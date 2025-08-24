use crate::parse::{Span, Spanned};

#[derive(Debug)]
pub struct Punctuated<T, S> {
    list: Vec<(T, S)>,
    end: Option<Box<T>>,
}

impl<T, S> Punctuated<T, S> {
    pub const fn new() -> Self {
        Punctuated {
            list: Vec::new(),
            end: None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty() && self.end.is_none()
    }

    pub fn len(&self) -> usize {
        self.list.len() + if self.end.is_some() { 1 } else { 0 }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if let Some((value, _punct)) = self.list.get(index) {
            Some(value)
        } else if index == self.list.len() {
            self.end.as_deref()
        } else {
            None
        }
    }

    pub fn push_value(&mut self, value: T) {
        assert!(
            self.empty_or_trailing(),
            "Punctuated::push_value: cannot push value if Punctuated is missing trailing separator"
        );

        self.end = Some(Box::new(value))
    }

    pub fn push_separator(&mut self, separator: S) {
        assert!(
            self.end.is_some(),
            "Punctuated::push_separator: cannot push separator if Punctuated is empty or already has a trailing separator"
        );

        let last = self.end.take().unwrap();
        self.list.push((*last, separator));
    }

    pub fn trailing_punct(&self) -> bool {
        self.end.is_none() && !self.is_empty()
    }

    pub fn empty_or_trailing(&self) -> bool {
        self.end.is_none()
    }

    pub fn iter(&self) -> PunctIter<'_, T, S> {
        PunctIter {
            list: self.list.iter(),
            end: self.end.as_ref().map(Box::as_ref).into_iter()
        }
    }
}

impl<T: Spanned, S: Spanned> Punctuated<T, S> {
    pub fn try_span(&self) -> Option<Span> {
        let first_span = self.list.first()?.0.span();
        let last_span = match &self.end {
            Some(value) => value.span(),
            None => self.list.last().unwrap().1.span()
        };
        Some(Span {
            start: first_span.start,
            end: last_span.end
        })
    }
}

pub struct PunctIter<'a, T, S> {
    list: std::slice::Iter<'a, (T, S)>,
    end: std::option::IntoIter<&'a T>
}

impl<'a, T, S> Iterator for PunctIter<'a, T, S> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.list
            .next()
            .map(|pair| &pair.0)
            .or_else(|| self.end.next())
    }
}

impl<'a, T, P> DoubleEndedIterator for PunctIter<'a, T, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.end
            .next()
            .or_else(|| self.list.next_back().map(|pair| &pair.0))
    }
}

impl<'a, T, P> ExactSizeIterator for PunctIter<'a, T, P> {
    fn len(&self) -> usize {
        self.list.len() + self.end.len()
    }
}
