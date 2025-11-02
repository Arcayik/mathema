use std::{error::Error, fmt::Display};

use crate::parsing::token::Span;

pub trait Diagnostic: Error {
    fn message(&self) -> String;
    fn source_code<'a>(&'a self) -> Option<Box<dyn Display + 'a>>;
    fn spans(&self) -> Option<Box<dyn Iterator<Item = Span>>>;
}

