// use std::{error::Error, fmt::Display};
//
// use crate::parsing::token::Span;
//
// pub trait Diagnostic: Error {
//     fn origin<'a>(&'a self) -> Option<Box<dyn Display + 'a>>;
//     fn message(&self) -> String;
//     fn source_code<'a>(&'a self) -> Option<Box<dyn Display + 'a>>;
//     fn spans(&self) -> Option<Box<dyn Iterator<Item = Span>>>;
// }

use crate::{algebra::EvalError, context::DefError, parsing::{lexer::LexError, parser::ParseError}};

pub enum MathemaError {
    Lexer(Vec<LexError>),
    Parser(ParseError),
    Definition(DefError),
    Eval(Vec<EvalError>),
}

/*TODO:

>> f(x) = x + a
>> f(4)
   ^^^^
f(x) = x + a
           ^
undefined var: 'a'

*/
