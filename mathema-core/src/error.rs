use crate::{algebra::eval::EvalError, context::DefError, parsing::{lexer::LexError, parser::ParseError}};

pub enum MathemaError {
    Lexer(Vec<LexError>),
    Parser(ParseError),
    Definition(DefError),
    Eval(Vec<EvalError>),
}

/*

>> f(x) = x + a
>> f(4)
   ^^^^
f(x) = x + a
           ^
undefined var: 'a'

*/
