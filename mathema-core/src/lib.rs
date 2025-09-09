#[macro_use]
mod token;
mod lexer;
mod parser;
mod stmt;
mod expr;
mod punctuated;
mod intrinsics;

mod algebra;
mod function;
mod context;
mod diagnostic;

pub mod parsing {
    pub use crate::token::{Span, Spanned, Token, Parse};
    pub use crate::lexer::tokenize;
    pub use crate::parser::{ParseBuffer, ParseError, ParseStream};
    pub use crate::expr::{BinOp, Expr, ExprBinary, ExprUnary, ExprValue, ExprFnCall, ExprGroup, Precedence, UnaryOp};
    pub use crate::stmt::Stmt;
}

pub use function::{Function, FunctionError, create_function};
pub use context::{Context, Outcome, process_statement};
pub use diagnostic::Diagnostic;

#[cfg(test)]
mod tests {
    use crate::{
        lexer::tokenize,
        parser::ParseBuffer, 
        stmt::Stmt,
    };

    #[test]
    fn sum_product_precedence() {
        assert!(parse_test("1 + 1"));
        assert!(parse_test("1 + 3 * 5"));
        assert!(parse_test("1 * 3 + 5"));
    }

    #[test]
    fn unexpected_and_trailing() {
        assert!(!parse_test("1 + +"));
        assert!(!parse_test("1 + 3 * 5 var"));
        assert!(!parse_test("1 / 3 x y"));
        assert!(!parse_test("1 3 * 5 var"));
    }

    #[test]
    fn simple_unary() {
        assert!(parse_test("1 + -1"));
        assert!(parse_test("-4 + -1"));
        assert!(parse_test("--4 + -1"));
    }

    #[test]
    fn parens() {
        assert!(!parse_test("()"));
        assert!(!parse_test("(2*4) )"));
        assert!(parse_test("(59.9)"));
        assert!(parse_test("1 + (1)"));
        assert!(parse_test("((1+1) * 30)"));
    }

    #[test]
    fn var_declaration() {
        assert!(parse_test("x = 50"));
    }

    fn parse_test(input: &str) -> bool {
        let input = String::from(input);
        let (tokens, errors) = tokenize(&input);
        if !errors.is_empty() {
            errors.iter().for_each(|e| println!("{e}"));
            return false;
        } 

        let parser = ParseBuffer::new(tokens);
        let ast = parser.parse::<Stmt>();

        println!("Input: [{}]", input);
        println!("AST: \n{:#?}", ast);
        ast.is_ok()
    }
}
