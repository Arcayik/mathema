pub mod algebra;
pub mod context;
pub mod intrinsics;
#[macro_use]
pub mod parsing;

mod name;
pub use name::Name;

#[cfg(test)]
mod tests {
    use crate::{
        parsing::{
            lexer::tokenize,
            parser::ParseBuffer, 
            ast::Stmt,
        }
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
