pub mod algebra;
pub mod context;
pub mod intrinsics;
pub mod function;
pub mod symbol;
pub mod error;
#[macro_use]
pub mod parsing;

#[cfg(test)]
mod tests {
    use crate::{
        algebra::ast::{expr_to_algebra, AlgebraTree},
        parsing::{
            ast::AstExpr,
            lexer::tokenize,
            parser::ParseBuffer
        }
    };

    fn parse(input: &'static str) -> AlgebraTree {
        let (tokens, errors) = tokenize(input);
        if !errors.is_empty() {
            panic!("tokenize failed");
        }
        let parser = ParseBuffer::new(tokens);
        let expr = parser.parse::<AstExpr>().unwrap();
        let alg = expr_to_algebra(&expr);
        alg
    }

    fn alg_str(input: &'static str) -> String {
        let alg = parse(input);
        todo!()
        // algebra_to_string(&alg).source
    }

    #[test]
    fn stringify() {
        assert_eq!(alg_str("1+2"), "1 + 2");
        assert_eq!(alg_str("1+2*3+4"), "1 + 2 * 3 + 4");
        assert_eq!(alg_str("(1+2)*(3+4)"), "(1 + 2) * (3 + 4)");
        assert_eq!(alg_str("5 ^ 2+1"), "5 ^ 2 + 1");
        assert_eq!(alg_str("2^(3*(4+5))"), "2 ^ (3 * (4 + 5))");

        assert_eq!(alg_str("-2"), "-2");
        assert_eq!(alg_str("-(6-7)"), "-(6 - 7)");
        assert_eq!(alg_str("-(3--8)"), "-(3 - -8)");

        assert_eq!(alg_str("2var_name"), "2var_name");
        assert_eq!(alg_str("5sin(theta)"), "5sin(theta)");
        assert_eq!(alg_str("(13x)f(2y)"), "13x * f(2y)");
    }
}
