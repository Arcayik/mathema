pub mod algebra;
pub mod arena;
pub mod context;
pub mod error;
pub mod intrinsics;
pub mod function;
#[macro_use]
pub mod parsing;
pub mod symbol;

#[cfg(test)]
mod tests {
    use crate::{
        context::Context,
        algebra::{
            ast::{expr_to_algebra, AlgebraTree},
            display::Display
        },
        parsing::{
            ast::AstExpr,
            lexer::tokenize,
            parser::ParseBuffer
        }
    };

    fn parse(input: &'static str) -> (Context, AlgebraTree) {
        let mut ctxt = Context::default();

        let tokens = tokenize(input).unwrap();
        let parser = ParseBuffer::new(tokens);
        let expr = parser.parse::<AstExpr>().unwrap();
        let alg = expr_to_algebra(&expr, &mut ctxt.arena);
        (ctxt, alg)
    }

    fn alg_str(input: &'static str) -> String {
        let (ctxt, alg) = parse(input);
        alg.visit(&ctxt, Display)
    }

    #[test]
    fn stringify() {
        assert_eq!(alg_str("1+2"), "1 + 2");
        assert_eq!(alg_str("1+2*3+4"), "1 + 2 * 3 + 4");
        assert_eq!(alg_str("(1+2)*(3+4)"), "(1 + 2) * (3 + 4)");
        assert_eq!(alg_str("5 ^ 2+1"), "5^2 + 1");
        assert_eq!(alg_str("2^ (3*(4+5))"), "2^(3(4 + 5))");

        // unary
        assert_eq!(alg_str("-2"), "-2");
        assert_eq!(alg_str("-(6-7)"), "-(6 - 7)");
        assert_eq!(alg_str("-(3--8)"), "-(3 - -8)");

        assert_eq!(alg_str("2(-1)"), "2(-1)");
        assert_eq!(alg_str("4(2^2 * 7)"), "4 * 2^2 * 7");
        assert_eq!(alg_str("x(1+2^2)"), "x(1 + 2^2)");
        assert_eq!(alg_str("2var_name"), "2var_name");
        assert_eq!(alg_str("(13x)f(2y)"), "13x * f(2y)");
        assert_eq!(alg_str("g(n)(2*6)"), "g(n)(2 * 6)");
        assert_eq!(alg_str("succ(n)x^3"), "succ(n)x^3");
        assert_eq!(alg_str("5+2f(f(x))"), "5 + 2f(f(x))");

        assert_eq!(alg_str("2x + 2f(x,y)"), "2x + 2f(x,y)");
        assert_eq!(alg_str("2(x^2) / 10(1+1)"), "2x^2 / 10(1 + 1)");
        assert_eq!(alg_str("3(-1)(-4)"), "3 * -1 * -4");
    }
}
