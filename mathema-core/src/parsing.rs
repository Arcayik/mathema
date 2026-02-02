#[macro_use]
pub mod token;
pub mod lexer;
pub mod parser;
pub mod ast;
pub mod punctuated;

#[cfg(test)]
mod tests {
    use crate::{
        parsing::{
            ast::{AstBinary, AstExpr, AstFnCall, AstGroup, AstUnary, AstValue, BinOp, UnaryOp}, lexer::tokenize, parser::{ParseBuffer, ParseError}, punctuated::Punctuated, token::{Caret, DelimKind, Ident, Literal, Minus, Plus, Span, Spanned, Star}
        },
        symbol::Symbol
    };

    fn parse(input: &str) -> Result<AstExpr, ParseError> {
        let buffer = tokenize(input).unwrap();
        let parser = ParseBuffer::new(buffer);
        parser.parse()
    }

    fn fail_spans(input: &str) -> Vec<Span> {
        match tokenize(input) {
            // check if any errors were expected at fail span
            Err(e) => if !e.is_empty() {
                return e.iter().map(|e| e.span()).collect()
            } else {
                panic!("unexpected error")
            },

            Ok(buf) => {
                let parser = ParseBuffer::new(buf);
                let result = parser.parse::<AstExpr>();
                if let Err(e) = result {
                    vec![e.span()]
                } else {
                    Vec::new()
                }
            }
        }

    }

    fn fails_at(input: &str, fail: impl Into<Span>) -> bool {
        let fail = fail.into();
        fail_spans(input).contains(&fail)
    }

    const EMPTY: Span = Span { start: 0, end: 0 };

    fn lit(num: f64) -> AstExpr {
        AstValue::Literal(Literal { num, span: EMPTY }).into()
    }

    fn var(name: &'static str) -> AstExpr {
        AstValue::Ident(Ident { symbol: Symbol::intern(name), span: EMPTY }).into()
    }

    fn add(lhs: AstExpr, rhs: AstExpr) -> AstExpr {
        let op = BinOp::Add(Plus { span: EMPTY });
        AstBinary { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }.into()
    }

    fn mul(lhs: AstExpr, rhs: AstExpr) -> AstExpr {
        let op = BinOp::Mul(Some(Star { span: EMPTY }));
        AstBinary { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }.into()
    }

    fn impl_mul(lhs: AstExpr, rhs: AstExpr) -> AstExpr {
        let op = BinOp::Mul(None);
        AstBinary { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }.into()
    }

    fn exp(lhs: AstExpr, rhs: AstExpr) -> AstExpr {
        let op = BinOp::Exp(Caret { span: EMPTY });
        AstBinary { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }.into()
    }

    fn neg(expr: AstExpr) -> AstExpr {
        let op = UnaryOp::Neg(Minus { span: EMPTY });
        AstUnary { op, expr: Box::new(expr) }.into()
    }

    fn group(expr: AstExpr) -> AstExpr {
        AstGroup { delim_kind: DelimKind::Parenthesis, expr: Box::new(expr) }.into()
    }

    fn func(name: &'static str) -> AstExpr {
        AstFnCall {
            fn_name: Ident { symbol: Symbol::intern(name), span: EMPTY },
            l_paren: super::token::LParen { span: EMPTY },
            inputs: Punctuated::new(),
            r_paren: super::token::RParen { span: EMPTY },
        }.into()
    }

    #[test]
    fn values() {
        assert_eq!(parse("1").unwrap(), lit(1.0).into());
        assert_eq!(parse("1.0").unwrap(), lit(1.0).into());
        assert_eq!(parse("3.14159265358979323").unwrap(), lit(3.14159265358979323).into());
        assert_eq!(parse("x").unwrap(), var("x").into());
        assert_eq!(parse("ident").unwrap(), var("ident").into());
        assert_eq!(parse("long_ident").unwrap(), var("long_ident").into());
        assert_eq!(parse("_long_ident_").unwrap(), var("_long_ident_").into());
        assert!(parse(" ").is_err());
        assert_eq!(parse("works55").unwrap(), var("works55").into());
        assert_eq!(parse("b_3").unwrap(), var("b_3").into());
    }

    #[test]
    fn precedence() {
        let expected = add( lit(1.0), mul( lit(3.0), lit(5.0) ) ).into();
        assert_eq!(parse("1 + 3 * 5").unwrap(), expected);

        let expected = add( mul( lit(1.0), lit(3.0) ), lit(5.0) ).into();
        assert_eq!(parse("1 * 3 + 5").unwrap(), expected);

        let expected = add( lit(1.2), mul( lit(3.4), exp( lit(5.6), lit(7.89) ) ) ).into();
        assert_eq!(parse("1.2 + 3.40 * 5.6 ^ 7.89").unwrap(), expected);

        let expected = add( lit(20.0), impl_mul(lit(40.0), var("x")) );
        assert_eq!(parse("20+40x").unwrap(), expected);

        let expected = add( lit(3.0), impl_mul( lit(4.0), func("f") ) );
        assert_eq!(parse("3+4f(x)").unwrap(), expected);
    }

    #[test]
    fn unexpected_and_trailing() {
        assert!(fails_at("9.98 /", (5, 6)));
        assert!(fails_at("1 + +", (4, 5)));
        assert!(fails_at("(1 + 3)*", (7, 8)));
        assert!(fails_at("(6-7)/(9/)", (8, 9)));
        assert!(fails_at("1 3 * 5 var", (2, 3)));
    }

    #[test]
    fn simple_unary() {
        assert_eq!(parse("1 + -2").unwrap(), add( lit(1.0), neg(lit(2.0) ) ));
        assert_eq!(parse("-4 + -1").unwrap(), add( neg(lit(4.0)), neg(lit(1.0)) ));
        assert_eq!(parse("--4 + -1").unwrap(), add( neg(neg(lit(4.0))), neg(lit(1.0)) ));
    }

    #[test]
    fn parens() {
        assert_eq!(parse("(59.9)").unwrap(), group(lit(59.9)));
        assert_eq!(parse("1 + (1)").unwrap(), add( lit(1.0), group(lit(1.0)) ));
        let expected = group(mul(
                group(add(
                        lit(1.0),
                        lit(1.0)
                )),
                lit(30.0)
        ));
        assert_eq!(parse("((1+1) * 30)").unwrap(), expected);
        assert!(fails_at("()", (1, 2)));
        // TODO: trailing delimiter checks
        // assert!(fails_at("(2*4) )", (6, 7)));
    }

    #[test]
    fn implied_multiplication() {
        let expected = impl_mul(
            impl_mul(
                mul(
                    lit(1.0),
                    lit(3.0),
                ),
                var("x"),
            ),
            var("y")
        );
        assert_eq!(parse("1 * 3 x y").unwrap(), expected);
        assert_eq!(parse("3(4.2x)").unwrap(), impl_mul(lit(3.0), group(impl_mul(lit(4.2), var("x")))) )
    }
}
