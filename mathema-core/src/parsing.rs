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
            ast::{AstBinary, AstExpr, AstGroup, AstUnary, AstValue, BinOp, UnaryOp},
            lexer::tokenize,
            parser::ParseBuffer,
            token::{Caret, Delimiter, Ident, Literal, Minus, Plus, Span, Spanned, Star}
        }, symbol::Symbol
    };

    fn parse(input: &str) -> AstExpr {
        let (buffer, errors) = tokenize(input);
        if !errors.is_empty() {
            panic!("parse returned errors");
        }
        let parser = ParseBuffer::new(buffer);
        parser.parse().unwrap()
    }

    fn fail_spans(input: &str) -> Vec<Span> {
        let (buffer, errors) = tokenize(input);
        // check if any errors were expected at fail span
        if !errors.is_empty() {
            return errors.iter().map(|e| e.span()).collect()
        }
        let parser = ParseBuffer::new(buffer);
        let result = parser.parse::<AstExpr>();
        if let Err(e) = result {
            vec![e.span()]
        } else {
            Vec::new()
        }
    }

    fn fails_at(input: &str, fail: Span) -> bool {
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
        let op = BinOp::Mul(Star { span: EMPTY });
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
        AstGroup { delim: Delimiter::Parenthesis, expr: Box::new(expr) }.into()
    }

    #[test]
    fn values() {
        assert_eq!(parse("1"), lit(1.0).into());
        assert_eq!(parse("1.0"), lit(1.0).into());
        assert_eq!(parse("3.14159265358979323"), lit(3.14159265358979323).into());
        assert_eq!(parse("x"), var("x").into());
        assert_eq!(parse("ident"), var("ident").into());
        assert_eq!(parse("long_ident"), var("long_ident").into());
        assert_eq!(parse("_long_ident_"), var("_long_ident_").into());
        // TODO FIX: This overflows the stack!!!
        // assert_eq!(parse(" "), var(" ").into());
        assert_eq!(parse("3fail"), lit(3.0).into());
        assert_eq!(parse("works55"), var("works55").into());
        assert_eq!(parse("b_3"), var("b_3").into());
    }

    #[test]
    fn precedence() {
        let expected = add( lit(1.0), mul( lit(3.0), lit(5.0) ) ).into();
        assert_eq!(parse("1 + 3 * 5"), expected);

        let expected = add( mul( lit(1.0), lit(3.0) ), lit(5.0) ).into();
        assert_eq!(parse("1 * 3 + 5"), expected);

        let expected = add( lit(1.2), mul( lit(3.4), exp( lit(5.6), lit(7.89) ) ) ).into();
        assert_eq!(parse("1.2 + 3.40 * 5.6 ^ 7.89"), expected);
    }

    #[test]
    fn unexpected_and_trailing() {
        assert!(fails_at("1 + +", (4, 5).into()));
        // TODO: fix trailing exprs only handled on Stmt parse
        // assert!(fails_at("1 + 3 * 5 var", (10, 13).into()));
        // assert!(fail_spans("1 / 3 x y").is_empty());
        // assert!(fail_spans("1 3 * 5 var").is_empty());
    }

    #[test]
    fn simple_unary() {
        assert_eq!(parse("1 + -1"), add( lit(1.0), neg(lit(1.0) ) ));
        assert_eq!(parse("-4 + -1"), add( neg(lit(4.0)), neg(lit(1.0)) ));
        assert_eq!(parse("--4 + -1"), add( neg(neg(lit(4.0))), neg(lit(1.0)) ));
    }

    #[test]
    fn parens() {
        assert_eq!(parse("(59.9)"), group(lit(59.9)));
        assert_eq!(parse("1 + (1)"), add( lit(1.0), group(lit(1.0)) ));
        let expected = group(mul(
                group(add(
                        lit(1.0),
                        lit(1.0)
                )),
                lit(30.0)
        ));
        assert_eq!(parse("((1+1) * 30)"), expected);
        assert!(fails_at("()", (0, 2).into()));
        assert!(fails_at("(2*4) )", (6, 7).into()));
    }

    #[test]
    fn var_declaration() {
    }
}
