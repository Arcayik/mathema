use crate::Token;
use super::{
    token::*,
};

enum Expr {
    Value(ExprValue),
    Binary(ExprBinary),
}

struct ExprBinary {
    lhs: Box<Expr>,
    op: BinOp,
    rhs: Box<Expr>,
}

enum ExprValue {
    Literal(Literal),
    Ident(Ident),
}

enum BinOp {
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Value(e) => e.span(),
            Self::Binary(e) => e.span(),
        }
    }
}

impl Spanned for ExprBinary {
    fn span(&self) -> Span {
        let start = self.lhs.span().start;
        let end = self.rhs.span().end;
        Span { start, end }
    }
}

impl Spanned for ExprValue {
    fn span(&self) -> Span {
        match self {
            Self::Literal(lit) => lit.span(),
            Self::Ident(id) => id.span(),
        }
    }
}

impl Spanned for BinOp {
    fn span(&self) -> Span {
        match self {
            Self::Add(t) => t.span(),
            Self::Sub(t) => t.span(),
            Self::Mul(t) => t.span(),
            Self::Div(t) => t.span(),
        }
    }
}
