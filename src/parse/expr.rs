use crate::Token;
use super::{
    token::{End, Ident, Literal, Parse, Span, Spanned, Token},
    parser::{ParseStream, Result},
};

pub enum Expr {
    Value(ExprValue),
    Binary(ExprBinary),
    Unary(ExprUnary),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(t) => write!(f, "{:?}", t),
            Self::Binary(t) => write!(f, "{:?}", t),
            Self::Unary(t) => write!(f, "{:?}", t),
        }
    }
}

impl From<ExprValue> for Expr {
    fn from(value: ExprValue) -> Self {
        Expr::Value(value)
    }
}

impl From<ExprBinary> for Expr {
    fn from(value: ExprBinary) -> Self {
        Expr::Binary(value)
    }
}

impl From<ExprUnary> for Expr {
    fn from(value: ExprUnary) -> Self {
        Expr::Unary(value)
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Value(e) => e.span(),
            Self::Binary(e) => e.span(),
            Self::Unary(e) => e.span(),
        }
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        let lhs = parsing::value_or_unary(input)?;
        parsing::parse_expr(input, lhs, Precedence::MIN)
    }
}

pub enum ExprValue {
    Literal(Literal),
    Ident(Ident),
}

impl std::fmt::Debug for ExprValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{}", l.num),
            Self::Ident(i) => write!(f, "{}", i.repr)
        }
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

impl Parse for ExprValue {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<Literal>() {
            input.parse().map(ExprValue::Literal)
        } else if input.peek::<Ident>() {
            input.parse().map(ExprValue::Ident)
        } else {
            Err(input.error("Expected literal or ident"))
        }
    }
}

pub struct ExprBinary {
    pub(super) lhs: Box<Expr>,
    pub(super) op: BinOp,
    pub(super) rhs: Box<Expr>,
}

impl std::fmt::Debug for ExprBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {:?} {:?})", self.lhs, self.op, self.rhs)
    }
}

impl Spanned for ExprBinary {
    fn span(&self) -> Span {
        let start = self.lhs.span().start;
        let end = self.rhs.span().end;
        Span { start, end }
    }
}

impl Parse for ExprBinary {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ExprBinary {
            lhs: Box::new(input.parse()?),
            op: input.parse()?,
            rhs: Box::new(input.parse()?),
        })
    }
}

pub enum BinOp {
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),
}

impl std::fmt::Debug for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(_) => write!(f, "{}", <Token![+]>::display()),
            Self::Sub(_) => write!(f, "{}", <Token![-]>::display()),
            Self::Mul(_) => write!(f, "{}", <Token![*]>::display()),
            Self::Div(_) => write!(f, "{}", <Token![/]>::display()),
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

impl Parse for BinOp {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<Token![+]>() {
            input.parse().map(BinOp::Add)
        } else if input.peek::<Token![-]>() {
            input.parse().map(BinOp::Sub)
        } else if input.peek::<Token![*]>() {
            input.parse().map(BinOp::Mul)
        } else if input.peek::<Token![/]>() {
            input.parse().map(BinOp::Div)
        } else {
            Err(input.error("Expected binary operator"))
        }
    }
}

pub struct ExprUnary {
    pub(super) op: UnaryOp,
    pub(super) expr: Box<Expr>,
}

impl std::fmt::Debug for ExprUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {:?})", self.op, self.expr)
    }
}

impl Spanned for ExprUnary {
    fn span(&self) -> Span {
        let start = self.op.span().start;
        let end = self.expr.span().end;
        Span { start, end }
    }
}

impl Parse for ExprUnary {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ExprUnary {
            op: input.parse()?,
            expr: Box::new(parsing::value_or_unary(input)?),
        })
    }
}

pub enum UnaryOp {
    Neg(Token![-])
}

impl std::fmt::Debug for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Neg(_) => write!(f, "{}", <Token![-]>::display()),
        }
    }
}

impl Spanned for UnaryOp {
    fn span(&self) -> Span {
        match self {
            Self::Neg(t) => t.span(),
        }
    }
}

impl Parse for UnaryOp {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<Token![-]>() {
            input.parse().map(UnaryOp::Neg)
        } else {
            Err(input.error("Expected unary operator"))
        }
    }
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence { Assign, Sum, Product, Unary, Unambiguous }

impl Precedence {
    pub(super) const MIN: Self = Precedence::Assign;

    pub fn of_binop(op: &BinOp) -> Self {
        match op {
            BinOp::Add(_) | BinOp::Sub(_) => Precedence::Sum,
            BinOp::Mul(_) | BinOp::Div(_) => Precedence::Product
        }
    }

    pub fn of(e: &Expr) -> Self {
        match e {
            Expr::Value(_) => Precedence::Unambiguous,
            Expr::Binary(b) => Precedence::of_binop(&b.op),
            Expr::Unary(_) => Precedence::Unary,
        }
    }
}

mod parsing {
    use super::*;

    pub fn value_or_unary(input: ParseStream) -> Result<Expr> {
        if input.peek::<Token![-]>() {
            input.parse().map(Expr::Unary)
        } else if input.peek::<Literal>() || input.peek::<Ident>() {
            input.parse().map(Expr::Value)
        } else {
            Err(input.error("Expected ident, literal, or unary operator"))
        }
    }

    pub fn parse_expr(
        input: ParseStream,
        mut left: Expr,
        base: Precedence
    ) -> Result<Expr> {
            // [1 + 2 *] 10
        loop {
            if input.peek::<End>() {
                break;
            }

            let begin = input.save_pos();

            let op = input.parse()?;
            // [1 +] 2 * 10
            let precedence = Precedence::of_binop(&op);

            if precedence < base {
                input.restore_pos(begin);
                break;
            } else { // ex: {1 / 2} [+ 9] Product:2 > Sum:1
                left = ExprBinary {
                    lhs: Box::new(left),
                    op,
                    // [1 +] 2 * 10
                    rhs: parse_binop_rhs(input, precedence)?,
                }.into();
            }
        }
        Ok(left)
    }

    fn parse_binop_rhs(
        input: ParseStream,
        precedence: Precedence,
    ) -> Result<Box<Expr>> {
        let mut rhs = value_or_unary(input)?;
        // [1 + 2] * 10
        loop {
            let begin = input.save_pos();
            let next = peek_precedence(input);
            // [1 + 2 *] 10

            if next >= precedence {
                // [1 + 2 *] 10
                rhs = parse_expr(input, rhs, next)?;
            } else {
                input.restore_pos(begin);
                break;
            }
        }
        Ok(Box::new(rhs))
    }

    fn peek_precedence(input: ParseStream) -> Precedence {
        let begin = input.save_pos();

        let precedence = if let Ok(op) = input.parse() {
            Precedence::of_binop(&op)
        } else if input.peek::<Token![=]>() {
            Precedence::Assign
        } else {
            Precedence::MIN
        };

        input.restore_pos(begin);
        precedence
    }
}
