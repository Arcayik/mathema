use crate::{parse::parser::ParseError, Token};
use super::{token::*, parser::ParseStream};

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
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let lhs: Expr = if input.peek::<Ident>() || input.peek::<Literal>() {
            input.parse::<ExprValue>()?.into()
        } else if peek_unary_op(input) {
            input.parse::<ExprUnary>()?.into()
        } else if peek_binop(input) {
            input.parse::<ExprBinary>()?.into()
        } else {
            return Err(input.error("Expected ident, literal, or unary operator"))
        };

        parsing::parse_expr(input, lhs)
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
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
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
    lhs: Box<Expr>,
    op: BinOp,
    rhs: Box<Expr>,
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
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
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
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
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

fn peek_binop(input: ParseStream) -> bool {
    input.peek::<Token![+]>()
        || input.peek::<Token![-]>()
        || input.peek::<Token![*]>()
        || input.peek::<Token![/]>()
}

pub struct ExprUnary {
    op: UnaryOp,
    rhs: Box<Expr>,
}

impl std::fmt::Debug for ExprUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {:?})", self.op, self.rhs)
    }
}

impl Spanned for ExprUnary {
    fn span(&self) -> Span {
        let start = self.op.span().start;
        let end = self.rhs.span().end;
        Span { start, end }
    }
}

impl Parse for ExprUnary {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        Ok(ExprUnary {
            op: input.parse()?,
            rhs: Box::new(parsing::parse_value_or_unary(input)?.into()),
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
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        if input.peek::<Token![-]>() {
            input.parse().map(UnaryOp::Neg)
        } else {
            Err(input.error("Expected unary operator"))
        }
    }
}

fn peek_unary_op(input: ParseStream) -> bool {
    input.peek::<Token![-]>()
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence { Sum, Product, Unary, Unambiguous }

impl Precedence {
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

    pub fn parse_value_or_unary(input: ParseStream) -> Result<Expr, ParseError> {
        if peek_unary_op(input) {
            input.parse().map(Expr::Unary)
        } else {
            input.parse().map(Expr::Value)
        }
    }

    pub fn parse_expr(input: ParseStream, prev: Expr) -> Result<Expr, ParseError> {
        // println!("parse_expr(): {:#?}", prev);

        if input.peek::<End>() {
            // println!("reached end");
            return Ok(prev)
        } else if !peek_binop(input) {
            return Err(input.error("Expected end or binary operator"))
        }

        // Unary operator not expected here
        let op: BinOp = input.parse()?;
        let rhs: Expr = parse_value_or_unary(input)?;

        match prev {
            Expr::Value(value) => {
                let curr_expr = ExprBinary {
                    lhs: Box::new(value.into()),
                    op,
                    rhs: Box::new(rhs.into()),
                }.into();
                // println!("match prev => Expr::Value");
                parse_expr(input, curr_expr)
            },

            /* 
             * (1 + 2) [* 3] => L < R => (1 + (2 * 3))
             * ROTATE: new becomes old's rhs
             * (1 * 2) [+ 3] => L > R => ((1 * 2) + 3)
             * old becomes new's lhs
             */
            Expr::Binary(mut expr) => {
                if Precedence::of_binop(&expr.op) < Precedence::of_binop(&op) {
                    let new_expr = ExprBinary {
                        lhs: expr.rhs,
                        op,
                        rhs: Box::new(rhs.into()),
                    }.into();

                    expr.rhs = Box::new(new_expr);
                    parse_expr(input, expr.into())
                } else {
                    let top_expr = ExprBinary {
                        lhs: Box::new(expr.into()),
                        op,
                        rhs: Box::new(rhs.into()),
                    }.into();

                    // println!("match prev => Expr::Binary");
                    parse_expr(input, top_expr)
                }
            },

            Expr::Unary(expr) => {
                // TODO: precedence check
                let new_expr = ExprBinary {
                    lhs: Box::new(expr.into()),
                    op,
                    rhs: Box::new(rhs.into()),
                }.into();

                // println!("match prev => Expr::Unary");
                parse_expr(input, new_expr)
            }
        }
    }

}
