use crate::{context::Context, parse::{expr::*, token::Ident}};

#[derive(Debug)]
pub struct EvalError {
    pub undefined: Vec<Ident>
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Undefined variable{}: {}",
            if self.undefined.len() > 1 { "s" } else { "" },
            self.undefined
            .iter()
            .map(|ident| ident.repr.clone())
            .collect::<Vec<_>>()
            .join(", "),
        )
    }
}

impl EvalError {
    fn new(ident: Ident) -> Self {
        EvalError {
            undefined: vec![ident]
        }
    }

    fn combine(left: Result<f64>, right: Result<f64>) -> Option<Self> {
        match (left, right) {
            (Err(mut l), Err(mut r)) => {
                l.undefined.append(&mut r.undefined);
                Some(l)
            },
            (Err(e), _) | (_, Err(e)) => Some(e),
            _ => None
        }
    }
}

pub type Result<T> = std::result::Result<T, EvalError>;

pub trait AstNode {
    fn eval(&self, ctxt: &Context) -> Result<f64>;
}

impl AstNode for Expr {
    fn eval(&self, ctxt: &Context) -> Result<f64> {
        match self {
            Self::Value(value) => value.eval(ctxt),
            Self::Binary(expr) => expr.eval(ctxt),
            Self::Unary(expr) => expr.eval(ctxt),
        }
    }
}

impl AstNode for ExprValue {
    fn eval(&self, ctxt: &Context) -> Result<f64> {
        match self {
            Self::Ident(ident) => try_get_variable(ctxt, ident),
            Self::Literal(literal) => Ok(literal.num),
        }
    }
}

fn try_get_variable(ctxt: &Context, ident: &Ident) -> Result<f64> {
    match ctxt.get_variable(ident.repr.clone()) {
        Some(num) => Ok(num),
        None => Err(EvalError::new(ident.clone()))
    }
}

impl AstNode for ExprBinary {
    fn eval(&self, ctxt: &Context) -> Result<f64> {
        let left = self.lhs.eval(ctxt);
        let right = self.rhs.eval(ctxt);
        if let (Ok(l), Ok(r)) = (&left, &right) {
            match self.op {
                BinOp::Add(_) => Ok(l + r),
                BinOp::Sub(_) => Ok(l - r),
                BinOp::Mul(_) => Ok(l * r),
                BinOp::Div(_) => Ok(l / r),
            }
        } else {
            Err(EvalError::combine(left, right).unwrap())
        }
    }
}

impl AstNode for ExprUnary {
    fn eval(&self, ctxt: &Context) -> Result<f64> {
        let right = self.expr.eval(ctxt);
        if let Ok(r) = right {
            match self.op {
                UnaryOp::Neg(_) => Ok(-r),
            }
        } else {
            right
        }
    }
}
