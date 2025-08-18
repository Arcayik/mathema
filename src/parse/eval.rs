use crate::parse::expr::*;

#[derive(Debug)]
pub struct EvalError {
    undefined: Vec<Box<str>>
}

impl ToString for EvalError {
    fn to_string(&self) -> String {
        format!("Undefined variable{}: {}",
            if self.undefined.len() > 1 { "s" } else { "" },
            self.undefined.join(", "),
        )
    }
}

impl EvalError {
    fn new(ident: Box<str>) -> Self {
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
    fn eval(&self) -> Result<f64>;
}

impl AstNode for Expr {
    fn eval(&self) -> Result<f64> {
        match self {
            Self::Value(value) => value.eval(),
            Self::Binary(expr) => expr.eval(),
            Self::Unary(expr) => expr.eval(),
        }
    }
}

impl AstNode for ExprValue {
    fn eval(&self) -> Result<f64> {
        match self {
            // TODO: variable table
            Self::Ident(ident) => Err(EvalError::new(ident.repr.clone())),
            Self::Literal(literal) => Ok(literal.num),
        }
    }
}

impl AstNode for ExprBinary {
    fn eval(&self) -> Result<f64> {
        let left = self.lhs.eval();
        let right = self.rhs.eval();
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
    fn eval(&self) -> Result<f64> {
        let right = self.rhs.eval();
        if let Ok(r) = right {
            match self.op {
                UnaryOp::Neg(_) => Ok(-r),
            }
        } else {
            right
        }
    }
}
