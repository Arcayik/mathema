use crate::{context::Context, parse::{expr::*, token::Ident, Function}};
use std::rc::Rc;

#[derive(Debug)]
pub enum ExprError {
    UndefinedVar(Ident),
    UndefinedFunc(Ident),
}

impl Expr {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
        match self {
            Self::Value(value) => value.eval(ctxt).map_err(|e| vec![e]),
            Self::Binary(expr) => expr.eval(ctxt),
            Self::Unary(expr) => expr.eval(ctxt),
            Self::Group(group) => group.eval(ctxt),
            Self::FnCall(call) => call.eval(ctxt),
        }
    }
}

impl ExprValue {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, ExprError> {
        match self {
            Self::Ident(ident) => try_get_variable(ctxt, ident),
            Self::Literal(literal) => Ok(literal.num),
        }
    }
}

fn try_get_variable(ctxt: &Context, ident: &Ident) -> Result<f64, ExprError> {
    match ctxt.get_variable(ident.repr.as_ref()) {
        Some(num) => Ok(num),
        None => Err(ExprError::UndefinedVar(ident.clone()))
    }
}

impl ExprBinary {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
        let left = self.lhs.eval(ctxt);
        let right = self.rhs.eval(ctxt);

        match (left, right) {
            (Ok(l), Ok(r)) => match self.op {
                BinOp::Add(_) => Ok(l + r),
                BinOp::Sub(_) => Ok(l - r),
                BinOp::Mul(_) => Ok(l * r),
                BinOp::Div(_) => Ok(l / r),
            },

            (Err(e), Ok(_)) | (Ok(_), Err(e)) => Err(e),

            (Err(mut le), Err(mut re)) => {
                le.append(&mut re);
                Err(le)
            }
        }
    }
}

impl ExprUnary {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
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

impl ExprGroup {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
        self.expr.eval(ctxt)
    }
}

impl ExprFnCall {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
        let name = &self.name.repr;
        let func = ctxt.get_function(name)
            .ok_or(vec![ExprError::UndefinedFunc(self.name.clone())])?;

        let mut arg_exprs = Vec::new();
        for expr in self.inputs.iter() {
            arg_exprs.push(expr.eval(ctxt)?);
        };

        let call = func.call(&arg_exprs);

        Ok(call)
    }
}

fn try_get_function(ctxt: &Context, ident: &Ident) -> Result<Rc<Function>, ExprError> {
    let name = &ident.repr;
    let func = ctxt.get_function(name);
    func.ok_or(ExprError::UndefinedVar(ident.clone()))
}
