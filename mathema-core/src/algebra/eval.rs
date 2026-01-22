use crate::{
    algebra::ast::AlgebraTree, context::{call_variable, Context, FuncError, VarError}, function::call_function, value::MathemaValue
};

#[derive(Debug)]
pub enum EvalErrorKind {
    BadFnCall(FuncError),
    BadVar(VarError),
}

#[derive(Debug)]
pub struct EvalError {
    pub kind: EvalErrorKind,
}

pub fn eval_algebra(context: &Context, algebra: &AlgebraTree) -> Result<MathemaValue, Vec<EvalError>> {
    todo!()
/* TODO
    fn recurse(
        context: &Context,
        algebra: &AlgExpr,
        errors: &mut Vec<EvalError>
    ) -> Option<MathemaValue> {
        match algebra {
            AlgExpr::Value(val) => match val {
                Value::Num(num) => Some(num.clone()),
                Value::Var(var) => {
                    match call_variable(context, *var) {
                        Ok(ans) => Some(ans),
                        Err(e) => {
                            let kind = EvalErrorKind::BadVar(e);
                            errors.push(EvalError { kind });
                            None
                        }
                    }
                }
            },
            AlgExpr::Unary(un) => {
                if let Some(val) = recurse(context, &un.expr, errors) {
                    match un.op {
                        AlgUnaryOp::Neg => Some(val.neg())
                    }
                } else {
                    None
                }
            },
            AlgExpr::Binary(bin) => {
                let left = recurse(context, &bin.left, errors);
                let right = recurse(context, &bin.right, errors);

                if let (Some(l), Some(r)) = (left, right) {
                    match bin.op {
                        AlgBinOp::Add => Some(l.add(&r)),
                        AlgBinOp::Sub => Some(l.sub(&r)),
                        AlgBinOp::Mul => Some(l.mul(&r)),
                        AlgBinOp::Div => Some(l.div(&r)),
                        AlgBinOp::Exp => Some(l.pow(&r)),
                    }
                } else {
                    None
                }
            },
            AlgExpr::FnCall(fc) => {
                let args: Vec<MathemaValue> = fc.args
                    .iter()
                    .map(|a| recurse(context, a, errors))
                    .collect::<Option<_>>()?;

                match call_function(context, fc.name, &args) {
                    Ok(ans) => Some(ans),
                    Err(e) => {
                        let kind = EvalErrorKind::BadFnCall(e);
                        let error = EvalError { kind };
                        errors.push(error);
                        None
                    }
                }
            }
        }
    }

    let mut errors = Vec::new();
    let result = recurse(context, algebra, &mut errors);
    if let Some(ans) = result {
        Ok(ans)
    } else {
        Err(errors)
    }
*/
}

