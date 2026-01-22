use crate::{
    algebra::ast::{AlgBinOp, AlgExpr, AlgUnaryOp, NodeIdx, TreeVisitor, Value},
    context::{call_variable, Context, FuncError, VarError},
    function::call_function,
    value::MathemaValue,
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

pub struct Evaluate<'c>(pub &'c Context);

impl TreeVisitor for Evaluate<'_> {
    type Output = Result<MathemaValue, Vec<EvalError>>;
    fn visit_tree(&self, nodes: &[AlgExpr], start_idx: NodeIdx) -> Self::Output {
        evaluate_tree(self.0, nodes, start_idx)
    }
}

fn evaluate_tree(context: &Context, nodes: &[AlgExpr], start_idx: NodeIdx) -> Result<MathemaValue, Vec<EvalError>> {
    fn recurse(
        context: &Context,
        nodes: &[AlgExpr],
        idx: NodeIdx,
        errors: &mut Vec<EvalError>
    ) -> Option<MathemaValue> {
        let alg = &nodes[idx];
        match alg {
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
            AlgExpr::Unary { op, inner } => {
                if let Some(val) = recurse(context, nodes, *inner, errors) {
                    match op {
                        AlgUnaryOp::Neg => Some(val.neg())
                    }
                } else {
                    None
                }
            },
            AlgExpr::Binary { left, op, right } => {
                let left = recurse(context, nodes, *left, errors);
                let right = recurse(context, nodes, *right, errors);

                if let (Some(l), Some(r)) = (left, right) {
                    match op {
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
            AlgExpr::FnCall { name, args } => {
                let args: Vec<MathemaValue> = args
                    .iter()
                    .map(|arg| recurse(context, nodes, *arg, errors))
                    .collect::<Option<_>>()?;

                match call_function(context, *name, &args) {
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
    let result = recurse(context, nodes, start_idx, &mut errors);
    if let Some(ans) = result {
        Ok(ans)
    } else {
        Err(errors)
    }
}

