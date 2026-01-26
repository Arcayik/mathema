use crate::{
    algebra::ast::{AlgBinOp, AlgExpr, AlgUnaryOp, NodeId, TreeVisitor},
    context::{call_variable, Context, FuncError, VarError},
    function::{call_function, FnArgs}
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

pub struct Evaluate;

impl TreeVisitor for Evaluate {
    type Output = Result<f64, Vec<EvalError>>;
    fn visit_tree(&self, ctxt: &Context, start_idx: NodeId) -> Self::Output {
        evaluate_tree(ctxt, start_idx, None)
    }
}

pub(crate) fn evaluate_tree(ctxt: &Context, start_idx: NodeId, args: Option<&FnArgs>) -> Result<f64, Vec<EvalError>> {
    fn recurse(
        ctxt: &Context,
        idx: NodeId,
        args: Option<&FnArgs>,
        errors: &mut Vec<EvalError>
    ) -> Option<f64> {
        let alg = &ctxt.arena[idx];
        match alg {
            AlgExpr::Literal(val) => Some(*val),
            AlgExpr::Ident(id) => {
                if let Some(args) = args {
                    let result = args.get_arg(*id);
                    if result.is_some() {
                        return result;
                    }
                }
                match call_variable(ctxt, *id) {
                    Ok(ans) => Some(ans),
                    Err(e) => {
                        let kind = EvalErrorKind::BadVar(e);
                        errors.push(EvalError { kind });
                        None
                    }
                }
            }
            AlgExpr::Unary { op, inner } => {
                if let Some(val) = recurse(ctxt, *inner, args, errors) {
                    match op {
                        AlgUnaryOp::Neg => Some(- val)
                    }
                } else {
                    None
                }
            },
            AlgExpr::Binary { left, op, right } => {
                let left = recurse(ctxt, *left, args, errors);
                let right = recurse(ctxt, *right, args, errors);

                if let (Some(l), Some(r)) = (left, right) {
                    match op {
                        AlgBinOp::Add => Some(l + r),
                        AlgBinOp::Sub => Some(l - r),
                        AlgBinOp::Mul => Some(l * r),
                        AlgBinOp::Div => Some(l / r),
                        AlgBinOp::Exp => Some(l.powf(r)),
                    }
                } else {
                    None
                }
            },
            AlgExpr::FnCall { name, args: these_args } => {
                let these_args: Vec<f64> = these_args
                    .iter()
                    .map(|idx| recurse(ctxt, *idx, args, errors))
                    .collect::<Option<Vec<_>>>()?;

                match call_function(ctxt, *name, these_args) {
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
    let result = recurse(ctxt, start_idx, args, &mut errors);
    if let Some(ans) = result {
        Ok(ans)
    } else {
        Err(errors)
    }
}

