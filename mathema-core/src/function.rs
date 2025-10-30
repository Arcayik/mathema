use crate::{
    algebra::{AlgExpr, AlgebraVisit, AlgebraFold, Evaluator, Value},
    context::{CallError, Context},
    Name
};

#[derive(Debug)]
pub struct Function {
    pub(crate) params: Vec<Name>,
    pub(crate) body: Box<AlgExpr>,
}

impl Function {
    pub fn new(algebra: AlgExpr, params: Vec<Name>) -> Self {
        let body = AlgebraToFunctionBody::new(&params).fold_expr(algebra);
        let body = Box::new(body);
        Function { params, body }
    }

    pub fn evaluate(&self, context: &Context, args: &[f64]) -> Result<f64, CallError> {
        let args_off = args.len() as isize - self.params.len() as isize;
        if args_off != 0 {
            return Err(CallError::BadArgs(args_off))
        }

        let mut eval = Evaluator::new(context, args);
        if let Some(value) = eval.visit_expr(&self.body) {
            Ok(value)
        } else {
            Err(CallError::Eval(eval.take_errors()))
        }
    }

    pub fn num_params(&self) -> usize {
        self.params.len()
    }
}

pub(crate) struct AlgebraToFunctionBody<'a> {
    params: &'a [Name]
}

// TODO: FnArgs newtype to uphold invariants (same name, etc.)

impl<'a> AlgebraToFunctionBody<'a> {
    pub fn new(params: &'a [Name]) -> Self {
        AlgebraToFunctionBody { params }
    }
}

impl<'a> AlgebraFold for AlgebraToFunctionBody<'a> {
    fn fold_value(&mut self, value: Value) -> Value {
        match value {
            Value::Var(ref name) => {
                // check if name matches param
                if let Some(idx) = self.params.iter().position(|n| *n == *name) {
                    Value::Param(idx)
                } else {
                    value
                }
            }
            Value::Num(_) => value,
            Value::Param(_) => unreachable!("AlgExprs outside of Functions cannot have params"),
        }
    }

}
