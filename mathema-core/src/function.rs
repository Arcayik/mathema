use crate::{
    algebra::{AlgExpr, AlgebraVisit, AlgebraFold, Evaluator, Value},
    context::{CallError, Context},
    Name
};

#[derive(Debug)]
pub struct Function {
    pub(crate) args: FnArgs,
    pub(crate) body: Box<AlgExpr>,
}

impl Function {
    pub fn new(algebra: AlgExpr, args: FnArgs) -> Self {
        let body = Box::new(algebra);
        Function { args, body }
    }

    pub fn evaluate(&self, context: &Context, args: &[f64]) -> Result<f64, CallError> {
        let args_off = args.len() as isize - self.args.len() as isize;
        if args_off != 0 {
            return Err(CallError::BadArgs(args_off))
        }

        let mut eval = Evaluator::new(context).with_args(args);
        if let Some(value) = eval.visit_expr(&self.body) {
            Ok(value)
        } else {
            Err(CallError::Eval(eval.take_errors()))
        }
    }

    pub fn num_params(&self) -> usize {
        self.args.len()
    }
}

#[derive(Clone, Debug)]
pub struct FnArgs(Vec<Name>);

impl FnArgs {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn from_vec(mut vec: Vec<Name>) -> Self {
        vec.dedup();
        Self(vec)
    }

    pub fn push(&mut self, arg: &Name) -> Result<(), ()> {
        if self.arg_check(arg) {
            self.0.push(arg.clone());
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn insert(&mut self, idx: usize, arg: &Name) -> Result<(), ()> {
        if self.arg_check(arg) {
            self.0.insert(idx, arg.clone());
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get_args(&self) -> &[Name] {
        self.0.as_slice()
    }

    pub fn take_args(&mut self) -> Vec<Name> {
        std::mem::take(&mut self.0)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Name> {
        self.0.iter()
    }

    fn arg_check(&self, arg: &Name) -> bool {
        !self.0.contains(arg)
    }
}

pub(crate) struct AlgebraToFunctionBody<'a> {
    params: &'a [Name]
}

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
