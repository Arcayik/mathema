use crate::{
    algebra::{self, visit, AlgExpr, AlgebraVisit, EvalError, EvalErrorKind, Evaluator},
    context::{CallError, Context},
    symbol::Symbol
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

        let mut eval = Evaluator::new(context);
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
pub struct FnArgs(Vec<Symbol>);

impl FnArgs {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn from_vec(mut vec: Vec<Symbol>) -> Self {
        vec.dedup();
        Self(vec)
    }

    pub fn push(&mut self, arg: Symbol) -> Result<(), ArgInUse> {
        if self.arg_check(arg) {
            self.0.push(arg);
            Ok(())
        } else {
            Err(ArgInUse())
        }
    }

    pub fn insert(&mut self, idx: usize, arg: Symbol) -> Result<(), ArgInUse> {
        if self.arg_check(arg) {
            self.0.insert(idx, arg);
            Ok(())
        } else {
            Err(ArgInUse())
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn idx_of(&self, arg: Symbol) -> Option<usize> {
        self.iter().position(|&a| a == arg)
    }

    pub fn get_args(&self) -> &[Symbol] {
        self.0.as_slice()
    }

    pub fn take_args(&mut self) -> Vec<Symbol> {
        std::mem::take(&mut self.0)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Symbol> {
        self.0.iter()
    }

    fn arg_check(&self, arg: Symbol) -> bool {
        !self.0.contains(&arg)
    }
}

pub struct ArgInUse();

pub struct FunctionEvaluator<'a> {
    context: &'a Context,
    args: &'a FnArgs,
    inputs: &'a [f64],
    errors: Vec<EvalError>
}

impl<'a> FunctionEvaluator<'a> {
    pub fn new(context: &'a Context, args: &'a FnArgs, inputs: &'a [f64]) -> Self {
        FunctionEvaluator {
            context,
            args,
            inputs,
            errors: Vec::new()
        }
    }

    fn store_error(&mut self, kind: EvalErrorKind)  {
        let err = EvalError::new(kind);
        self.errors.push(err);
    }

    fn eval_variable(&mut self, name: Symbol) -> Option<f64> {
        if let Some(idx) = self.args.iter().position(|&n| n == name) {
            return self.inputs.get(idx).copied()
        };

        let var = self.context.get_variable(name);
        if let Some(expr) = var {
            let mut eval = Evaluator::new(self.context);
            eval.visit_expr(expr)
        } else {
            self.store_error(EvalErrorKind::UndefinedVar(name));
            None
        }
    }
}

impl AlgebraVisit for FunctionEvaluator<'_> {
    type Result = Option<f64>;

    fn visit_expr(&mut self, node: &AlgExpr) -> Self::Result {
        visit::walk_expr(self, node)
    }

    fn visit_unary(&mut self, node: &algebra::Unary) -> Self::Result {
        visit::walk_unary(self, node)
    }

    fn visit_binary(&mut self, node: &algebra::Binary) -> Self::Result {
        let left = self.visit_expr(&node.left)?;
        let right = self.visit_expr(&node.right)?;

        let value = match node.op {
            algebra::AlgBinOp::Add => left + right,
            algebra::AlgBinOp::Sub => left - right,
            algebra::AlgBinOp::Mul => left * right,
            algebra::AlgBinOp::Div => left / right,
            algebra::AlgBinOp::Exp => left.powf(right),
        };

        Some(value)
    }

    fn visit_fn_call(&mut self, node: &algebra::FnCall) -> Self::Result {
        let args: Vec<f64> = node.args.iter()
            .map(|node| self.visit_expr(node))
            .collect::<Option<_>>()?;

        self.context.call_func(node.func, &args)
            .map_err(|e| self.store_error(EvalErrorKind::BadFnCall(e)))
            .ok()
    }

    fn visit_value(&mut self, node: &algebra::Value) -> Self::Result {
        match node {
            algebra::Value::Num(n) => Some(*n),
            algebra::Value::Var(v) => self.eval_variable(*v)
        }
    }
}
