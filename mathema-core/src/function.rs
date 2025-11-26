use crate::{
    algebra::{self, eval_algebra, AlgExpr, EvalError, EvalErrorKind}, context::{CallError, Context}, intrinsics::{self, call_binary_func, call_unary_func, is_binary_func, is_unary_func}, snippet::{create_algebra_snippet, create_function_snippet}, symbol::Symbol, value::MathemaValue
};

#[derive(Debug)]
pub struct Function {
    pub(crate) name: Symbol,
    pub(crate) args: FnArgs,
    pub(crate) body: Box<AlgExpr>,
}

impl Function {
    pub fn new(name: Symbol, algebra: AlgExpr, args: FnArgs) -> Self {
        let body = Box::new(algebra);
        Function { name, args, body }
    }

    pub fn num_params(&self) -> usize {
        self.args.len()
    }
}

#[derive(Clone, Debug)]
pub struct FnArgs(Vec<Symbol>);

impl std::fmt::Display for FnArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = self.0.join(", ");
        write!(f, "{}", str)
    }
}

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

#[derive(Copy, Clone, Debug)]
pub struct ArgInUse();

impl std::fmt::Display for ArgInUse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "argument already present in FnArgs")
    }
}

impl std::error::Error for ArgInUse {}

fn eval_user_function(context: &Context, function: &Function, input: &[MathemaValue]) -> Result<MathemaValue, CallError> {
    fn recurse(
        context: &Context,
        function: &Function,
        input: &[MathemaValue],
        algebra: &AlgExpr,
        errors: &mut Vec<EvalError>,
    ) -> Option<MathemaValue> {
        match algebra {
            AlgExpr::Value(val) => match val {
                algebra::Value::Num(num) => Some(num.clone()),
                algebra::Value::Var(var) => {
                    if let Some(idx) = function.args.idx_of(*var) {
                        Some(input[idx].clone())
                    } else if let Some(expr) = context.get_variable(*var) {
                        eval_algebra(context, expr)
                            .map_err(|mut e| errors.append(&mut e))
                            .ok()
                    } else if let Some(c) = intrinsics::CONSTANTS.get(var.as_str()) {
                        Some(c.clone())
                    } else {
                        let snip = create_algebra_snippet(&function.body);
                        let source = snip.source();
                        let span = snip.get_span(algebra).unwrap();
                        let error = EvalError {
                            kind: EvalErrorKind::UndefinedVar(*var),
                            source: source.into(),
                            span,
                        };
                        errors.push(error);
                        None
                    }
                }
            },
            AlgExpr::Unary(un) => {
                if let Some(val) = recurse(context, function, input, &un.expr, errors) {
                    match un.op {
                        algebra::AlgUnaryOp::Neg => Some(val.neg()),
                    }
                } else {
                    None
                }
            },
            AlgExpr::Binary(bin) => {
                let left = recurse(context, function, input, &bin.left, errors);
                let right = recurse(context, function, input, &bin.right, errors);

                if let (Some(l), Some(r)) = (left, right) {
                    match bin.op {
                        algebra::AlgBinOp::Add => Some(l.add(&r)),
                        algebra::AlgBinOp::Sub => Some(l.sub(&r)),
                        algebra::AlgBinOp::Mul => Some(l.mul(&r)),
                        algebra::AlgBinOp::Div => Some(l.div(&r)),
                        algebra::AlgBinOp::Exp => Some(l.pow(&r)),
                    }
                } else {
                    None
                }
            },
            AlgExpr::FnCall(fc) => {
                let args: Vec<MathemaValue> = fc.args
                    .iter()
                    .map(|a| recurse(context, function, input, a, errors))
                    .collect::<Option<_>>()?;

                match call_function(context, fc.name, &args) {
                    Ok(ans) => Some(ans),
                    Err(e) => {
                        let snip = create_algebra_snippet(&function.body);
                        let source = snip.source();
                        let span = snip.get_span(algebra).unwrap();
                        let error = EvalError {
                            kind: EvalErrorKind::BadFnCall(e),
                            source: source.into(),
                            span,
                        };
                        errors.push(error);
                        None
                    }
                }
            }
        }
    }

    let name = function.name;

    let args_off = input.len() as isize - function.args.len() as isize;
    if args_off != 0 {
        return Err(CallError::BadArgs(name, args_off))
    }

    let origin = create_function_snippet(&function).source().into();
    dbg!(&origin);
     
    let mut errors = Vec::new();
    let result = recurse(context, function, input, &function.body, &mut errors);
    result.ok_or(CallError::Eval { name, origin, errors })
}

pub fn call_function(context: &Context, name: Symbol, input: &[MathemaValue]) -> Result<MathemaValue, CallError> {
    if let Some(func) = context.get_function(name) {
        eval_user_function(context, func, input)
    } else if is_unary_func(name.as_str()) && input.len() == 1 {
        Ok(call_unary_func(name.as_str(), input[0].clone()))
    } else if is_binary_func(name.as_str()) && input.len() == 2 {
        Ok(call_binary_func(name.as_str(), input[0].clone(), input[1].clone()))
    } else {
        Err(CallError::NotDefined(name))
    }
}
