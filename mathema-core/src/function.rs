use std::vec::IntoIter;

use crate::{
    algebra::{
        ast::{AlgBinOp, AlgExpr, AlgUnaryOp, AlgebraTree, NodeIdx, TreeVisitor, Value},
        eval::{EvalError, EvalErrorKind}
    }, context::{call_variable, Context, FuncError}, intrinsics::{call_binary_func, call_unary_func, is_binary_func, is_unary_func}, symbol::Symbol, value::MathemaValue
};

#[derive(Debug)]
pub struct Function {
    pub(crate) params: FnParams,
    pub(crate) body: AlgebraTree,
}

impl Function {
    pub fn new(body: AlgebraTree, args: FnParams) -> Self {
        Function { params: args, body }
    }

    pub fn num_params(&self) -> usize {
        self.params.len()
    }
}

#[derive(Clone, Debug)]
pub struct FnParams(Vec<Symbol>);

impl std::fmt::Display for FnParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = self.0.join(", ");
        write!(f, "{}", str)
    }
}

impl IntoIterator for FnParams {
    type Item = Symbol;
    type IntoIter = IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FnParams {
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

#[derive(Debug, Clone)]
pub struct FnArgs(Vec<(Symbol, MathemaValue)>);

impl FnArgs {
    pub fn from_params(params: FnParams, args: Vec<MathemaValue>) -> Self {
        let inner = params.into_iter().zip(args).collect();
        FnArgs(inner)
    }

    pub fn get_arg(&self, symbol: Symbol) -> Option<MathemaValue> {
        self.0.iter()
            .find(|(s, _)| *s == symbol)
            .map(|(_, v)| v)
            .cloned()
    }
}

pub struct EvaluateWithArgs<'c>(pub &'c Context, FnArgs);

impl TreeVisitor for EvaluateWithArgs<'_> {
    type Output = Result<MathemaValue, Vec<EvalError>>;
    fn visit_tree(&self, nodes: &[AlgExpr], start_idx: NodeIdx) -> Self::Output {
        let Self(context, args) = self;
        evaluate_tree_with_args(context, args, nodes, start_idx)
    }
}

fn evaluate_tree_with_args(context: &Context, args: &FnArgs, nodes: &[AlgExpr], start_idx: NodeIdx) -> Result<MathemaValue, Vec<EvalError>> {
    fn recurse(
        context: &Context,
        args: &FnArgs,
        nodes: &[AlgExpr],
        idx: NodeIdx,
        errors: &mut Vec<EvalError>
    ) -> Option<MathemaValue> {
        let alg = &nodes[idx];
        match alg {
            AlgExpr::Value(val) => match val {
                Value::Num(num) => Some(num.clone()),
                Value::Var(var) => {
                    let arg_result = args.get_arg(*var);
                    if arg_result.is_some() {
                        arg_result
                    } else {
                        match call_variable(context, *var) {
                            Ok(ans) => Some(ans),
                            Err(e) => {
                                let kind = EvalErrorKind::BadVar(e);
                                errors.push(EvalError { kind });
                                None
                            }
                        }
                    }
                }
            },
            AlgExpr::Unary { op, inner } => {
                if let Some(val) = recurse(context, args, nodes, *inner, errors) {
                    match op {
                        AlgUnaryOp::Neg => Some(val.neg())
                    }
                } else {
                    None
                }
            },
            AlgExpr::Binary { left, op, right } => {
                let left = recurse(context, args, nodes, *left, errors);
                let right = recurse(context, args, nodes, *right, errors);

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
            AlgExpr::FnCall { name, args: these_args } => {
                let these_args: Vec<MathemaValue> = these_args
                    .iter()
                    .map(|arg| recurse(context, args, nodes, *arg, errors))
                    .collect::<Option<_>>()?;

                match call_function(context, *name, &these_args) {
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
    let result = recurse(context, args, nodes, start_idx, &mut errors);
    if let Some(ans) = result {
        Ok(ans)
    } else {
        Err(errors)
    }
}

fn eval_user_function(context: &Context, function: &Function, input: &[MathemaValue]) -> Result<MathemaValue, FuncError> {
    let args_off = input.len() as isize - function.params.len() as isize;
    if args_off != 0 {
        return Err(FuncError::BadArgs(args_off))
    }
     
    let args = FnArgs::from_params(function.params.clone(), input.to_vec());
    function.body.accept(EvaluateWithArgs(context, args))
        .map_err(FuncError::Eval)
}

pub fn call_function(context: &Context, name: Symbol, input: &[MathemaValue]) -> Result<MathemaValue, FuncError> {
    if let Some(func) = context.get_function(name) {
        eval_user_function(context, func, input)
    } else if is_unary_func(name.as_str()) && input.len() == 1 {
        call_unary_func(name.as_str(), input[0].clone())
    } else if is_binary_func(name.as_str()) && input.len() == 2 {
        call_binary_func(name.as_str(), input[0].clone(), input[1].clone())
    } else {
        Err(FuncError::NotDefined(name))
    }
}
