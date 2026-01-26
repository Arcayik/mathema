use std::vec::IntoIter;

use crate::{
    algebra::{
        ast::{AlgebraTree, NodeId, TreeVisitor},
        eval::{evaluate_tree, EvalError}
    },
    context::{Context, FuncError},
    intrinsics::{call_binary_func, call_unary_func, is_binary_func, is_unary_func},
    symbol::Symbol
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
pub struct FnArgs(Box<[(Symbol, f64)]>);

impl FnArgs {
    pub fn from_params(params: FnParams, args: Vec<f64>) -> Self {
        let inner = params.into_iter().zip(args).collect();
        FnArgs(inner)
    }

    pub fn get_arg(&self, symbol: Symbol) -> Option<f64> {
        let idx = self.0.iter().position(|(s, _)| *s == symbol)?;
        Some(self.0[idx].1)
    }
}

pub struct EvaluateWithArgs(FnArgs);

impl TreeVisitor for EvaluateWithArgs {
    type Output = Result<f64, Vec<EvalError>>;
    fn visit_tree(&self, ctxt: &Context, start_idx: NodeId) -> Self::Output {
        evaluate_tree(ctxt, start_idx, Some(&self.0))
    }
}

fn eval_user_function(ctxt: &Context, function: &Function, input: Vec<f64>) -> Result<f64, FuncError> {
    let args_off = input.len() as isize - function.params.len() as isize;
    if args_off != 0 {
        return Err(FuncError::BadArgs(args_off))
    }
     
    let args = FnArgs::from_params(function.params.clone(), input);
    function.body.visit(ctxt, EvaluateWithArgs(args))
        .map_err(FuncError::Eval)
}

pub fn call_function(ctxt: &Context, name: Symbol, input: Vec<f64>) -> Result<f64, FuncError> {
    // TODO: show wrong args error instead of undefined function error
    if let Some(func) = ctxt.get_function(name) {
        eval_user_function(ctxt, func, input)
    } else if is_unary_func(name.as_str()) && input.len() == 1 {
        call_unary_func(name.as_str(), input[0])
    } else if is_binary_func(name.as_str()) && input.len() == 2 {
        call_binary_func(name.as_str(), input[0], input[1])
    } else {
        Err(FuncError::NotDefined(name))
    }
}
