use std::vec::IntoIter;

use crate::{
    algebra::{
        ast::{AlgExpr, AlgebraTree, NodeId, TreeVisitor},
        eval::{evaluate_tree, EvalError, Evaluate}
    },
    context::{Context, FuncError, ValueSource},
    intrinsics::{call_binary_func, call_unary_func, is_binary_func, is_unary_func},
    symbol::Symbol,
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

#[derive(Clone)]
pub struct FunctionContext<'c>(&'c Context, &'c FnArgs);

impl ValueSource for FunctionContext<'_> {
    fn get_variable(&self, name: Symbol) -> Option<&AlgebraTree> {
        let arg_result = self.1.take_arg(name);
        if arg_result.is_some() {
            arg_result
        } else {
            self.0.get_variable(name)
        }
    }

    fn get_function(&self, name: Symbol) -> Option<&Function> {
        self.0.get_function(name)
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

#[derive(Debug)]
pub struct FnArgs(Vec<(Symbol, AlgebraTree)>);

impl FnArgs {
    pub fn from_params(params: FnParams, args: Vec<AlgebraTree>) -> Self {
        let inner = params.into_iter().zip(args).collect();
        FnArgs(inner)
    }

    pub fn take_arg(&self, symbol: Symbol) -> Option<&AlgebraTree> {
        let idx = self.0.iter().position(|(s, _)| *s == symbol)?;
        Some(&self.0[idx].1)
    }
}

pub struct EvaluateWithArgs<'c>(&'c dyn ValueSource);

impl TreeVisitor for EvaluateWithArgs<'_> {
    type Output = Result<f64, Vec<EvalError>>;
    fn visit_tree(&self, nodes: &[AlgExpr], start_idx: NodeId) -> Self::Output {
        let context = self.0;
        evaluate_tree(context, nodes, start_idx)
    }
}

fn eval_user_function(context: &dyn ValueSource, function: &Function, input: Vec<AlgebraTree>) -> Result<f64, FuncError> {
    let args_off = input.len() as isize - function.params.len() as isize;
    if args_off != 0 {
        return Err(FuncError::BadArgs(args_off))
    }
     
    function.body.accept(EvaluateWithArgs(context))
        .map_err(FuncError::Eval)
}

pub fn call_function(context: &dyn ValueSource, name: Symbol, input: Vec<AlgebraTree>) -> Result<f64, FuncError> {
    if let Some(func) = context.get_function(name) {
        eval_user_function(context, func, input)
    } else if is_unary_func(name.as_str()) && input.len() == 1 {
        let first = input[0].accept(Evaluate(context)).map_err(FuncError::Eval)?;
        call_unary_func(name.as_str(), first)
    } else if is_binary_func(name.as_str()) && input.len() == 2 {
        let first = input[0].accept(Evaluate(context)).map_err(FuncError::Eval)?;
        let second = input[1].accept(Evaluate(context)).map_err(FuncError::Eval)?;
        call_binary_func(name.as_str(), first, second)
    } else {
        Err(FuncError::NotDefined(name))
    }
}
