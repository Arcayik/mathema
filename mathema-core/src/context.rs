use std::collections::HashMap;

use crate::{
    algebra::{Algebra, EvalError},
    intrinsics::{self, ConstFunc}
};

#[derive(Default)]
pub struct Context {
    variables: HashMap<String, f64>,
    functions: HashMap<String, Algebra>,
}

impl Context {
    pub fn set_variable(&mut self, name: String, value: f64) {
        self.variables.insert(name, value);
    }

    pub fn get_variable(&self, name: &str) -> Option<f64> {
        self.variables.get(name).copied()
    }

    pub fn has_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    pub fn set_function(&mut self, name: String, algebra: Algebra) {
        self.functions.insert(name, algebra);
    }

    fn get_builtin_fn(&self, name: &str) -> Option<&ConstFunc> {
        intrinsics::CONST_FNS.get(name)
    }

    pub fn get_function(&self, name: &str) -> Option<Function<'_>> {
        if let Some(func) = self.get_builtin_fn(name) {
            return Some(Function::Builtin(func));
        }

        self.functions.get(name).map(Function::UserDefined)
    }

    pub fn call_function(&self, name: &str, args: &[f64]) -> FuncResult {
        let func = match self.functions.get(name) {
            Some(f) => f,
            None => return FuncResult::NotFound
        };

        func.evaluate(self, args)
            .map(FuncResult::Return)
            .unwrap_or_else(FuncResult::Error)
    }

    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}

pub enum Function<'a> {
    UserDefined(&'a Algebra),
    Builtin(&'a ConstFunc)
}

impl Function<'_> {
    pub fn num_params(&self) -> usize {
        match self {
            Self::UserDefined(alg) => alg.num_params(),
            Self::Builtin(func) => func.num_params()
        }
    }
}

pub enum FuncResult {
    Return(f64),
    Error(EvalError),
    NotFound,
}
