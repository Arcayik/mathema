use std::collections::HashMap;

use crate::{
    algebra::{Algebra, EvalError},
    intrinsics
};

pub trait Context {
    fn set_variable(&mut self, name: String, value: f64);
    fn get_variable(&self, name: &str) -> Option<f64>;
    fn has_variable(&self, name: &str) -> bool;

    fn set_function(&mut self, name: String, algebra: Algebra);
    fn get_function(&self, name: &str) -> Option<&Algebra>;
    fn call_function(&self, name: &str, args: &[f64]) -> FuncResult;
    fn has_function(&self, name: &str) -> bool;
}

pub struct ScopeContext {
    variables: HashMap<String, f64>,
    functions: HashMap<String, Algebra>,
}

impl Default for ScopeContext {
    fn default() -> Self {
        ScopeContext {
            variables: HashMap::new(),
            functions: HashMap::new()
        }
    }
}

pub enum FuncResult {
    Return(f64),
    Error(EvalError),
    NotFound,
}

impl Context for ScopeContext {
    fn set_variable(&mut self, name: String, value: f64) {
        self.variables.insert(name, value);
    }

    fn get_variable(&self, name: &str) -> Option<f64> {
        self.variables.get(name).copied()
    }

    fn has_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    fn set_function(&mut self, name: String, algebra: Algebra) {
        self.functions.insert(name, algebra);
    }

    fn get_function(&self, name: &str) -> Option<&Algebra> {
        self.functions.get(name)
    }

    fn call_function(&self, name: &str, args: &[f64]) -> FuncResult {
        match intrinsics::CONST_FNS.get(name) {
            Some(func) => return func.call(args)
                .map(FuncResult::Return)
                .unwrap_or_else(FuncResult::Error),
            None => {}
        };

        if let Some(func) = self.functions.get(name) {
            func.evaluate(self, args)
                .map(FuncResult::Return)
                .unwrap_or_else(FuncResult::Error)
        } else {
            FuncResult::NotFound
        }
    }

    fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}
