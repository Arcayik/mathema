use std::rc::Rc;
use std::collections::HashMap;

use crate::{
    function::Function, intrinsics::{declare_constants, declare_functions},
};

pub trait Context {
    fn define_defaults(&mut self) where Self: Sized {
        declare_constants(self as &mut dyn Context);
        declare_functions(self as &mut dyn Context);
    }

    fn set_variable(&mut self, name: Box<str>, value: f64);
    fn get_variable(&self, name: &str) -> Option<f64>;
    fn set_function(&mut self, name: Box<str>, func: Function);
    fn get_function(&self, name: &str) -> Option<Rc<Function>>;
}

pub struct ScopeContext {
    variables: HashMap<Box<str>, f64>,
    functions: HashMap<Box<str>, Rc<Function>>,
}

impl Default for ScopeContext {
    fn default() -> Self {
        let mut ctxt = ScopeContext {
            variables: HashMap::new(),
            functions: HashMap::new()
        };
        ctxt.define_defaults();
        ctxt
    }
}

impl Context for ScopeContext {
    fn get_variable(&self, name: &str) -> Option<f64> {
        self.variables.get(name).copied()
    }

    fn set_variable(&mut self, name: Box<str>, value: f64) {
        self.variables.insert(name, value);
    }

    fn get_function(&self, name: &str) -> Option<Rc<Function>> {
        self.functions.get(name).map(Rc::clone)
    }

    fn set_function(&mut self, name: Box<str>, func: Function) {
        self.functions.insert(name, func.into());
    }
}
