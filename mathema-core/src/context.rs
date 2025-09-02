use std::{collections::HashMap, f64::consts::{E, PI}, rc::Rc};

use crate::Function;

pub struct Context {
    variables: HashMap<Box<str>, f64>,
    functions: HashMap<Box<str>, Rc<Function>>,
}

impl Default for Context {
    fn default() -> Self {
        let mut variables = HashMap::new();
        variables.insert("pi".into(), PI);
        variables.insert("e".into(), E);

        let functions = HashMap::new();
        Context { variables, functions }
    }
}

impl Context {
    pub fn get_variable(&self, name: &str) -> Option<f64> {
        self.variables.get(name).copied()
    }

    pub fn set_variable(&mut self, name: &str, value: f64) {
        self.variables.insert(name.into(), value);
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<Function>> {
        self.functions.get(name).map(Rc::clone)
    }

    pub fn set_function(&mut self, name: &str, func: Function) {
        self.functions.insert(name.into(), func.into());
    }
}
