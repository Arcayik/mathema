use std::{
    collections::HashMap,
    f64::consts::{E, PI},
    rc::Rc
};

use crate::{
    function::Function,
    diagnostic::Diagnostic,
};

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

    pub fn set_variable(&mut self, name: Box<str>, value: f64) {
        self.variables.insert(name, value);
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<Function>> {
        self.functions.get(name).map(Rc::clone)
    }

    pub fn set_function(&mut self, name: Box<str>, func: Function) {
        self.functions.insert(name, func.into());
    }
}

pub enum Outcome {
    ShowDiagnostics(Vec<Diagnostic>),
    ShowAnswer(f64),
    AssignVar(Box<str>, f64),
    DefineFn(Box<str>)
}
