use std::rc::Rc;

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
