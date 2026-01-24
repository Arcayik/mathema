use std::{collections::HashMap, sync::LazyLock};

use crate::context::FuncError;
use functions::*;

pub static CONSTANTS: LazyLock<HashMap<&'static str, f64>> = LazyLock::new(declare_constants);
pub static UNARY_FUNCS: LazyLock<HashMap<&'static str, UnaryFunc>> = LazyLock::new(declare_unary);
pub static BINARY_FUNCS: LazyLock<HashMap<&'static str, BinaryFunc>> = LazyLock::new(declare_binary);

macro_rules! define_constants {
    (
        $( $name:ident = $def:expr ;)*
    ) => {
        fn declare_constants() -> HashMap<&'static str, f64> {
            let mut hashmap = HashMap::new();
            $(
                let _ = hashmap.insert(stringify!($name), $def)
            );*;
            hashmap
        }
    }
}

define_constants! {
    pi = std::f64::consts::PI;
    tau = std::f64::consts::TAU;
    e = std::f64::consts::E;
}

pub struct UnaryFunc(fn(f64) -> f64);

impl UnaryFunc {
    pub fn call(&self, x: f64) -> f64 {
        (self.0)(x)
    }
}

pub struct BinaryFunc(fn(f64, f64) -> f64);

impl BinaryFunc {
    pub fn call(&self, x: f64, y: f64) -> f64 {
        (self.0)(x, y)
    }
}

fn declare_unary() -> HashMap<&'static str, UnaryFunc> {
    let mut hashmap = HashMap::new();
    hashmap.insert("ln", UnaryFunc(ln));

    hashmap.insert("sin", UnaryFunc(sin));
    hashmap.insert("cos", UnaryFunc(cos));
    hashmap.insert("tan", UnaryFunc(tan));
    hashmap
}

fn declare_binary() -> HashMap<&'static str, BinaryFunc> {
    let mut hashmap = HashMap::new();

    hashmap.insert("log", BinaryFunc(log));
    hashmap
}

pub fn is_constant(name: &str) -> bool {
    CONSTANTS.contains_key(name)
}

pub fn is_unary_func(name: &str) -> bool {
    UNARY_FUNCS.contains_key(name)
}

pub fn is_binary_func(name: &str) -> bool {
    BINARY_FUNCS.contains_key(name)
}

pub fn call_unary_func(name: &str, x: f64) -> Result<f64, FuncError> {
    let func = UNARY_FUNCS.get(name).unwrap();
    Ok(func.call(x))
}

pub fn call_binary_func(name: &str, x: f64, y: f64) -> Result<f64, FuncError> {
    let func = BINARY_FUNCS.get(name).unwrap();
    Ok(func.call(x, y))
}

mod functions {
    pub fn sin(x: f64) -> f64 {
        x.sin()
    }

    pub fn cos(x: f64) -> f64 {
        x.cos()
    }

    pub fn tan(x: f64) -> f64 {
        x.tan()
    }

    pub fn log(x: f64, b: f64) -> f64 {
        x.log10() / b.log10()
    }

    pub fn ln(x: f64) -> f64 {
        x.ln()
    }
}
