use std::{collections::HashMap, sync::LazyLock};
use rug::float::Constant;

use crate::value::MathemaValue;
use functions::*;

pub static CONSTANTS: LazyLock<HashMap<&'static str, MathemaValue>> = LazyLock::new(declare_constants);
pub static UNARY_FUNCS: LazyLock<HashMap<&'static str, UnaryFunc>> = LazyLock::new(declare_unary);
pub static BINARY_FUNCS: LazyLock<HashMap<&'static str, BinaryFunc>> = LazyLock::new(declare_binary);

macro_rules! define_constants {
    (
        $( $name:ident = $def:expr ;)*
    ) => {
        fn declare_constants() -> HashMap<&'static str, MathemaValue> {
            let mut hashmap = HashMap::new();
            $(
                let value = $crate::value::MathemaValue { inner: $def };
                let _ = hashmap.insert(stringify!($name), value)
            );*;
            hashmap
        }
    }
}

define_constants! {
    pi = rug::Float::with_val(512, Constant::Pi);
    tau = rug::Float::with_val(512, Constant::Pi) * 2;
    e = rug::Float::with_val(512, Constant::Euler);
}

pub struct UnaryFunc(fn(MathemaValue) -> MathemaValue);

impl UnaryFunc {
    pub fn call(&self, x: MathemaValue) -> MathemaValue {
        (self.0)(x)
    }
}

pub struct BinaryFunc(fn(MathemaValue, MathemaValue) -> MathemaValue);

impl BinaryFunc {
    pub fn call(&self, x: MathemaValue, y: MathemaValue) -> MathemaValue {
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

pub fn is_unary_func(name: &str) -> bool {
    UNARY_FUNCS.contains_key(name)
}

pub fn is_binary_func(name: &str) -> bool {
    BINARY_FUNCS.contains_key(name)
}

pub fn call_unary_func(name: &str, x: MathemaValue) -> MathemaValue {
    let func = UNARY_FUNCS.get(name).unwrap();
    func.call(x)
}

pub fn call_binary_func(name: &str, x: MathemaValue, y: MathemaValue) -> MathemaValue {
    let func = BINARY_FUNCS.get(name).unwrap();
    func.call(x, y)
}

mod functions {
    use crate::value::MathemaValue;

    pub fn sin(x: MathemaValue) -> MathemaValue {
        MathemaValue { inner: x.number().sin() }
    }

    pub fn cos(x: MathemaValue) -> MathemaValue {
        MathemaValue { inner: x.number().cos() }
    }

    pub fn tan(x: MathemaValue) -> MathemaValue {
        MathemaValue { inner: x.number().tan() }
    }

    pub fn log(x: MathemaValue, b: MathemaValue) -> MathemaValue {
        let inner = x.number().log10() / b.number().log10();
        MathemaValue { inner }
    }

    pub fn ln(x: MathemaValue) -> MathemaValue {
        let inner = x.number().ln();
        MathemaValue { inner }
    }
}
