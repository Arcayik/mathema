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

pub struct BinaryFunc(fn(MathemaValue, MathemaValue) -> MathemaValue);

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

mod functions {
    use crate::value::MathemaValue;

    pub fn sin(value: MathemaValue) -> MathemaValue {
        MathemaValue { inner: value.number().sin() }
    }

    pub fn cos(value: MathemaValue) -> MathemaValue {
        MathemaValue { inner: value.number().cos() }
    }

    pub fn tan(value: MathemaValue) -> MathemaValue {
        MathemaValue { inner: value.number().tan() }
    }

    pub fn log(value: MathemaValue, base: MathemaValue) -> MathemaValue {
        let inner = value.number().log10() / base.number().log10();
        MathemaValue { inner }
    }

    pub fn ln(value: MathemaValue) -> MathemaValue {
        let inner = value.number().ln();
        MathemaValue { inner }
    }
}
