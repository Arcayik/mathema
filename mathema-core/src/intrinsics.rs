use std::{collections::HashMap, sync::LazyLock};

type IntrinsicFn = fn(&[f64]) -> f64;

pub static CONSTANTS: LazyLock<HashMap<String, f64>> = LazyLock::new(declare_constants);
pub static CONST_FNS: LazyLock<HashMap<String, IntrinsicFn>> = LazyLock::new(declare_functions);

macro_rules! define_constants {
    ( $( $name:ident = $def:expr ;)* ) => {
        pub(crate) fn declare_constants() -> HashMap<String, f64> {
            let mut hashmap = HashMap::new();
            $(
                let _ = hashmap.insert(stringify!($name).into(), $def)
            );*;
            hashmap
        }
    }
}

macro_rules! define_functions {
    (
        $( $name:ident ( $($args:ident),* ) = $body:expr ; )*
    ) => {
        pub(crate) fn declare_functions() -> HashMap<String, IntrinsicFn> {
            let mut hashmap = HashMap::new();
            $(
                let func: IntrinsicFn = $body;
                hashmap.insert(stringify!($name).into(), func)
            );*;
            hashmap
        }
    };
}

define_constants! {
    pi = std::f64::consts::PI;
    e = std::f64::consts::E;
    tau = std::f64::consts::TAU;
}

define_functions! {
    log(x, b) = |args| f64::log(args[0], args[1]);
    ln(x) =     |args| f64::ln(args[0]);
    sin(x) =    |args| f64::sin(args[0]);
    cos(x) =    |args| f64::cos(args[0]);
    tan(x) =    |args| f64::tan(args[0]);
}

