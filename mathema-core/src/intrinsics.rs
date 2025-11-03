use std::{collections::HashMap, sync::LazyLock};

use crate::symbol::Symbol;

type IntrinsicFn = fn(&[f64]) -> f64;

pub static CONSTANTS: LazyLock<HashMap<Symbol, f64>> = LazyLock::new(declare_constants);
pub static CONST_FNS: LazyLock<HashMap<Symbol, IntrinsicFn>> = LazyLock::new(declare_functions);

macro_rules! define_constants {
    ( $( $name:ident = $def:expr ;)* ) => {
        fn declare_constants() -> HashMap<Symbol, f64> {
            let mut hashmap = HashMap::new();
            $(
                let _ = hashmap.insert(Symbol::intern(stringify!($name)), $def)
            );*;
            hashmap
        }
    }
}

macro_rules! define_functions {
    (
        $( $name:ident ( $($args:ident),* ) = $body:expr ; )*
    ) => {
        fn declare_functions() -> HashMap<Symbol, IntrinsicFn> {
            let mut hashmap = HashMap::new();
            $(
                let func: IntrinsicFn = $body;
                hashmap.insert(Symbol::intern(stringify!($name)), func)
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

