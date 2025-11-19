use crate::{symbol::Symbol, value::MathemaValue};
use rug::float::Constant;
use std::{collections::HashMap, sync::LazyLock};

type IntrinsicFn = fn(&[MathemaValue]) -> MathemaValue;

pub static CONSTANTS: LazyLock<HashMap<Symbol, MathemaValue>> = LazyLock::new(declare_constants);
pub static CONST_FNS: LazyLock<HashMap<Symbol, IntrinsicFn>> = LazyLock::new(declare_functions);

macro_rules! define_constants {
    (
        $( $name:ident = $def:expr ;)*
    ) => {
        fn declare_constants() -> HashMap<Symbol, MathemaValue> {
            let mut hashmap = HashMap::new();
            $(
                let value = $crate::value::MathemaValue { inner: $def };
                let _ = hashmap.insert(Symbol::intern(stringify!($name)), value)
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
            #[allow(unused_mut)]
            let mut hashmap = HashMap::new();
            $(
                let name = Symbol::intern(stringify!($name));
                let func: IntrinsicFn = $body;
                hashmap.insert(name, func);
            )*
            hashmap
        }
    };
}

define_constants! {
    pi = rug::Float::with_val(512, Constant::Pi);
    tau = rug::Float::with_val(512, Constant::Pi) * 2;
    e = rug::Float::with_val(512, Constant::Euler);
}

define_functions! {
    log(x, b) = |args| {
        let inner = rug::Float::log10(args[0].inner.clone()) / rug::Float::log10(args[1].inner.clone());
        MathemaValue { inner }
    };
    // ln(x) =     |args| f64::ln(args[0]);
    // sin(x) =    |args| f64::sin(args[0]);
    // cos(x) =    |args| f64::cos(args[0]);
    // tan(x) =    |args| f64::tan(args[0]);
}

