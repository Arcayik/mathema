macro_rules! define_constants {
    ( $( $name:ident = $def:expr ;)* ) => {
        pub const BUILTIN_VARS: &[&'static str] = &[
            "ans",
            $(
                stringify!($name),
            )*
        ];

        pub(crate) fn declare_constants(ctxt: &mut dyn $crate::context::Context) {
            $(
                let _ = ctxt.set_variable(stringify!($name).into(), $def);
            )*
        }
    }
}

use crate::function::Function;

macro_rules! declare_functions {
    (
        $( $name:ident ( $($args:ident),* ) = $body:expr ; )*
    ) => {
        pub const BUILTIN_FUNCS: &[&'static str] = &[
            $( stringify!($name) ),*
        ];

        $(
            #[expect(non_upper_case_globals)]
            pub const $name: Function = Function::new_builtin(
                stringify!($name),
                &[ $( stringify!($args) ),* ],
                $body
            );
        )*

            pub(crate) fn declare_functions(ctxt: &mut dyn $crate::context::Context) {
                $(
                    ctxt.set_function(
                        stringify!($name).into(),
                        $name.into()
                    )
                );*;
            }
    };
}

define_constants! {
    pi = std::f64::consts::PI;
    e = std::f64::consts::E;
    tau = std::f64::consts::TAU;
}

declare_functions! {
    log(x, base) = |args| f64::log(args[0], args[1]);
    ln(x) = |args| f64::ln(args[0]);
    sin(x) = |args| f64::sin(args[0]);
    cos(x) = |args| f64::cos(args[0]);
    tan(x) = |args| f64::tan(args[0]);
}

