macro_rules! define_constants {
    ( $( $name:ident = $def:expr ;)* ) => {
        pub(crate) const BUILTIN_VARS: &[&'static str] = &[
            "ans",
            $(
                stringify!($name),
            )*
        ];

        pub(crate) fn declare_constants() -> std::collections::HashMap<Box<str>, f64> {
            let mut map = std::collections::HashMap::new();
            $(
                let _ = map.insert(stringify!($name).into(), $def);
            )*
            map
        }
    }
}

define_constants! {
    pi = std::f64::consts::PI;
    e = std::f64::consts::E;
    tau = std::f64::consts::TAU;
}

