use crate::{
    algebra::*,
    context::Context,
    parsing::expr::*,
};

#[derive(Debug)]
pub enum FunctionError {
    ExprError(ExprError),
    Recursion(ExprFnCall),
}

#[derive(Debug)]
pub struct CallError {
    pub(crate) args_off: isize,
}

impl From<ExprError> for FunctionError {
    fn from(value: ExprError) -> Self {
        Self::ExprError(value)
    }
}

impl FunctionError {
    pub fn from_vec(value: Vec<ExprError>) -> Vec<Self> {
        value.into_iter().map(FunctionError::from).collect()
    }
}

pub struct FunctionBuilder<'c> {
    fn_name: String,
    params: Vec<String>,
    context: &'c dyn Context,
}

impl<'c> FunctionBuilder<'c> {
    pub fn new(fn_name: String, params: Vec<String>, context: &'c dyn Context) -> Self {
        FunctionBuilder { fn_name, params, context }
    }

    pub fn build_function(self, expr: Expr) -> Result<Function, Vec<FunctionError>> {
        let alg_builder = AlgebraBuilder::new(self.context, self.params);
        let algebra: Algebra = alg_builder.build(&expr)
            .map_err(|e| e.into_iter().map(FunctionError::from).collect::<Vec<_>>())?;

        Ok(Function::new(self.fn_name, algebra))
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    inner: FunctionInner
}

impl Function {
    pub fn new(name: String, ast: Algebra) -> Self {
        Function { inner: FunctionInner::new_user_defined(name, ast) }
    }

    pub(crate) const fn new_builtin(name: &'static str, params: &'static [&'static str], body: fn(&[f64]) -> f64) -> Self {
        Function { inner: FunctionInner::new_builtin(name, params, body) }
    }

    pub fn name(&self) -> &str {
        self.inner.name()
    }

    pub fn params(&self) -> Box<[&str]> {
        self.inner.params()
    }

    pub fn num_params(&self) -> usize {
        self.inner.num_params()
    }

    pub fn call(&self, args: &[f64]) -> Result<f64, CallError> {
        if args.len() != self.num_params() {
            let diff = self.num_params() as isize - args.len() as isize;
            Err(CallError { args_off: diff })
        } else {
            Ok(self.inner.call(args))
        }
    }
}

#[derive(Debug)]
enum FunctionInner {
    UserDefined {
        name: String,
        body: Algebra
    },
    Builtin {
        name: &'static str,
        params: &'static [&'static str],
        body: fn(&[f64]) -> f64,
    }
}

impl PartialEq for FunctionInner {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.params() == other.params()
    }
}

impl FunctionInner {
    fn new_user_defined(name: String, body: Algebra) -> Self {
        FunctionInner::UserDefined { name, body }
    }

    const fn new_builtin(name: &'static str, params: &'static [&'static str], body: fn(&[f64]) -> f64) -> Self {
        FunctionInner::Builtin { name, params, body }
    }

    fn name(&self) -> &str {
        match self {
            Self::UserDefined { name, .. } => name,
            Self::Builtin { name, .. } => name
        }
    }

    fn params(&self) -> Box<[&str]> {
        match self {
            Self::UserDefined { body, .. } => body.params.iter().map(|s| &s[..]).collect(),
            Self::Builtin { params, .. } => params.to_owned().into()
        }
    }

    fn num_params(&self) -> usize {
        match self {
            Self::UserDefined { body, .. } => body.params.len(),
            Self::Builtin { params, .. } => params.len(),
        }
    }

    fn call(&self, args: &[f64]) -> f64 {
        // TODO: make this a recoverable error
        assert_eq!(args.len(), self.num_params());

        match self {
            FunctionInner::UserDefined { body, .. } => {
                let mut eval = Evaluator::new(args);
                eval.run(&body.tree)
            },
            FunctionInner::Builtin { body, .. } => body(args),
        }
    }
}

pub fn create_function(
    name: String,
    params: Vec<String>,
    expr: Expr,
    ctxt: &dyn Context
) -> Result<Function, Vec<FunctionError>> {
    let builder = FunctionBuilder::new(name, params, ctxt);
    builder.build_function(expr)
}
