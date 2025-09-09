use std::rc::Rc;

use crate::{
    algebra::*,
    context::Context,
    expr::*,
};

#[derive(Debug)]
pub enum FunctionError {
    ExprError(ExprError),
    Recursion(ExprFnCall),
}

impl From<ExprError> for FunctionError {
    fn from(value: ExprError) -> Self {
        Self::ExprError(value)
    }
}

struct FunctionBuilder<'c> {
    fn_name: String,
    params: Vec<String>,
    context: &'c Context,
}

impl<'c> FunctionBuilder<'c> {
    fn new(fn_name: String, params: Vec<String>, context: &'c Context) -> Self {
        FunctionBuilder { fn_name, params, context }
    }

    fn build_function(self, expr: Expr) -> Result<Function, FunctionError> {
        let ast = self.convert_algebra_tree(expr)?;
        let Self { params, .. } = self;
        Ok(Function::new(self.fn_name, params, ast))
    }

    fn convert_algebra_tree(&self, expr: Expr) -> Result<AlgebraTree, FunctionError> {
        Ok(match expr {
            Expr::Value(v) => AlgebraTree::Value(self.convert_value(v)?),
            Expr::Binary(b) => AlgebraTree::Binary(self.convert_binary(b)?),
            Expr::Unary(u) => AlgebraTree::Unary(self.convert_unary(u)?),
            Expr::Group(g) => self.convert_algebra_tree(*g.expr)?,
            Expr::FnCall(c) => AlgebraTree::FnCall(self.convert_fn_call(c)?),
        })
    }

    fn convert_value(&self, value: ExprValue) -> Result<ValueNode, FunctionError> {
        match value {
            ExprValue::Literal(l) => Ok(ValueNode::Num(NumNode { value: l.num })),
            ExprValue::Ident(id) => {
                let name = &id.repr;
                // check if it is a function parameter
                if let Some(idx) = self.params.iter().position(|p| **p == **name) {
                    return Ok(ValueNode::Param(ParamNode { idx }))
                } 

                // then check if it is a variable in scope
                let num = self.context.get_variable(name)
                    .ok_or(ExprError::UndefinedVar(id))?;

                Ok(ValueNode::Num(NumNode { value: num }))
            }
        }
    }

    fn convert_binary(&self, binary: ExprBinary) -> Result<BinaryNode, FunctionError> {
        Ok(BinaryNode {
            left: Rc::new(self.convert_algebra_tree(*binary.lhs)?),
            op: binary.op.into(),
            right: Rc::new(self.convert_algebra_tree(*binary.rhs)?)
        })
    }

    fn convert_unary(&self, unary: ExprUnary) -> Result<UnaryNode, FunctionError> {
        Ok(UnaryNode {
            op: unary.op.into(),
            tree: Box::new(self.convert_algebra_tree(*unary.expr)?),
        })
    }

    fn convert_fn_call(&self, fn_call: ExprFnCall) -> Result<FnCallNode, FunctionError> {
        // naive recursion check
        if *fn_call.name.repr == *self.fn_name {
            return Err(FunctionError::Recursion(fn_call))
        }

        let name = fn_call.name.repr.as_ref();
        let func = self.context.get_function(name)
            .ok_or(ExprError::UndefinedFunc(fn_call.name))?;

        let exprs = fn_call.inputs.values();
        let args: Result<Box<_>, _> = exprs
            .into_iter()
            .map(|e| self.convert_algebra_tree(e))
            .collect();

        Ok(FnCallNode { args: args?, func })
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    inner: FunctionInner
}

impl Function {
    pub fn new(name: String, params: Vec<String>, ast: AlgebraTree) -> Self {
        Function { inner: FunctionInner::new_user_defined(name, params, ast) }
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

    pub fn call(&self, args: &[f64]) -> f64 {
        assert_eq!(args.len(), self.num_params());

        let mut fn_args = Vec::new();
        for (idx, param) in self.inner.params().into_iter().enumerate() {
            fn_args.push((param, args[idx]));
        }

        self.inner.call(args)
    }
}

#[derive(Debug)]
enum FunctionInner {
    UserDefined {
        name: String,
        params: Vec<String>,
        body: Box<AlgebraTree>,
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
    fn new_user_defined(name: String, params: Vec<String>, ast: AlgebraTree) -> Self {
        let body = Box::new(ast);
        FunctionInner::UserDefined { name, params, body }
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
            Self::UserDefined { params, .. } => params.iter().map(|s| &s[..]).collect(),
            Self::Builtin { params, .. } => params.to_owned().into()
        }
    }

    fn num_params(&self) -> usize {
        match self {
            Self::UserDefined { params, .. } => params.len(),
            Self::Builtin { params, .. } => params.len(),
        }
    }

    fn call(&self, args: &[f64]) -> f64 {
        assert_eq!(args.len(), self.num_params());

        let mut fn_args = Vec::new();
        for (idx, param) in self.params().into_iter().enumerate() {
            fn_args.push((param, args[idx]));
        }

        match self {
            FunctionInner::UserDefined { body, .. } => body.eval(args),
            FunctionInner::Builtin { body, .. } => body(args),
        }
    }
}

pub fn create_function(
    name: String,
    params: Vec<String>,
    expr: Expr,
    ctxt: &Context
) -> Result<Function, FunctionError> {
    let builder = FunctionBuilder::new(name, params, ctxt);
    builder.build_function(expr)
}
