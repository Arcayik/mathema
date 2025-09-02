use std::rc::Rc;

use crate::{
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
    fn_name: Box<str>,
    params: Box<[Box<str>]>,
    context: &'c Context,
}

impl<'c> FunctionBuilder<'c> {
    fn new(fn_name: Box<str>, params: Box<[Box<str>]>, context: &'c Context) -> Self {
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
                if let Some(idx) = self.params.iter().position(|p| p == name) {
                    return Ok(ValueNode::Param(ParamNode { idx }))
                } 

                // then check if it is a variable in scope
                let num = self.context.get_variable(&name)
                    .ok_or(ExprError::UndefinedVar(id))?;

                Ok(ValueNode::Num(NumNode { value: num }))
            }
        }
    }

    fn convert_binary(&self, binary: ExprBinary) -> Result<BinaryNode, FunctionError> {
        Ok(BinaryNode {
            left: Box::new(self.convert_algebra_tree(*binary.lhs)?),
            op: binary.op.into(),
            right: Box::new(self.convert_algebra_tree(*binary.rhs)?)
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
        if fn_call.name.repr == self.fn_name {
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

pub struct Function {
    name: Box<str>,
    params: Box<[Box<str>]>,
    ast: AlgebraTree
}

impl Function {
    pub fn new(name: Box<str>, params: Box<[Box<str>]>, ast: AlgebraTree) -> Self {
        Function { name, params, ast }
    }

    fn num_params(&self) -> usize {
        self.params.len()
    }

    pub fn call(&self, args: &[f64]) -> f64 {
        assert_eq!(args.len(), self.num_params());

        let mut fn_args = Vec::new();
        for (idx, param) in self.params.iter().enumerate() {
            fn_args.push((param.as_ref(), args[idx]));
        }

        self.ast.eval(args)
    }
}

pub enum AlgebraTree {
    Value(ValueNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    FnCall(FnCallNode),
}

impl AlgebraTree {
    fn eval(&self, args: &[f64]) -> f64 {
        match self {
            Self::Value(v) => v.eval(args),
            Self::Binary(b) => b.eval(args),
            Self::Unary(u) => u.eval(args),
            Self::FnCall(f) => f.eval(args),
        }
    }
}

pub struct NumNode {
    value: f64
}

impl NumNode {
    pub fn eval(&self) -> f64 {
        self.value
    }
}

pub struct ParamNode {
    idx: usize
}

impl ParamNode {
    fn eval(&self, args: &[f64]) -> f64 {
        args[self.idx]
    }
}

pub enum ValueNode {
    Num(NumNode),
    Param(ParamNode),
}

impl ValueNode {
    fn eval(&self, args: &[f64]) -> f64 {
        match self {
            Self::Num(n) => n.eval(),
            Self::Param(p) => p.eval(args),
        }
    }
}

pub struct BinaryNode {
    left: Box<AlgebraTree>,
    op: BinaryOperation,
    right: Box<AlgebraTree>,
}

impl BinaryNode {
    fn eval(&self, args: &[f64]) -> f64 {
        let left = self.left.eval(args);
        let right = self.right.eval(args);
        match self.op {
            BinaryOperation::Add => left + right,
            BinaryOperation::Sub => left - right,
            BinaryOperation::Mul => left * right,
            BinaryOperation::Div => left / right,
        }
    }
}

pub enum BinaryOperation { Add, Sub, Mul, Div }

impl From<BinOp> for BinaryOperation {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Add(_) => Self::Add,
            BinOp::Sub(_) => Self::Sub,
            BinOp::Mul(_) => Self::Mul,
            BinOp::Div(_) => Self::Div,
        }
    }
}

pub struct UnaryNode {
    op: UnaryOperation,
    tree: Box<AlgebraTree>,
}

impl UnaryNode {
    fn eval(&self, args: &[f64]) -> f64 {
        let inner_tree = self.tree.as_ref();
        let value = inner_tree.eval(args);
        match self.op {
            UnaryOperation::Neg => - value,
        }
    }
}

pub enum UnaryOperation { Neg }

impl From<UnaryOp> for UnaryOperation {
    fn from(value: UnaryOp) -> Self {
        match value {
            UnaryOp::Neg(_) => UnaryOperation::Neg,
        }
    }
}

pub struct FnCallNode {
    args: Box<[AlgebraTree]>,
    func: Rc<Function>
}

impl FnCallNode {
    fn eval_arg_exprs(&self, args: &[f64]) -> Vec<f64> {
        let eval_results = self.args
            .iter()
            .map(|a| AlgebraTree::eval(a, args))
            .collect();

        eval_results
    }

    fn eval(&self, args: &[f64]) -> f64 {
        let args = self.eval_arg_exprs(args);
        self.func.call(&args)
    }
}

pub fn create_function(
    name: Box<str>,
    params: Box<[Box<str>]>,
    expr: Expr,
    ctxt: &Context
) -> Result<Function, FunctionError> {
    let builder = FunctionBuilder::new(name, params, ctxt);
    builder.build_function(expr)
}
