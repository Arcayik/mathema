use std::rc::Rc;

use crate::{
    function::Function,
    expr::*,
};

pub(crate) type AlgebraNode = Rc<AlgebraTree>;

#[derive(Debug, PartialEq)]
pub enum AlgebraTree {
    Value(ValueNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    FnCall(FnCallNode),
}

impl AlgebraTree {
    pub fn eval(&self, args: &[f64]) -> f64 {
        match self {
            Self::Value(v) => v.eval(args),
            Self::Binary(b) => b.eval(args),
            Self::Unary(u) => u.eval(args),
            Self::FnCall(f) => f.eval(args),
        }
    }
}

impl From<ValueNode> for AlgebraTree {
    fn from(value: ValueNode) -> Self {
        Self::Value(value)
    }
}

impl From<NumNode> for AlgebraTree {
    fn from(value: NumNode) -> Self {
        ValueNode::Num(value).into()
    }
}

impl From<f64> for AlgebraTree {
    fn from(value: f64) -> Self {
        NumNode { value }.into()
    }
}

impl From<ParamNode> for AlgebraTree {
    fn from(value: ParamNode) -> Self {
        ValueNode::Param(value).into()
    }
}

impl From<BinaryNode> for AlgebraTree {
    fn from(value: BinaryNode) -> Self {
        Self::Binary(value)
    }
}

impl From<UnaryNode> for AlgebraTree {
    fn from(value: UnaryNode) -> Self {
        Self::Unary(value)
    }
}

impl From<FnCallNode> for AlgebraTree {
    fn from(value: FnCallNode) -> Self {
        Self::FnCall(value)
    }
}

#[derive(Debug)]
pub struct NumNode {
    pub(crate) value: f64
}

impl NumNode {
    pub fn eval(&self) -> f64 {
        self.value
    }
}

#[derive(Debug)]
pub struct ParamNode {
    pub(crate) idx: usize
}

impl ParamNode {
    fn eval(&self, args: &[f64]) -> f64 {
        args[self.idx]
    }
}

#[derive(Debug, PartialEq)]
pub enum ValueNode {
    Num(NumNode),
    Param(ParamNode),
}

impl From<NumNode> for ValueNode {
    fn from(value: NumNode) -> Self {
        Self::Num(value)
    }
}

impl From<ParamNode> for ValueNode {
    fn from(value: ParamNode) -> Self {
        Self::Param(value)
    }
}

impl ValueNode {
    fn eval(&self, args: &[f64]) -> f64 {
        match self {
            Self::Num(n) => n.eval(),
            Self::Param(p) => p.eval(args),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryNode {
    pub(crate) left: AlgebraNode,
    pub(crate) op: BinaryOperation,
    pub(crate) right: AlgebraNode,
}

impl BinaryNode {
    fn eval(&self, args: &[f64]) -> f64 {
        let left = self.left.clone().eval(args);
        let right = self.right.clone().eval(args);

        match self.op {
            BinaryOperation::Add => left + right,
            BinaryOperation::Sub => left - right,
            BinaryOperation::Mul => left * right,
            BinaryOperation::Div => left / right,
            BinaryOperation::Exp => left.powf(right),
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperation { Add, Sub, Mul, Div, Exp }

impl From<BinOp> for BinaryOperation {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Add(_) => Self::Add,
            BinOp::Sub(_) => Self::Sub,
            BinOp::Mul(_) => Self::Mul,
            BinOp::Div(_) => Self::Div,
            BinOp::Exp(_) => Self::Exp,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct UnaryNode {
    pub(crate) op: UnaryOperation,
    pub(crate) tree: Box<AlgebraTree>,
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

#[derive(Debug, PartialEq)]
pub enum UnaryOperation { Neg }

impl From<UnaryOp> for UnaryOperation {
    fn from(value: UnaryOp) -> Self {
        match value {
            UnaryOp::Neg(_) => UnaryOperation::Neg,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FnCallNode {
    pub(crate) args: Box<[AlgebraTree]>,
    pub(crate) func: std::rc::Rc<Function>
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
