use std::rc::Rc;

use crate::{
    function::Function,
    expr::*,
    context::Context,
};

#[derive(Debug)]
pub struct Algebra {
    pub(crate) params: Vec<String>,
    pub(crate) tree: AlgebraTree,
}

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
        ValueNode::Var(value).into()
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

#[derive(Debug, PartialEq)]
pub struct NumNode {
    pub(crate) value: f64
}

impl NumNode {
    pub fn eval(&self) -> f64 {
        self.value
    }
}

#[derive(Debug, PartialEq)]
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
    Var(ParamNode),
}

impl From<NumNode> for ValueNode {
    fn from(value: NumNode) -> Self {
        Self::Num(value)
    }
}

impl From<ParamNode> for ValueNode {
    fn from(value: ParamNode) -> Self {
        Self::Var(value)
    }
}

impl ValueNode {
    fn eval(&self, args: &[f64]) -> f64 {
        match self {
            Self::Num(n) => n.eval(),
            Self::Var(p) => p.eval(args),
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

#[derive(Debug, PartialEq)]
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

pub struct AlgebraBuilder<'b> {
    context: &'b dyn Context,
    node_queued: Option<AlgebraTree>,
    params: Vec<String>,
    errors: Vec<ExprError>,
}

impl<'b> AlgebraBuilder<'b> {
    pub fn new(context: &'b dyn Context, params: Vec<String>) -> Self {
        AlgebraBuilder {
            context,
            params,
            node_queued: None,
            errors: Vec::new()
        }
    }

    pub fn build(mut self, expr: &Expr) -> Result<Algebra, Vec<ExprError>> {
        if let Some(tree) = self.build_tree(expr) {
            Ok(Algebra {
                params: self.params,
                tree
            })
        } else {
            Err(self.errors)
        }
    }

    fn build_tree(&mut self, expr: &Expr) -> Option<AlgebraTree> {
        self.visit_expr(expr);
        self.take_node()
    }

    fn take_node(&mut self) -> Option<AlgebraTree> {
        self.node_queued.take()
    }

    fn store_node(&mut self, node: AlgebraTree) {
        if self.node_queued.is_some() {
            panic!("AlgebraTreeBuilder already has a node stored")
        } else {
            self.node_queued = Some(node)
        };
    }
}

impl<'b> ExprVisit for AlgebraBuilder<'b> {
    fn visit_value(&mut self, node: &ExprValue) {
        match node {
            ExprValue::Literal(l) => self.store_node(
                NumNode { value: l.num }.into()
            ),
            ExprValue::Ident(id) => {
                // check if ident name has already been seen
                let new_node = if let Some(found) = self.params.iter().position(|v| *v == *id.repr) {
                    ParamNode { idx: found }.into()
                } else {
                    let value = match self.context.get_variable(&id.repr) {
                        Some(v) => v,
                        None => {
                            self.errors.push(ExprError::UndefinedFunc(id.clone()));
                            return
                        }
                    };
                    NumNode { value }.into()
                };

                self.store_node(new_node)
            },
        }
    }

    fn visit_binary(&mut self, node: &ExprBinary) {
        self.visit_expr(&node.lhs);
        let left = self.take_node();
        self.visit_expr(&node.rhs);
        let right = self.take_node();

        match (left, right) {
            (Some(l), Some(r)) => self.store_node(
                BinaryNode {
                    left: l.into(),
                    op: node.op.clone().into(),
                    right: r.into(),
                }.into()
            ),

            _ => {}
        }

    }

    fn visit_unary(&mut self, node: &ExprUnary) {
        self.visit_expr(&node.expr);
        let expr = self.take_node();

        if let Some(en) = expr {
            self.store_node(
                UnaryNode {
                    op: node.op.clone().into(),
                    tree: Box::new(en)
                }.into()
            );
        }
    }

    fn visit_group(&mut self, node: &ExprGroup) {
        self.visit_expr(&node.expr);
    }

    fn visit_fn_call(&mut self, node: &ExprFnCall) {
        let args: Option<Box<[AlgebraTree]>> = node.inputs.iter()
            .map(|expr| {
                self.visit_expr(expr);
                self.take_node()
            })
            .collect();

        let args = match args {
            Some(a) => a,
            None => return
        };

        let func = match self.context.get_function(&node.name.repr) {
            Some(f) => f,
            None => {
                self.errors.push(ExprError::UndefinedFunc(node.name.clone()));
                return
            }
        };

        let new_node = FnCallNode {
            args,
            func
        }.into();

        self.store_node(new_node);
    }
}

pub trait AlgebraVisit {
    fn visit_tree(&mut self, node: &AlgebraTree) {
        match node {
            AlgebraTree::Value(n) => self.visit_value(n),
            AlgebraTree::Binary(n) => self.visit_binary(n),
            AlgebraTree::Unary(n) => self.visit_unary(n),
            AlgebraTree::FnCall(n) => self.visit_fn_call(n),
        }
    }

    fn visit_value(&mut self, node: &ValueNode) {
        let _ = node;
    }

    fn visit_binary(&mut self, node: &BinaryNode) {
        self.visit_tree(&node.left);
        self.visit_tree(&node.right);
    }

    fn visit_unary(&mut self, node: &UnaryNode) {
        self.visit_tree(&node.tree);
    }

    fn visit_fn_call(&mut self, node: &FnCallNode) {
        node.args.iter().for_each(|e| self.visit_tree(e));
    }
}
