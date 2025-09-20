use std::rc::Rc;

use crate::{
    context::Context, expr::*, function::Function, token::Spanned
};

#[derive(Debug)]
pub struct Algebra {
    pub(crate) params: Vec<String>,
    pub(crate) tree: AlgebraTree,
}

impl Algebra {
    pub fn accept<V: AlgebraVisit>(&self, visitor: &mut V) {
        visitor.visit(&self.tree);
    }
}

pub(crate) type AlgebraNode = Rc<AlgebraTree>;

#[derive(Debug, PartialEq)]
pub enum AlgebraTree {
    Value(ValueNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    FnCall(FnCallNode),
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

#[derive(Debug, PartialEq)]
pub struct NumNode {
    pub(crate) value: f64
}

#[derive(Debug, PartialEq)]
pub struct ParamNode {
    pub(crate) idx: usize
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

#[derive(Debug, PartialEq)]
pub struct BinaryNode {
    pub(crate) left: AlgebraNode,
    pub(crate) op: BinaryOperation,
    pub(crate) right: AlgebraNode,
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

pub struct AlgebraBuilder<'b> {
    context: &'b dyn Context,
    stored_node: Option<AlgebraTree>,
    params: Vec<String>,
    errors: Vec<ExprError>,
}

impl<'b> AlgebraBuilder<'b> {
    pub fn new(context: &'b dyn Context, params: Vec<String>) -> Self {
        AlgebraBuilder {
            context,
            params,
            stored_node: None,
            errors: Vec::new()
        }
    }

    pub fn build(mut self, expr: &Expr) -> Result<Algebra, Vec<ExprError>> {
        if let Some(tree) = self.build_tree(expr) {
            let params = self.params;
            Ok(Algebra { params, tree })
        } else {
            Err(self.errors)
        }
    }

    fn build_tree(&mut self, expr: &Expr) -> Option<AlgebraTree> {
        self.visit(expr);
        self.take_node()
    }

    fn take_node(&mut self) -> Option<AlgebraTree> {
        self.stored_node.take()
    }

    fn store_node(&mut self, node: AlgebraTree) {
        if self.stored_node.is_some() {
            panic!("AlgebraTreeBuilder already has a node stored")
        } else {
            self.stored_node = Some(node)
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
                // check if ident is a parameter, which takes priority
                let new_node = if let Some(idx) = self.params.iter().position(|v| **v == *id.repr) {
                    ParamNode { idx }.into()
                } else {
                    let value = match self.context.get_variable(&id.repr) {
                        Some(v) => v,
                        None => {
                            self.errors.push(ExprError::UndefinedVar(id.clone()));
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

        if let (Some(l), Some(r)) = (left, right) {
            self.store_node(
                BinaryNode {
                    left: l.into(),
                    op: node.op.clone().into(),
                    right: r.into(),
                }.into()
            )
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

        let args_off = args.len() as isize - func.num_params() as isize;
        if args_off != 0 {
            let name = node.name.repr.clone();
            let span = node.parens.span();
            self.errors.push(ExprError::BadFnCall(name, span, args_off));
            return
        }

        let new_node = FnCallNode {
            args,
            func
        }.into();

        self.store_node(new_node);
    }
}

pub trait AlgebraVisit {
    fn visit(&mut self, node: &AlgebraTree) {
        self.visit_tree(node);
    }

    fn visit_tree(&mut self, node: &AlgebraTree) {
        match node {
            AlgebraTree::Value(n) => self.visit_value(n),
            AlgebraTree::Unary(n) => self.visit_unary(n),
            AlgebraTree::Binary(n) => self.visit_binary(n),
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

pub struct Evaluator<'a> {
    args: &'a [f64],

    stored_value: Option<f64>,
}

impl<'a> Evaluator<'a> {
    pub fn new(args: &'a [f64]) -> Self {
        Evaluator {
            args,
            stored_value: None,
        }
    }

    pub fn run(&mut self, algebra: &AlgebraTree) -> f64 {
        self.visit(algebra);
        self.take_value()
    }

    pub fn take_answer(&mut self) -> Option<f64> {
        self.stored_value.take()
    }

    pub fn get_args(&self) -> &[f64] {
        self.args
    }

    pub fn set_args(&mut self, args: &'a [f64]) {
        self.args = args;
    }

    fn store_value(&mut self, value: f64) {
        if self.stored_value.is_some() {
            panic!("Evaluator already has a value stored");
        } else {
            self.stored_value = Some(value)
        };
    }

    fn take_value(&mut self) -> f64 {
        self.stored_value.take().expect("Evaluator has no value stored")
    }

    fn clear_value(&mut self) {
        self.stored_value = None;
    }
}

impl<'a> AlgebraVisit for Evaluator<'a> {
    fn visit(&mut self, node: &AlgebraTree) {
        self.clear_value();
        self.visit_tree(node);
    }

    fn visit_value(&mut self, node: &ValueNode) {
        let value = match node {
            ValueNode::Num(n) => n.value,
            ValueNode::Param(v) => self.args[v.idx],
        };
        self.store_value(value);
    }

    fn visit_unary(&mut self, node: &UnaryNode) {
        self.visit_tree(&node.tree);
        let inner_value = self.take_value();
        match node.op {
            UnaryOperation::Neg => self.store_value(- inner_value)
        }
    }

    fn visit_binary(&mut self, node: &BinaryNode) {
        self.visit_tree(&node.left);
        let left = self.take_value();
        self.visit_tree(&node.right);
        let right = self.take_value();

        let value = match node.op {
            BinaryOperation::Add => left + right,
            BinaryOperation::Sub => left - right,
            BinaryOperation::Mul => left * right,
            BinaryOperation::Div => left / right,
            BinaryOperation::Exp => left.powf(right),
        };

        self.store_value(value);
    }

    fn visit_fn_call(&mut self, node: &FnCallNode) {
        let mut args = Vec::new();
        for arg in node.args.iter() {
            self.visit_tree(arg);
            let arg_value = self.take_value();
            args.push(arg_value);
        }

        let value = node.func.call(&args)
            .expect("function args checked at expr conversion");
        self.store_value(value);
    }

    fn visit_tree(&mut self, node: &AlgebraTree) {
        match node {
            AlgebraTree::Value(n) => self.visit_value(n),
            AlgebraTree::Unary(n) => self.visit_unary(n),
            AlgebraTree::Binary(n) => self.visit_binary(n),
            AlgebraTree::FnCall(n) => self.visit_fn_call(n),
        }
    }
}
