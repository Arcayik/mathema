use std::rc::Rc;

use crate::{
    context::{Context, FuncResult},
    parsing::{
        ast::{BinOp, Expr, ExprBinary, ExprFnCall, ExprGroup, ExprUnary, ExprValue, ExprVisit, UnaryOp},
        token::Span
    },
    Name
};

#[derive(Debug)]
pub struct Algebra {
    pub(crate) params: Vec<Name>,
    pub(crate) tree: AlgebraNode,
}

impl Algebra {
    pub fn evaluate(&self, context: &Context, args: &[f64]) -> Result<f64, Vec<EvalError>> {
        let args_off = args.len() as isize - self.params.len() as isize;
        if args_off != 0 {
            todo!()
            // return Err(vec![EvalError::BadArgs(args_off)])
        }

        let value = Evaluator::run(self, context, args);
        todo!()
    }

    pub fn num_params(&self) -> usize {
        self.params.len()
    }
}

pub(crate) type AlgebraNode = Box<AlgebraTree>;

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

impl From<f64> for AlgebraTree {
    fn from(value: f64) -> Self {
        ValueNode::Num(value).into()
    }
}

impl From<Name> for AlgebraTree {
    fn from(value: Name) -> Self {
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
pub enum ValueNode {
    Num(f64),
    Var(Name),
}

#[derive(Debug, PartialEq)]
pub struct BinaryNode {
    pub(crate) left: AlgebraNode,
    pub(crate) op: AlgBinOp,
    pub(crate) right: AlgebraNode,
}

#[derive(Debug, PartialEq)]
pub enum AlgBinOp { Add, Sub, Mul, Div, Exp }

impl AlgBinOp {
    pub fn is_additive(&self) -> bool {
        matches!(self, AlgBinOp::Add | AlgBinOp::Sub)
    }

    pub fn is_multiplicative(&self) -> bool {
        matches!(self, AlgBinOp::Mul | AlgBinOp::Div)
    }
}

impl From<BinOp> for AlgBinOp {
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
    pub(crate) op: AlgUnaryOp,
    pub(crate) tree: AlgebraNode,
}

#[derive(Debug, PartialEq)]
pub enum AlgUnaryOp { Neg }

impl From<UnaryOp> for AlgUnaryOp {
    fn from(value: UnaryOp) -> Self {
        match value {
            UnaryOp::Neg(_) => AlgUnaryOp::Neg,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FnCallNode {
    pub(crate) args: Box<[AlgebraNode]>,
    pub(crate) func: Name,
}

pub struct AlgebraConverter {
    stored_node: Option<AlgebraTree>,
    params: Vec<Name>,
}

impl AlgebraConverter {
    pub fn new(params: Vec<Name>) -> Self {
        AlgebraConverter {
            params,
            stored_node: None,
        }
    }

    pub fn build(mut self, expr: &Expr) -> Algebra {
        let tree = self.build_tree(expr);
        let tree = Box::new(tree);
        let params = self.params;
        Algebra { params, tree }
    }

    fn build_tree(&mut self, expr: &Expr) -> AlgebraTree {
        self.visit(expr);
        self.take_node()
    }

    fn take_node(&mut self) -> AlgebraTree {
        self.stored_node.take().expect("AlgebraConverter should have stored node to take")
    }

    fn store_node(&mut self, node: AlgebraTree) {
        if self.stored_node.is_some() {
            panic!("AlgebraTreeBuilder already has a node stored")
        } else {
            self.stored_node = Some(node)
        };
    }
}

impl ExprVisit for AlgebraConverter {
    fn visit_value(&mut self, node: &ExprValue) {
        let new_node = match node {
            ExprValue::Literal(l) => ValueNode::Num(l.num),
            ExprValue::Ident(id) => ValueNode::Var(id.name.clone())
        };

        self.store_node(new_node.into())
    }

    fn visit_binary(&mut self, node: &ExprBinary) {
        self.visit_expr(&node.lhs);
        let left = self.take_node();
        self.visit_expr(&node.rhs);
        let right = self.take_node();

        self.store_node(
            BinaryNode {
                left: Box::new(left),
                op: node.op.clone().into(),
                right: Box::new(right),
            }.into()
        )
    }

    fn visit_unary(&mut self, node: &ExprUnary) {
        self.visit_expr(&node.expr);
        let expr_node = self.take_node();

        self.store_node(
            UnaryNode {
                op: node.op.clone().into(),
                tree: Box::new(expr_node)
            }.into()
        );
    }

    fn visit_group(&mut self, node: &ExprGroup) {
        self.visit_expr(&node.expr);
    }

    fn visit_fn_call(&mut self, node: &ExprFnCall) {
        let args: Box<[AlgebraNode]> = node.inputs.iter()
            .map(|expr| {
                self.visit_expr(expr);
                let tree = self.take_node();
                Box::new(tree)
            })
            .collect();

        let new_node = FnCallNode {
            args,
            func: node.fn_name.name.clone()
        }.into();

        self.store_node(new_node);
    }
}

pub trait AlgebraVisit {
    fn visit(&mut self, algebra: &Algebra) {
        self.visit_tree(&algebra.tree);
    }

    fn visit_tree(&mut self, node: &AlgebraTree) {
        visit::walk_tree(self, node);
    }

    fn visit_value(&mut self, node: &ValueNode) {
        let _ = node;
    }

    fn visit_binary(&mut self, node: &BinaryNode) {
        visit::walk_binary(self, node);
    }

    fn visit_unary(&mut self, node: &UnaryNode) {
        visit::walk_unary(self, node);
    }

    fn visit_fn_call(&mut self, node: &FnCallNode) {
        visit::walk_fn_call(self, node);
    }
}

mod visit {
    use crate::algebra::{AlgebraTree, AlgebraVisit, BinaryNode, FnCallNode, UnaryNode};

    pub fn walk_tree<V: AlgebraVisit + ?Sized>(visitor: &mut V, node: &AlgebraTree) {
        match node {
            AlgebraTree::Value(n) => visitor.visit_value(n),
            AlgebraTree::Binary(n) => visitor.visit_binary(n),
            AlgebraTree::Unary(n) => visitor.visit_unary(n),
            AlgebraTree::FnCall(n) => visitor.visit_fn_call(n),
        }
    }

    pub fn walk_unary<V: AlgebraVisit + ?Sized>(visitor: &mut V, node: &UnaryNode) {
        visitor.visit_tree(&node.tree);
    }

    pub fn walk_binary<V: AlgebraVisit + ?Sized>(visitor: &mut V, node: &BinaryNode) {
        visitor.visit_tree(&node.left);
        visitor.visit_tree(&node.right);
    }

    pub fn walk_fn_call<V: AlgebraVisit + ?Sized>(visitor: &mut V, node: &FnCallNode) {
        node.args.iter().for_each(|t| visitor.visit_tree(t));
    }
}

pub enum EvalErrorKind {
    UndefinedVar(Name),
    UndefinedFunc(Name),
    BadArgs(isize),
}

pub struct EvalError {
    pub kind: EvalErrorKind,
    pub(crate) source: Rc<str>,
    pub(crate) span: Span
}

impl EvalError {
    pub fn new(kind: EvalErrorKind, source: Rc<str>, span: Span) -> Self {
        EvalError { kind, source, span }
    }
}

pub struct Evaluator<'a> {
    input: &'a Algebra,
    args: &'a [f64],
    context: &'a Context,

    stored_value: Option<f64>,
    errors: Vec<EvalError>
}

impl<'a> Evaluator<'a> {
    fn new(input: &'a Algebra, context: &'a Context, args: &'a [f64]) -> Self {
        Evaluator {
            input,
            args,
            context,
            stored_value: None,
            errors: Vec::new()
        }
    }

    pub fn run(algebra: &'a Algebra, context: &'a Context, args: &'a [f64]) -> f64 {
        let eval = Evaluator::new(algebra, context, args);
        todo!()
    }

    fn error(&mut self, kind: EvalErrorKind) {
        let source = todo!();
        let span = todo!();
        let err = EvalError::new(kind, source, span);
        self.errors.push(err);
    }

    fn eval_variable(&mut self, name: &Name) {
        // TODO check param

        match self.context.get_var(name) {
            Some(var) => self.store_value(var),
            None => self.error(EvalErrorKind::UndefinedVar(name.clone())),
        };
    }

    fn store_value(&mut self, value: f64) {
        if self.stored_value.is_some() {
            panic!("Evaluator already has a value stored");
        } else {
            self.stored_value = Some(value)
        };
    }

    fn take_value(&mut self) -> Option<f64> {
        self.stored_value.take()
    }

    fn clear_value(&mut self) {
        self.stored_value = None;
    }
}

impl<'a> AlgebraVisit for Evaluator<'a> {
    fn visit(&mut self, algebra: &Algebra) {
        self.clear_value();
        self.visit_tree(&algebra.tree);
    }

    fn visit_value(&mut self, node: &ValueNode) {
        match node {
            ValueNode::Num(n) => self.store_value(*n),
            ValueNode::Var(v) => self.eval_variable(v)
        };
    }

    fn visit_unary(&mut self, node: &UnaryNode) {
        self.visit_tree(&node.tree);
        let inner_value = self.take_value();

        if let Some(val) = inner_value {
            match node.op {
                AlgUnaryOp::Neg => self.store_value(- val)
            }
        }
    }

    fn visit_binary(&mut self, node: &BinaryNode) {
        self.visit_tree(&node.left);
        let left = self.take_value();
        self.visit_tree(&node.right);
        let right = self.take_value();

        if let (Some(l), Some(r)) = (left, right) {
            let value = match node.op {
                AlgBinOp::Add => l + r,
                AlgBinOp::Sub => l - r,
                AlgBinOp::Mul => l * r,
                AlgBinOp::Div => l / r,
                AlgBinOp::Exp => l.powf(r),
            };

            self.store_value(value);
        }
    }

    fn visit_fn_call(&mut self, node: &FnCallNode) {
        let args: Option<Vec<f64>> = node.args.iter()
            .map(|node| {
                self.visit_tree(&node);
                self.take_value()
            })
            .collect();

        if let Some(arg_vec) = args {
            let result = self.context.call_func(&node.func, &arg_vec);

            if let FuncResult::Return(value) = result {
                self.store_value(value);
            }
        }
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
