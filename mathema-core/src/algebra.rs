use std::rc::Rc;

use crate::{
    context::{Context, CallError},
    parsing::{
        ast::{BinOp, Expr, ExprBinary, ExprFnCall, ExprGroup, ExprUnary, ExprValue, ExprVisit, UnaryOp},
        token::Span
    },
    Name
};

#[derive(Debug)]
pub struct Function {
    pub(crate) params: Vec<Name>,
    pub(crate) tree: Box<AlgExpr>,
}

impl Function {
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

#[derive(Debug, PartialEq)]
pub enum AlgExpr {
    Value(Value),
    Binary(Binary),
    Unary(Unary),
    FnCall(FnCall),
}

impl From<Value> for AlgExpr {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
}

impl From<f64> for AlgExpr {
    fn from(value: f64) -> Self {
        Value::Num(value).into()
    }
}

impl From<Name> for AlgExpr {
    fn from(value: Name) -> Self {
        Value::Var(value).into()
    }
}

impl From<Binary> for AlgExpr {
    fn from(value: Binary) -> Self {
        Self::Binary(value)
    }
}

impl From<Unary> for AlgExpr {
    fn from(value: Unary) -> Self {
        Self::Unary(value)
    }
}

impl From<FnCall> for AlgExpr {
    fn from(value: FnCall) -> Self {
        Self::FnCall(value)
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Var(Name),
    Param(usize)
}

#[derive(Debug, PartialEq)]
pub struct Binary {
    pub(crate) left: Box<AlgExpr>,
    pub(crate) op: AlgBinOp,
    pub(crate) right: Box<AlgExpr>,
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
pub struct Unary {
    pub(crate) op: AlgUnaryOp,
    pub(crate) tree: Box<AlgExpr>,
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
pub struct FnCall {
    pub(crate) args: Box<[Box<AlgExpr>]>,
    pub(crate) func: Name,
}

pub struct AlgebraConverter {
    stored_node: Option<AlgExpr>,
    params: Vec<Name>,
}

impl AlgebraConverter {
    pub fn new(params: Vec<Name>) -> Self {
        AlgebraConverter {
            params,
            stored_node: None,
        }
    }

    pub fn build(mut self, expr: &Expr) -> Function {
        let tree = self.build_tree(expr);
        let tree = Box::new(tree);
        let params = self.params;
        Function { params, tree }
    }

    fn build_tree(&mut self, expr: &Expr) -> AlgExpr {
        self.visit(expr);
        self.take_node()
    }

    fn take_node(&mut self) -> AlgExpr {
        self.stored_node.take().expect("AlgebraConverter should have stored node to take")
    }

    fn store_node(&mut self, node: AlgExpr) {
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
            ExprValue::Literal(l) => Value::Num(l.num),
            ExprValue::Ident(id) => Value::Var(id.name.clone())
        };

        self.store_node(new_node.into())
    }

    fn visit_binary(&mut self, node: &ExprBinary) {
        self.visit_expr(&node.lhs);
        let left = self.take_node();
        self.visit_expr(&node.rhs);
        let right = self.take_node();

        self.store_node(
            Binary {
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
            Unary {
                op: node.op.clone().into(),
                tree: Box::new(expr_node)
            }.into()
        );
    }

    fn visit_group(&mut self, node: &ExprGroup) {
        self.visit_expr(&node.expr);
    }

    fn visit_fn_call(&mut self, node: &ExprFnCall) {
        let args: Box<[Box<AlgExpr>]> = node.inputs.iter()
            .map(|expr| {
                self.visit_expr(expr);
                let tree = self.take_node();
                Box::new(tree)
            })
            .collect();

        let new_node = FnCall {
            args,
            func: node.fn_name.name.clone()
        }.into();

        self.store_node(new_node);
    }
}

pub trait AlgebraVisit {
    fn visit(&mut self, algebra: &Function) {
        self.visit_tree(&algebra.tree);
    }

    fn visit_tree(&mut self, node: &AlgExpr) {
        visit::walk_tree(self, node);
    }

    fn visit_value(&mut self, node: &Value) {
        let _ = node;
    }

    fn visit_binary(&mut self, node: &Binary) {
        visit::walk_binary(self, node);
    }

    fn visit_unary(&mut self, node: &Unary) {
        visit::walk_unary(self, node);
    }

    fn visit_fn_call(&mut self, node: &FnCall) {
        visit::walk_fn_call(self, node);
    }
}

mod visit {
    use crate::algebra::{AlgExpr, AlgebraVisit, Binary, FnCall, Unary};

    pub fn walk_tree<V: AlgebraVisit + ?Sized>(visitor: &mut V, node: &AlgExpr) {
        match node {
            AlgExpr::Value(n) => visitor.visit_value(n),
            AlgExpr::Binary(n) => visitor.visit_binary(n),
            AlgExpr::Unary(n) => visitor.visit_unary(n),
            AlgExpr::FnCall(n) => visitor.visit_fn_call(n),
        }
    }

    pub fn walk_unary<V: AlgebraVisit + ?Sized>(visitor: &mut V, node: &Unary) {
        visitor.visit_tree(&node.tree);
    }

    pub fn walk_binary<V: AlgebraVisit + ?Sized>(visitor: &mut V, node: &Binary) {
        visitor.visit_tree(&node.left);
        visitor.visit_tree(&node.right);
    }

    pub fn walk_fn_call<V: AlgebraVisit + ?Sized>(visitor: &mut V, node: &FnCall) {
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
    context: &'a Context,
    args: &'a [f64],

    stored_value: Option<f64>,
    errors: Vec<EvalError>
}

impl<'a> Evaluator<'a> {
    fn new(context: &'a Context, args: &'a [f64]) -> Self {
        Evaluator {
            args,
            context,
            stored_value: None,
            errors: Vec::new()
        }
    }

    pub fn run(algebra: &'a Function, context: &'a Context, args: &'a [f64]) -> f64 {
        let eval = Evaluator::new(context, args);
        todo!()
    }

    fn error(&mut self, kind: EvalErrorKind) {
        let source = todo!();
        let span = todo!();
        let err = EvalError::new(kind, source, span);
        self.errors.push(err);
    }

    fn eval_variable(&mut self, name: &Name) {
        match self.context.get_variable(name) {
            Some(var) => self.store_value(var),
            None => self.error(EvalErrorKind::UndefinedVar(name.clone())),
        };
    }

    fn eval_parameter(&mut self, idx: usize) {
        self.args.get(idx).copied().map(|v| self.store_value(v));
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
    fn visit(&mut self, algebra: &Function) {
        self.clear_value();
        self.visit_tree(&algebra.tree);
    }

    fn visit_value(&mut self, node: &Value) {
        match node {
            Value::Num(n) => self.store_value(*n),
            Value::Var(v) => self.eval_variable(v),
            Value::Param(idx) => self.eval_parameter(*idx)
        };
    }

    fn visit_unary(&mut self, node: &Unary) {
        self.visit_tree(&node.tree);
        let inner_value = self.take_value();

        if let Some(val) = inner_value {
            match node.op {
                AlgUnaryOp::Neg => self.store_value(- val)
            }
        }
    }

    fn visit_binary(&mut self, node: &Binary) {
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

    fn visit_fn_call(&mut self, node: &FnCall) {
        let args: Option<Vec<f64>> = node.args.iter()
            .map(|node| {
                self.visit_tree(&node);
                self.take_value()
            })
            .collect();

        if let Some(arg_vec) = args {
            let result = self.context.call_func(&node.func, &arg_vec);

            if let Ok(value) = result {
                self.store_value(value);
            }
        }
    }

    fn visit_tree(&mut self, node: &AlgExpr) {
        match node {
            AlgExpr::Value(n) => self.visit_value(n),
            AlgExpr::Unary(n) => self.visit_unary(n),
            AlgExpr::Binary(n) => self.visit_binary(n),
            AlgExpr::FnCall(n) => self.visit_fn_call(n),
        }
    }
}
