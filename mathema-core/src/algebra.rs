use crate::{
    context::{CallError, Context}, error::Diagnostic, function::{FnArgs, Function}, parsing::ast::{BinOp, Expr, ExprBinary, ExprFnCall, ExprGroup, ExprUnary, ExprValue, ExprVisit, Stmt, UnaryOp}, symbol::Symbol
};

pub enum AlgStmt {
    Expr(AlgExpr),
    VarDecl(Symbol, AlgExpr),
    FnDecl(Symbol, Function),
}

impl AlgStmt {
    pub fn from_expr_stmt(stmt: &Stmt) -> Self {
        match stmt {
            Stmt::Expr(expr) => {
                let alg = ExprToAlgebra.visit_expr(expr);
                AlgStmt::Expr(alg)
            },
            Stmt::VarDecl(decl) => {
                let name = decl.var_name.symbol;
                let alg = ExprToAlgebra.visit_expr(&decl.expr);
                AlgStmt::VarDecl(name, alg)
            },
            Stmt::FnDecl(decl) => {
                let name = decl.sig.fn_name.symbol;
                let alg = ExprToAlgebra.visit_expr(&decl.body);

                let arg_vec: Vec<Symbol> = decl.sig.inputs
                    .iter()
                    .map(|i| i.symbol)
                    .collect();
                let args = FnArgs::from_vec(arg_vec);

                let func = Function::new(alg, args);
                AlgStmt::FnDecl(name, func)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl From<Symbol> for AlgExpr {
    fn from(value: Symbol) -> Self {
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

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Var(Symbol),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binary {
    pub(crate) left: Box<AlgExpr>,
    pub(crate) op: AlgBinOp,
    pub(crate) right: Box<AlgExpr>,
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub struct Unary {
    pub(crate) op: AlgUnaryOp,
    pub(crate) expr: Box<AlgExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AlgUnaryOp { Neg }

impl From<UnaryOp> for AlgUnaryOp {
    fn from(value: UnaryOp) -> Self {
        match value {
            UnaryOp::Neg(_) => AlgUnaryOp::Neg,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub(crate) args: Vec<AlgExpr>,
    pub(crate) func: Symbol,
}

pub struct ExprToAlgebra;

impl ExprVisit for ExprToAlgebra {
    type Result = AlgExpr;

    fn visit_expr(&mut self, node: &Expr) -> Self::Result {
        match node {
            Expr::Value(e) => self.visit_value(e),
            Expr::Unary(e) => self.visit_unary(e),
            Expr::Binary(e) => self.visit_binary(e),
            Expr::FnCall(e) => self.visit_fn_call(e),
            Expr::Group(e) => self.visit_group(e),
        }
    }

    fn visit_value(&mut self, node: &ExprValue) -> Self::Result {
        match node {
            ExprValue::Literal(l) => Value::Num(l.num).into(),
            ExprValue::Ident(id) => Value::Var(id.symbol).into()
        }
    }

    fn visit_binary(&mut self, node: &ExprBinary) -> Self::Result {
        let left = self.visit_expr(&node.lhs);
        let right = self.visit_expr(&node.rhs);

        Binary {
            left: Box::new(left),
            op: node.op.clone().into(),
            right: Box::new(right),
        }.into()
    }

    fn visit_unary(&mut self, node: &ExprUnary) -> Self::Result {
        let expr_node = self.visit_expr(&node.expr);

        Unary {
            op: node.op.clone().into(),
            expr: Box::new(expr_node)
        }.into()
    }

    fn visit_group(&mut self, node: &ExprGroup) -> Self::Result {
        self.visit_expr(&node.expr)
    }

    fn visit_fn_call(&mut self, node: &ExprFnCall) -> Self::Result {
        let args: Vec<AlgExpr> = node.inputs.iter()
            .map(|expr| self.visit_expr(expr))
            .collect();

        FnCall {
            args,
            func: node.fn_name.symbol
        }.into()
    }
}

pub trait AlgebraVisit {
    type Result;

    fn visit_expr(&mut self, node: &AlgExpr) -> Self::Result;
    fn visit_value(&mut self, node: &Value) -> Self::Result;
    fn visit_binary(&mut self, node: &Binary) -> Self::Result;
    fn visit_unary(&mut self, node: &Unary) -> Self::Result;
    fn visit_fn_call(&mut self, node: &FnCall) -> Self::Result;
}

pub mod visit {
    use crate::algebra::{AlgExpr, AlgebraVisit, Unary};

    pub fn walk_expr<V: AlgebraVisit + ?Sized>(visitor: &mut V, expr: &AlgExpr) -> V::Result {
        match expr {
            AlgExpr::Value(e) => visitor.visit_value(e),
            AlgExpr::Unary(e) => visitor.visit_unary(e),
            AlgExpr::Binary(e) => visitor.visit_binary(e),
            AlgExpr::FnCall(e) => visitor.visit_fn_call(e),
        }
    }

    pub fn walk_unary<V: AlgebraVisit + ?Sized>(visitor: &mut V, unary: &Unary) -> V::Result {
        visitor.visit_expr(&unary.expr)
    }
}

pub trait AlgebraFold {
    fn fold_expr(&mut self, expr: AlgExpr) -> AlgExpr {
        match expr {
            AlgExpr::Value(v) => self.fold_value(v).into(),
            AlgExpr::Unary(u) => self.fold_unary(u).into(),
            AlgExpr::Binary(b) => self.fold_binary(b).into(),
            AlgExpr::FnCall(f) => self.fold_fn_call(f).into()
        }
    }

    fn fold_value(&mut self, value: Value) -> Value {
        value
    }

    fn fold_binary(&mut self, binary: Binary) -> Binary {
        let left = Box::new(self.fold_expr(*binary.left));
        let op = binary.op;
        let right = Box::new(self.fold_expr(*binary.right));
        Binary { left, op, right, }
    }

    fn fold_unary(&mut self, unary: Unary) -> Unary {
        let op = unary.op;
        let expr = unary.expr;
        Unary { op, expr }
    }

    fn fold_fn_call(&mut self, fn_call: FnCall) -> FnCall {
        let args = fn_call.args
            .into_iter()
            .map(|a| self.fold_expr(a))
            .collect();
        let func = fn_call.func;
        FnCall { args, func }
    }
}

#[derive(Debug)]
pub enum EvalErrorKind {
    UndefinedVar(Symbol),
    UndefinedFunc(Symbol),
    BadFnCall(CallError)
}

#[derive(Debug)]
pub struct EvalError {
    pub kind: EvalErrorKind,
    // pub(crate) source: Rc<str>,
    // pub(crate) span: Span
}

impl EvalError {
    pub fn new(kind: EvalErrorKind) -> Self {
        EvalError { kind }
    }
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match &self.kind {
            EvalErrorKind::UndefinedVar(s) => format!("undefined var: {s}"),
            EvalErrorKind::UndefinedFunc(s) => format!("undefined func: {s}"),
            EvalErrorKind::BadFnCall(e) => e.to_string()
        };
        write!(f, "{}", str)
    }
}

impl std::error::Error for EvalError {}

impl Diagnostic for EvalError {
    fn message(&self) -> String {
        todo!()
    }

    fn source_code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        todo!()
    }

    fn spans(&self) -> Option<Box<dyn Iterator<Item = crate::parsing::token::Span>>> {
        todo!()
    }
}

pub struct Evaluator<'a> {
    context: &'a Context,
    errors: Vec<EvalError>
}

impl<'a> Evaluator<'a> {
    pub fn new(context: &'a Context) -> Self {
        Evaluator {
            context,
            errors: Vec::new()
        }
    }

    pub fn take_errors(&mut self) -> Vec<EvalError> {
        std::mem::take(&mut self.errors)
    }

    fn store_error(&mut self, kind: EvalErrorKind) {
        let err = EvalError::new(kind);
        self.errors.push(err);
    }

    fn eval_variable(&mut self, name: Symbol) -> Option<f64> {
        let var = self.context.get_variable(name);
        if let Some(expr) = var {
            let mut eval = Evaluator::new(self.context);
            eval.visit_expr(expr)
        } else {
            self.store_error(EvalErrorKind::UndefinedVar(name));
            None
        }
    }
}

impl<'a> AlgebraVisit for Evaluator<'a> {
    type Result = Option<f64>;

    fn visit_expr(&mut self, node: &AlgExpr) -> Self::Result {
        visit::walk_expr(self, node)
    }

    fn visit_value(&mut self, node: &Value) -> Self::Result {
        match node {
            Value::Num(n) => Some(*n),
            Value::Var(v) => self.eval_variable(*v),
        }
    }

    fn visit_unary(&mut self, node: &Unary) -> Self::Result {
        let value = self.visit_expr(&node.expr)?;

        match node.op {
            AlgUnaryOp::Neg => Some(- value)
        }
    }

    fn visit_binary(&mut self, node: &Binary) -> Self::Result {
        let left = self.visit_expr(&node.left)?;
        let right = self.visit_expr(&node.right)?;

        let value = match node.op {
            AlgBinOp::Add => left + right,
            AlgBinOp::Sub => left - right,
            AlgBinOp::Mul => left * right,
            AlgBinOp::Div => left / right,
            AlgBinOp::Exp => left.powf(right),
        };

        Some(value)
    }

    fn visit_fn_call(&mut self, node: &FnCall) -> Self::Result {
        let args: Vec<f64> = node.args.iter()
            .map(|node| self.visit_expr(node))
            .collect::<Option<_>>()?;

        self.context.call_func(node.func, &args)
            .map_err(|e| self.store_error(EvalErrorKind::BadFnCall(e)))
            .ok()
    }
}
