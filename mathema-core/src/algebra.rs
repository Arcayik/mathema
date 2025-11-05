use std::rc::Rc;

use crate::{
    context::{CallError, Context},
    error::Diagnostic,
    function::{eval_function, FnArgs, Function},
    intrinsics,
    parsing::{
        ast::{BinOp, Expr, ExprBinary, ExprFnCall, ExprGroup, ExprUnary, ExprValue, ExprVisit, Stmt, UnaryOp},
        token::Span
    },
    symbol::Symbol
};

pub enum AlgStmt {
    Expr(AlgExpr),
    VarDecl(Symbol, AlgExpr),
    FnDecl(Symbol, Function),
}

impl std::fmt::Display for AlgStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AlgStmt::Expr(alg) => {
                let (body, _) = algebra_to_string(alg, &[]);
                write!(f, "{}", body)
            },
            AlgStmt::VarDecl(var, alg) => {
                let (body, _) = algebra_to_string(alg, &[]);
                write!(f, "{} = {}", var.as_str(), body)
            },
            AlgStmt::FnDecl(name, func) => {
                let (body, _) = algebra_to_string(&func.body, &[]);
                let args = func.args.get_args()
                    .iter()
                    .map(|a| a.as_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({}) = {}", name, args, body)
            }
        }
    }
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

impl std::fmt::Display for AlgBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Exp => write!(f, "^"),
        }
    }
}

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

impl std::fmt::Display for AlgUnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Neg => write!(f, "-")
        }
    }
}

impl From<UnaryOp> for AlgUnaryOp {
    fn from(value: UnaryOp) -> Self {
        match value {
            UnaryOp::Neg(_) => AlgUnaryOp::Neg,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub(crate) func: Symbol,
    pub(crate) args: Vec<AlgExpr>,
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
    pub(crate) source: Rc<str>,
    pub(crate) span: Span
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
        self.to_string()
    }

    fn source_code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(self.source.clone()))
    }

    fn spans(&self) -> Option<Box<dyn Iterator<Item = Span>>> {
        Some(Box::new(std::iter::once(self.span)))
    }
}

pub fn eval_algebra(context: &Context, algebra: &AlgExpr) -> Result<f64, Vec<EvalError>> {
    fn recurse(
        context: &Context,
        root: &AlgExpr,
        algebra: &AlgExpr,
        errors: &mut Vec<EvalError>
    ) -> Option<f64> {
        match algebra {
            AlgExpr::Value(val) => match val {
                Value::Num(num) => Some(*num),
                Value::Var(var) => {
                    if let Some(alg) = context.get_variable(*var) {
                        recurse(context, root, alg, errors)
                    } else if let Some(c) = intrinsics::CONSTANTS.get(var) {
                        Some(*c)
                    } else {
                        let (source, span) = algebra_to_string(root, &[algebra]);
                        let error = EvalError {
                            kind: EvalErrorKind::UndefinedVar(*var),
                            source: source.into(),
                            span: span[0]
                        };
                        errors.push(error);
                        None
                    }
                }
            },
            AlgExpr::Unary(un) => {
                if let Some(val) = recurse(context, root, algebra, errors) {
                    match un.op {
                        AlgUnaryOp::Neg => Some(- val)
                    }
                } else {
                    None
                }
            },
            AlgExpr::Binary(bin) => {
                let left = recurse(context, root, &bin.left, errors);
                let right = recurse(context, root, &bin.right, errors);

                if let (Some(l), Some(r)) = (left, right) {
                    match bin.op {
                        AlgBinOp::Add => Some(l + r),
                        AlgBinOp::Sub => Some(l - r),
                        AlgBinOp::Mul => Some(l * r),
                        AlgBinOp::Div => Some(l / r),
                        AlgBinOp::Exp => Some(l.powf(r)),
                    }
                } else {
                    None
                }
            },
            AlgExpr::FnCall(fc) => {
                let args: Vec<f64> = fc.args
                    .iter()
                    .map(|a| recurse(context, root, a, errors))
                    .collect::<Option<_>>()?;

                if let Some(func) = intrinsics::CONST_FNS.get(&fc.func) {
                    return Some((func)(&args))
                };

                if let Some(func) = context.get_function(fc.func) {
                    match eval_function(context, func, &args) {
                        Ok(ans) => Some(ans),
                        Err(e) => {
                            let (source, span) = algebra_to_string(root, &[algebra]);
                            let error = EvalError {
                                kind: EvalErrorKind::BadFnCall(e),
                                source: source.into(),
                                span: span[0]
                            };
                            errors.push(error);
                            None
                        }
                    }
                } else {
                    let (source, span) = algebra_to_string(root, &[algebra]);
                    let error = EvalError {
                        kind: EvalErrorKind::UndefinedFunc(fc.func),
                        source: source.into(),
                        span: span[0]
                    };
                    errors.push(error);
                    None
                }
            }
        }
    }

    let mut errors = Vec::new();
    let result = recurse(context, algebra, algebra, &mut errors);
    if let Some(ans) = result {
        Ok(ans)
    } else {
        Err(errors)
    }
}

pub fn algebra_to_string(algebra: &AlgExpr, take_span: &[&AlgExpr]) -> (String, Vec<Span>) {
    // TODO: parenthesis when precedence mismatches!
    /*
     *    +
     *   1 *  => 1+2*3
     *    2 3
     *
     *   *  
     *  + 3 => 1+2*3 => (1+2)*3
     * 1 2 
     *
     * if subtree has lower precedence!
     */

    fn recurse(
        algebra: &AlgExpr,
        take_span: &[&AlgExpr],
        string: &mut String,
        spans: &mut Vec<Span>
    ) {
        let old_end = string.len();
        match algebra {
            AlgExpr::Value(val) => match val {
                Value::Num(num) => string.push_str(&num.to_string()),
                Value::Var(var) => string.push_str(var.as_str()),
            },
            AlgExpr::Unary(un) => {
                string.push_str(&un.op.to_string());
                let mut sub_str = String::new();
                recurse(&un.expr, take_span, &mut sub_str, spans);
                if !matches!(*un.expr, AlgExpr::Value(_)) {
                    string.push_str(&format!("({})", sub_str));
                } else {
                    string.push_str(&sub_str)
                }
            },
            AlgExpr::Binary(bin) => {
                recurse(&bin.left, take_span, string, spans);
                string.push_str(&format!(" {} ", bin.op));
                recurse(&bin.right, take_span, string, spans);
            },
            AlgExpr::FnCall(fc) => {
                string.push_str(fc.func.as_str());
                string.push('(');

                let mut arg_strings: Vec<String> = Vec::new();
                for arg in &fc.args {
                    let mut arg_str = String::new();
                    recurse(arg, take_span, &mut arg_str, spans);
                    arg_strings.push(arg_str);
                }
                string.push_str(&arg_strings.join(", "));
                string.push(')');
            }
        };

        if take_span.iter().any(|t| core::ptr::eq(algebra, *t)) {
            let span = Span {
                start: old_end,
                end: string.len(),
            };
            spans.push(span);
        }
    }

    let mut string = String::new();
    let mut spans = Vec::new();

    recurse(algebra, take_span, &mut string, &mut spans);

    (string, spans)
}
