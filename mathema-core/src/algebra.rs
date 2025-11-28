use crate::{
    context::{Context, FuncError, VarError},
    float,
    function::{call_function, FnArgs, Function},
    intrinsics, parsing::{
        ast::{AstBinary, AstExpr, AstFnCall, AstGroup, AstStmt, AstUnary, AstValue, AstVisit, BinOp, UnaryOp},
        token::Span
    },
    snippet::create_algebra_snippet,
    symbol::Symbol,
    value::MathemaValue
};

pub enum AlgStmt {
    Expr(AlgExpr),
    VarDecl(Symbol, AlgExpr),
    FnDecl(Function),
}

impl std::fmt::Display for AlgStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AlgStmt::Expr(alg) => {
                let snip = create_algebra_snippet(alg);
                let body = snip.source();
                write!(f, "{}", body)
            },
            AlgStmt::VarDecl(var, alg) => {
                let snip = create_algebra_snippet(alg);
                let body = snip.source();
                write!(f, "{} = {}", var.as_str(), body)
            },
            AlgStmt::FnDecl(func) => {
                let snip = create_algebra_snippet(&func.body);
                let body = snip.source();
                let args = func.args.get_args()
                    .iter()
                    .map(|a| a.as_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({}) = {}", func.name, args, body)
            }
        }
    }
}

impl AlgStmt {
    pub fn from_expr_stmt(stmt: &AstStmt) -> Self {
        match stmt {
            AstStmt::Expr(expr) => {
                let alg = ExprToAlgebra.visit_expr(expr);
                AlgStmt::Expr(alg)
            },
            AstStmt::VarDecl(decl) => {
                let name = decl.var_name.symbol;
                let alg = ExprToAlgebra.visit_expr(&decl.expr);
                AlgStmt::VarDecl(name, alg)
            },
            AstStmt::FnDecl(decl) => {
                let name = decl.sig.fn_name.symbol;
                let alg = ExprToAlgebra.visit_expr(&decl.expr);

                let arg_vec: Vec<Symbol> = decl.sig.inputs
                    .iter()
                    .map(|i| i.symbol)
                    .collect();
                let args = FnArgs::from_vec(arg_vec);

                let func = Function::new(name, alg, args);
                AlgStmt::FnDecl(func)
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

impl From<MathemaValue> for AlgExpr {
    fn from(value: MathemaValue) -> Self {
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
    Num(MathemaValue),
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
    pub(crate) name: Symbol,
    pub(crate) args: Vec<AlgExpr>,
}

// TODO: make it populate the hash table or something with original span, which changes if
// something new happens
pub struct ExprToAlgebra;

impl AstVisit for ExprToAlgebra {
    type Result = AlgExpr;

    fn visit_expr(&mut self, expr: &AstExpr) -> Self::Result {
        match expr {
            AstExpr::Value(e) => self.visit_value(e),
            AstExpr::Unary(e) => self.visit_unary(e),
            AstExpr::Binary(e) => self.visit_binary(e),
            AstExpr::FnCall(e) => self.visit_fn_call(e),
            AstExpr::Group(e) => self.visit_group(e),
        }
    }

    fn visit_value(&mut self, value: &AstValue) -> Self::Result {
        match value {
            AstValue::Literal(l) => Value::Num(float!(l.num)).into(),
            AstValue::Ident(id) => Value::Var(id.symbol).into()
        }
    }

    fn visit_binary(&mut self, binary: &AstBinary) -> Self::Result {
        let left = self.visit_expr(&binary.lhs);
        let right = self.visit_expr(&binary.rhs);

        Binary {
            left: Box::new(left),
            op: binary.op.clone().into(),
            right: Box::new(right),
        }.into()
    }

    fn visit_unary(&mut self, unary: &AstUnary) -> Self::Result {
        let expr_node = self.visit_expr(&unary.expr);

        Unary {
            op: unary.op.clone().into(),
            expr: Box::new(expr_node)
        }.into()
    }

    fn visit_group(&mut self, group: &AstGroup) -> Self::Result {
        self.visit_expr(&group.expr)
    }

    fn visit_fn_call(&mut self, fn_call: &AstFnCall) -> Self::Result {
        let args: Vec<AlgExpr> = fn_call.inputs.iter()
            .map(|expr| self.visit_expr(expr))
            .collect();

        FnCall {
            args,
            name: fn_call.fn_name.symbol
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
        let func = fn_call.name;
        FnCall { args, name: func }
    }
}

#[derive(Debug)]
pub enum EvalErrorKind {
    UndefinedVar(Symbol),
    BadFnCall(FuncError),
    BadVar(VarError),
}

#[derive(Debug)]
pub struct EvalError {
    pub kind: EvalErrorKind,
    pub source: String,
    pub span: Span
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match &self.kind {
            EvalErrorKind::UndefinedVar(s) => format!("undefined var: {s}"),
            EvalErrorKind::BadFnCall(e) => "Bad function call: TODO!".to_string(),
            EvalErrorKind::BadVar(ve) => "Bad variable: TODO!".to_string()
        };
        write!(f, "{}", str)
    }
}

pub fn eval_algebra(context: &Context, algebra: &AlgExpr) -> Result<MathemaValue, Vec<EvalError>> {
    fn recurse(
        context: &Context,
        root: &AlgExpr,
        algebra: &AlgExpr,
        errors: &mut Vec<EvalError>
    ) -> Option<MathemaValue> {
        match algebra {
            AlgExpr::Value(val) => match val {
                Value::Num(num) => Some(num.clone()),
                Value::Var(var) => {
                    if let Some(alg) = context.get_variable(*var) {
                        recurse(context, root, alg, errors)
                    } else if let Some(c) = intrinsics::CONSTANTS.get(var.as_str()) {
                        Some(c.clone())
                    } else {
                        assert!(std::ptr::eq(root, algebra));
                        let snippet = create_algebra_snippet(root);
                        let source = snippet.source();
                        let span = snippet.get_span(algebra).unwrap();
                        let error = EvalError {
                            kind: EvalErrorKind::UndefinedVar(*var),
                            source: source.into(),
                            span,
                        };
                        errors.push(error);
                        None
                    }
                }
            },
            AlgExpr::Unary(un) => {
                if let Some(val) = recurse(context, root, &un.expr, errors) {
                    match un.op {
                        AlgUnaryOp::Neg => Some(val.neg())
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
                        AlgBinOp::Add => Some(l.add(&r)),
                        AlgBinOp::Sub => Some(l.sub(&r)),
                        AlgBinOp::Mul => Some(l.mul(&r)),
                        AlgBinOp::Div => Some(l.div(&r)),
                        AlgBinOp::Exp => Some(l.pow(&r)),
                    }
                } else {
                    None
                }
            },
            AlgExpr::FnCall(fc) => {
                let args: Vec<MathemaValue> = fc.args
                    .iter()
                    .map(|a| recurse(context, root, a, errors))
                    .collect::<Option<_>>()?;

                match call_function(context, fc.name, &args) {
                    Ok(ans) => Some(ans),
                    Err(e) => {
                        let snippet = create_algebra_snippet(root);
                        let source = snippet.source();
                        let span = snippet.get_span(algebra).unwrap();
                        let error = EvalError {
                            kind: EvalErrorKind::BadFnCall(e),
                            source: source.into(),
                            span,
                        };
                        errors.push(error);
                        None
                    }
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

