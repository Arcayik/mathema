use crate::{
    parsing::{
        ast::{AstExpr, AstValue, BinOp, UnaryOp},
    },
    context::{call_variable, Context, FuncError, VarError},
    function::call_function,
    symbol::Symbol,
    value::MathemaValue,
    float,
};

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

pub fn expr_to_algebra(ast: &AstExpr) -> AlgExpr {
    fn recurse(ast: &AstExpr) -> AlgExpr {
        match ast {
            AstExpr::Value(v) => {
                let alg = match v {
                    AstValue::Literal(l) => Value::Num(float!(l.num)),
                    AstValue::Ident(id) => Value::Var(id.symbol)
                }.into();

                alg
            },
            AstExpr::Unary(u) => {
                let expr_node = recurse(&u.expr);
                let alg = Unary {
                    op: u.op.clone().into(),
                    expr: Box::new(expr_node)
                }.into();

                alg
            },
            AstExpr::Binary(b) => {
                let left = recurse(&b.lhs);
                let right = recurse(&b.rhs);

                let alg = Binary {
                    left: Box::new(left),
                    op: b.op.clone().into(),
                    right: Box::new(right),
                }.into();

                alg
            },
            AstExpr::Group(g) => {
                let alg = recurse(&g.expr);
                alg
            },
            AstExpr::FnCall(f) => {
                let args: Vec<AlgExpr> = f.inputs.iter()
                    .map(recurse)
                    .collect();

                let alg = FnCall {
                    args,
                    name: f.fn_name.symbol
                }.into();

                alg
            }
        }
    }

    recurse(ast)
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
    BadFnCall(FuncError),
    BadVar(VarError),
}

#[derive(Debug)]
pub struct EvalError {
    pub kind: EvalErrorKind,
}

// impl std::fmt::Display for EvalError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let str = match &self.kind {
//             EvalErrorKind::BadFnCall(e) => "Bad function call: TODO!".to_string(),
//             EvalErrorKind::BadVar(ve) => "Bad variable: TODO!".to_string()
//         };
//         write!(f, "{}", str)
//     }
// }

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
                    match call_variable(context, *var) {
                        Ok(ans) => Some(ans),
                        Err(e) => {
                            let kind = EvalErrorKind::BadVar(e);
                            errors.push(EvalError { kind });
                            None
                        }
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
                        let kind = EvalErrorKind::BadFnCall(e);
                        let error = EvalError { kind };
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

