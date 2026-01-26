use crate::{
    arena::Arena,
    context::Context,
    parsing::ast::{AstBinary, AstExpr, AstFnCall, AstGroup, AstUnary, AstValue, BinOp, UnaryOp},
    symbol::Symbol
};

#[derive(Debug, Clone)]
pub struct AlgebraTree {
    root: NodeId,
}

impl AlgebraTree {
    pub fn visit<V: TreeVisitor>(&self, ctxt: &Context, visitor: V) -> V::Output {
        visitor.visit_tree(ctxt, self.root)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub(crate) usize);

impl From<usize> for NodeId {
    fn from(value: usize) -> Self {
        NodeId(value)
    }
}

pub fn expr_to_algebra(ast: &AstExpr, arena: &mut Arena) -> AlgebraTree {
    let root = parse_ast(ast, arena);
    AlgebraTree { root }
}

fn parse_ast(ast: &AstExpr, arena: &mut Arena) -> NodeId {
    match ast {
        AstExpr::Value(v) => parse_value(v, arena),
        AstExpr::Unary(u) => parse_unary(u, arena),
        AstExpr::Binary(b) => parse_binary(b, arena),
        AstExpr::Group(g) => parse_group(g, arena),
        AstExpr::FnCall(f) => parse_fn_call(f, arena),
    }
}

fn parse_value(value: &AstValue, arena: &mut Arena) -> NodeId {
    let node = match value {
        AstValue::Literal(lit) => AlgExpr::Literal(lit.num),
        AstValue::Ident(ident) => AlgExpr::Ident(ident.symbol)
    };
    arena.intern(node)
}

fn parse_unary(unary: &AstUnary, arena: &mut Arena) -> NodeId {
    let alg = AlgExpr::Unary {
        op: unary.op.clone().into(),
        inner: parse_ast(&unary.expr, arena)
    };
    arena.intern(alg)
}

fn parse_binary(binary: &AstBinary, arena: &mut Arena) -> NodeId {
    let alg = AlgExpr::Binary {
        left: parse_ast(&binary.lhs, arena),
        op: binary.op.clone().into(),
        right: parse_ast(&binary.rhs, arena)
    };
    arena.intern(alg)
}

fn parse_group(group: &AstGroup, arena: &mut Arena) -> NodeId {
    parse_ast(&group.expr, arena)
}

fn parse_fn_call(fn_call: &AstFnCall, arena: &mut Arena) -> NodeId {
    let args = fn_call.inputs
        .iter()
        .map(|ast| parse_ast(ast, arena))
        .collect();

    let alg = AlgExpr::FnCall {
        name: fn_call.fn_name.symbol,
        args
    };
    arena.intern(alg)
}

pub trait TreeVisitor {
    type Output;
    fn visit_tree(&self, ctxt: &Context, id: NodeId) -> Self::Output;
}

#[derive(Clone, Debug, PartialEq)]
pub enum AlgExpr {
    Literal(f64),
    Ident(Symbol),
    Binary { left: NodeId, op: AlgBinOp, right: NodeId },
    Unary { op: AlgUnaryOp, inner: NodeId },
    FnCall { name: Symbol, args: Vec<NodeId> },
}

impl From<f64> for AlgExpr {
    fn from(value: f64) -> Self {
        AlgExpr::Literal(value)
    }
}

impl From<Symbol> for AlgExpr {
    fn from(id: Symbol) -> Self {
        AlgExpr::Ident(id)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

#[cfg(test)]
mod tests {
    use crate::{algebra::ast::expr_to_algebra, arena::Arena, parsing::{ast::{AstExpr, AstValue}, token::Ident}, symbol::Symbol};

    #[test]
    fn single_expr() {
        let mut arena = Arena::default();
        let ast = AstExpr::Value(AstValue::Ident(Ident { symbol: Symbol::intern("x"), span: (0,1).into() }));
        let tree = expr_to_algebra(&ast, &mut arena);
        dbg!(tree);
    }
}
