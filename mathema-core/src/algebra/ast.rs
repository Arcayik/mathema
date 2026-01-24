use crate::{
    parsing::ast::{AstBinary, AstExpr, AstFnCall, AstGroup, AstUnary, AstValue, BinOp, UnaryOp},
    symbol::Symbol
};

#[derive(Debug)]
pub struct AlgebraTree {
    root: NodeIdx,
    nodes: Vec<AlgExpr>
}

pub type NodeIdx = usize;

impl AlgebraTree {
    /// Create an empty AlgebraTree 
    fn empty() -> Self {
        // Root not actually zero, just a placeholder
        AlgebraTree { root: 0, nodes: Vec::new() }
    }

    /// Create a new, non-empty AlgebraTree 
    pub fn new(node: AlgExpr) -> Self {
        AlgebraTree {
            root: 0,
            nodes: vec![node]
        }
    }

    /// Push new node to the tree
    fn push_node(&mut self, node: AlgExpr) -> NodeIdx {
        let idx = self.nodes.len();
        self.nodes.push(node);
        idx
    }

    pub fn accept<V: TreeVisitor>(&self, visitor: V) -> V::Output {
        visitor.visit_tree(&self.nodes, self.root)
    }
}

pub fn expr_to_algebra(ast: &AstExpr) -> AlgebraTree {
    let mut tree = AlgebraTree::empty();
    let root = parse_ast(ast, &mut tree);
    tree.root = root;
    tree
}

fn parse_ast(ast: &AstExpr, tree: &mut AlgebraTree) -> NodeIdx {
    match ast {
        AstExpr::Value(v) => parse_value(v, tree),
        AstExpr::Unary(u) => parse_unary(u, tree),
        AstExpr::Binary(b) => parse_binary(b, tree),
        AstExpr::Group(g) => parse_group(g, tree),
        AstExpr::FnCall(f) => parse_fn_call(f, tree),
    }
}

fn parse_value(value: &AstValue, tree: &mut AlgebraTree) -> NodeIdx {
    tree.push_node(AlgExpr::Value(value.into()))
}

fn parse_unary(unary: &AstUnary, tree: &mut AlgebraTree) -> NodeIdx {
    let alg = AlgExpr::Unary {
        op: unary.op.clone().into(),
        inner: parse_ast(&unary.expr, tree)
    };
    tree.push_node(alg)
}

fn parse_binary(binary: &AstBinary, tree: &mut AlgebraTree) -> NodeIdx {
    let alg = AlgExpr::Binary {
        left: parse_ast(&binary.lhs, tree),
        op: binary.op.clone().into(),
        right: parse_ast(&binary.rhs, tree)
    };
    tree.push_node(alg)
}

fn parse_group(group: &AstGroup, tree: &mut AlgebraTree) -> NodeIdx {
    parse_ast(&group.expr, tree)
}

fn parse_fn_call(fn_call: &AstFnCall, tree: &mut AlgebraTree) -> NodeIdx {
    let args = fn_call.inputs
        .iter()
        .map(|ast| parse_ast(ast, tree))
        .collect();

    let alg = AlgExpr::FnCall {
        name: fn_call.fn_name.symbol,
        args
    };
    tree.push_node(alg)
}

pub trait TreeVisitor {
    type Output;
    fn visit_tree(&self, nodes: &[AlgExpr], start_idx: NodeIdx) -> Self::Output;
}

#[derive(Clone, Debug, PartialEq)]
pub enum AlgExpr {
    Value(Value),
    Binary { left: NodeIdx, op: AlgBinOp, right: NodeIdx },
    Unary { op: AlgUnaryOp, inner: NodeIdx},
    FnCall { name: Symbol, args: Vec<NodeIdx> },
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

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Var(Symbol),
}

impl From<&AstValue> for Value {
    fn from(value: &AstValue) -> Self {
        match value {
            AstValue::Ident(i) => Value::Var(i.symbol),
            AstValue::Literal(l) => Value::Num(l.num)
        }
    }
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
    use crate::{algebra::ast::expr_to_algebra, parsing::{ast::{AstExpr, AstValue}, token::Ident}, symbol::Symbol};

    #[test]
    fn single_expr() {
        let ast = AstExpr::Value(AstValue::Ident(Ident { symbol: Symbol::intern("x"), span: (0,1).into() }));
        let tree = expr_to_algebra(&ast);
        dbg!(tree);
    }
}
