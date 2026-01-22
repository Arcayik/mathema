use crate::{
    float,
    parsing::ast::{AstBinary, AstExpr, AstFnCall, AstGroup, AstUnary, AstValue, BinOp, UnaryOp},
    symbol::Symbol,
    value::MathemaValue
};

#[derive(Debug)]
pub struct AlgebraTree {
    nodes: Vec<AlgExpr>
}

pub type NodeIdx = usize;

impl AlgebraTree {
    pub fn new() -> Self {
        AlgebraTree {
            nodes: Vec::new()
        }
    }

    pub fn add_node(&mut self, node: AlgExpr) -> NodeIdx {
        let idx = self.nodes.len();
        self.nodes.push(node);
        idx
    }

    fn get_node(&self, idx: NodeIdx) -> Option<&AlgExpr> {
        self.nodes.get(idx)
    }

    fn get_node_mut(&mut self, idx: NodeIdx) -> Option<&mut AlgExpr> {
        self.nodes.get_mut(idx)
    }

    fn num_nodes(&self) -> usize {
        self.nodes.len()
    }
}

pub fn expr_to_algebra(ast: &AstExpr) -> AlgebraTree {
    let mut tree = AlgebraTree::new();
    parse_ast(ast, &mut tree);
    tree
}

pub fn parse_ast(ast: &AstExpr, tree: &mut AlgebraTree) -> NodeIdx {
    match ast {
        AstExpr::Value(v) => parse_value(v, tree),
        AstExpr::Unary(u) => parse_unary(u, tree),
        AstExpr::Binary(b) => parse_binary(b, tree),
        AstExpr::Group(g) => parse_group(g, tree),
        AstExpr::FnCall(f) => parse_fn_call(f, tree),
    }
}

pub fn parse_value(value: &AstValue, tree: &mut AlgebraTree) -> NodeIdx {
    tree.add_node(AlgExpr::Value(value.into()))
}

pub fn parse_unary(unary: &AstUnary, tree: &mut AlgebraTree) -> NodeIdx {
    let alg = AlgExpr::Unary {
        op: unary.op.clone().into(),
        inner: parse_ast(&unary.expr, tree)
    };
    tree.add_node(alg)
}

pub fn parse_binary(binary: &AstBinary, tree: &mut AlgebraTree) -> NodeIdx {
    let alg = AlgExpr::Binary {
        left: parse_ast(&binary.lhs, tree),
        op: binary.op.clone().into(),
        right: parse_ast(&binary.rhs, tree)
    };
    tree.add_node(alg)
}

pub fn parse_group(group: &AstGroup, tree: &mut AlgebraTree) -> NodeIdx {
    parse_ast(&group.expr, tree)
}

pub fn parse_fn_call(fn_call: &AstFnCall, tree: &mut AlgebraTree) -> NodeIdx {
    let args = fn_call.inputs
        .iter()
        .map(|ast| parse_ast(ast, tree))
        .collect();

    let alg = AlgExpr::FnCall {
        name: fn_call.fn_name.symbol,
        args
    };
    tree.add_node(alg)
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

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Num(MathemaValue),
    Var(Symbol),
}

impl From<&AstValue> for Value {
    fn from(value: &AstValue) -> Self {
        match value {
            AstValue::Ident(i) => Value::Var(i.symbol),
            AstValue::Literal(l) => Value::Num(float!(l.num))
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
    }
}
