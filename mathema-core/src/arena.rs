use std::collections::HashMap;

use crate::{
    algebra::ast::{AlgBinOp, AlgExpr, AlgUnaryOp, NodeId},
    parsing::ast::Precedence,
    symbol::Symbol
};

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct F64Key(u64);

impl From<f64> for F64Key {
    fn from(mut value: f64) -> Self {
        if value == 0.0 {
            value = 0.0;
        }
        if value.is_nan() {
            return F64Key(f64::NAN.to_bits())
        };
        F64Key(value.to_bits())
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum AlgExprKey {
    Literal(F64Key),
    Ident(Symbol),
    Binary { left: NodeId, op: AlgBinOp, right: NodeId },
    Unary { op: AlgUnaryOp, inner: NodeId },
    FnCall { name: Symbol, args: Vec<NodeId> }
}

impl From<AlgExpr> for AlgExprKey {
    fn from(value: AlgExpr) -> Self {
        match value {
            AlgExpr::Literal(num) => AlgExprKey::Literal(num.into()),
            AlgExpr::Ident(id) => AlgExprKey::Ident(id),
            AlgExpr::Binary { left, op, right } => AlgExprKey::Binary { left, op, right },
            AlgExpr::Unary { op, inner } => AlgExprKey::Unary { op, inner },
            AlgExpr::FnCall { name, args } => AlgExprKey::FnCall { name, args }
        }
    }
}

#[derive(Default)]
pub struct Arena {
    table: HashMap<AlgExprKey, NodeId>,
    strings: HashMap<NodeId, String>,
    nodes: Vec<AlgExpr>
}

impl Arena {
    pub(crate) fn intern(&mut self, node: AlgExpr) -> NodeId {
        // check if node already exists
        let key = node.clone().into();
        if let Some(&id) = self.table.get(&key) {
            return id;
        }

        let id: NodeId = self.nodes.len().into();

        self.nodes.push(node);
        self.table.insert(key.clone(), id);

        let string: String = self.create_whole_string(id);
        self.strings.insert(id, string);

        id
    }

    pub(crate) fn get_whole_string(&self, id: NodeId) -> &str {
        &self.strings[&id]
    }

    fn create_whole_string(&self, id: NodeId) -> String {
        let node = &self[id];
        algexpr_to_string(self, node)
    }
}

impl std::ops::Index<NodeId> for Arena {
    type Output = AlgExpr;
    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index.0]
    }
}

fn algexpr_to_string(arena: &Arena, node: &AlgExpr) -> String {
    match node {
        AlgExpr::Literal(num) => format!("{}", num),
        AlgExpr::Ident(id) => format!("{}", id),
        AlgExpr::Unary { op, inner } => {
            let arg = arena.strings[inner].clone();
            let node = &arena[*inner];
            let arg = if Precedence::of_alg(node) < Precedence::Unary {
                format!("({})", arg)
            } else {
                arg
            };
            format!("{}{}", op, arg)
        }
        AlgExpr::Binary { left, op, right } => {
            binary_to_string(arena, left, op, right)
        }
        AlgExpr::FnCall { name, args } => {
            let args_string = args.iter()
                .map(|id| arena.strings[id].clone())
                .collect::<Vec<String>>()
                .join(",");
            format!("{}({})", name, args_string)
        }
    }
}

fn binary_to_string(arena: &Arena, left: &NodeId, op: &AlgBinOp, right: &NodeId) -> String {
    let left_arg = arena.strings[left].clone();
    let right_arg = arena.strings[right].clone();

    // precedence and parentheses
    let node_l = &arena[*left];
    let left_arg = if Precedence::of_alg(node_l) < Precedence::of_alg_binop(op) {
        format!("({})", left_arg)
    } else {
        left_arg
    };

    let node_r = &arena[*right];
    let mut right_arg = if Precedence::of_alg(node_r) < Precedence::of_alg_binop(op) {
        format!("({})", right_arg)
    } else {
        right_arg
    };

    let implicit = *op == AlgBinOp::Mul && match (node_l, node_r) {
        // 2x, 2 x, 3 f(...), 3f(...)
        (AlgExpr::Literal(_), AlgExpr::Ident(_)) => true,
        (AlgExpr::Literal(_), AlgExpr::FnCall{..}) => true,
        // 3(...), 3 (...)
        (AlgExpr::Literal(_), AlgExpr::Binary { op, .. }) => {
            Precedence::of_alg_binop(op) != Precedence::Product
        }
        // 4(-1), 4 (-1)
        (AlgExpr::Literal(_), AlgExpr::Unary{..}) => {
            right_arg = format!("({})", right_arg);
            true
        },

        // x 4
        (AlgExpr::Ident(_), AlgExpr::Literal(_)) => true,
        // x x, x f(...)
        (AlgExpr::Ident(_), AlgExpr::Ident(_)) => true,
        (AlgExpr::Ident(_), AlgExpr::FnCall{..}) => true,

        // -2x, -2 x, -2f(...), -2 f(...)
        (AlgExpr::Unary{..}, AlgExpr::Ident(_)) => true,
        (AlgExpr::Unary{..}, AlgExpr::FnCall{..}) => true,
        // -2(...), -2 (...)
        (AlgExpr::Unary{..}, AlgExpr::Binary { op, .. }) => {
            Precedence::of_alg_binop(op) < Precedence::Product
        },

        // f(...)x, f(...) x, f(...)g(...), f(...) g(...)
        (AlgExpr::FnCall{..}, AlgExpr::Ident(_)) => true,
        (AlgExpr::FnCall{..}, AlgExpr::FnCall{..}) => true,

        // f(...)(...), f(...) (...)
        (AlgExpr::FnCall{..}, AlgExpr::Binary { op, .. }) => {
            if Precedence::of_alg_binop(op) <= Precedence::Product {
                right_arg = format!("({})", right_arg);
            }
            true
        },

        _ => false
    };

    if implicit {
        format!("{}{}", left_arg, right_arg)
    } else if matches!(op, AlgBinOp::Exp) {
        format!("{}{}{}", left_arg, op, right_arg)
    } else {
        format!("{} {} {}", left_arg, op, right_arg)
    }
}

