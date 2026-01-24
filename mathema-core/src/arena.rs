use std::collections::HashMap;

use crate::{algebra::ast::{AlgBinOp, AlgExpr, AlgUnaryOp, NodeId}, symbol::Symbol};

#[derive(Hash, PartialEq, Eq)]
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

#[derive(Hash, PartialEq, Eq)]
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
    nodes: Vec<AlgExpr>
}

impl Arena {
    pub fn intern(&mut self, node: AlgExpr) -> NodeId {
        let key = node.clone().into();
        if let Some(&id) = self.table.get(&key) {
            return id;
        }

        let id = self.nodes.len();
        self.nodes.push(node);
        self.table.insert(key, id);
        id
    }
}

