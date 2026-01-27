use crate::{algebra::ast::{NodeId, TreeVisitor}, context::Context};

pub struct Display;

impl TreeVisitor for Display {
    type Output = String;
    fn visit_tree(&self, ctxt: &Context, id: NodeId) -> Self::Output {
        display_tree(ctxt, id).to_string()
    }
}

pub(crate) fn display_tree(ctxt: &Context, id: NodeId) -> &str {
    ctxt.arena.get_whole_string(id)
}

