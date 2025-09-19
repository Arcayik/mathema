use std::collections::HashMap;
use std::rc::Rc;

use mathema_core::{
    algebra::{AlgebraBuilder, Evaluator}, context::Context, function::{Function, FunctionBuilder}, intrinsics, parsing::{Expr, FnDecl, Spanned, Stmt, VarDecl}
};

use crate::Diagnostic;

pub struct ScopeContext {
    variables: HashMap<Box<str>, f64>,
    functions: HashMap<Box<str>, Rc<Function>>,
}

impl Default for ScopeContext {
    fn default() -> Self {
        let mut ctxt = ScopeContext {
            variables: HashMap::new(),
            functions: HashMap::new()
        };
        ctxt.define_defaults();
        ctxt
    }
}

impl Context for ScopeContext {
    fn get_variable(&self, name: &str) -> Option<f64> {
        self.variables.get(name).copied()
    }

    fn set_variable(&mut self, name: Box<str>, value: f64) {
        self.variables.insert(name, value);
    }

    fn get_function(&self, name: &str) -> Option<Rc<Function>> {
        self.functions.get(name).map(Rc::clone)
    }

    fn set_function(&mut self, name: Box<str>, func: Function) {
        self.functions.insert(name, func.into());
    }
}

pub enum Outcome {
    ShowDiagnostics(Vec<Diagnostic>),
    ShowAnswer(f64),
    AssignVar(Box<str>, f64),
    DefineFn(Box<str>)
}

pub fn process_statement(ctxt: &mut dyn Context, stmt: Stmt) -> Outcome {
    match stmt {
        Stmt::Expr(expr) => process_expr(ctxt, expr),
        Stmt::VarDecl(var_decl) => process_var_decl(ctxt, var_decl),
        Stmt::FnDecl(fn_decl) => process_fn_decl(ctxt, fn_decl)
    }
}

pub fn process_expr(ctxt: &mut dyn Context, expr: Expr) -> Outcome {
    let algebra = AlgebraBuilder::new(ctxt, vec![]).build(&expr);
    let algebra = match algebra {
        Ok(a) => a,
        Err(e) => return Outcome::ShowDiagnostics(Diagnostic::from_vec(e)),
    };

    let mut evaluator = Evaluator::new(&[]);
    algebra.accept(&mut evaluator);
    let answer = evaluator.take_answer().unwrap();

    ctxt.set_variable("ans".into(), answer);
    Outcome::ShowAnswer(answer)
}

pub fn process_var_decl(ctxt: &mut dyn Context, var_decl: VarDecl) -> Outcome {
    let name = var_decl.var_name.repr.clone();
    let expr = var_decl.expr;

    let algebra = AlgebraBuilder::new(ctxt, vec![]).build(&expr);
    let algebra = match algebra {
        Ok(a) => a,
        Err(e) => return Outcome::ShowDiagnostics(Diagnostic::from_vec(e)),
    };

    let mut evaluator = Evaluator::new(&[]);
    algebra.accept(&mut evaluator);
    let answer = evaluator.take_answer().unwrap();

    if intrinsics::BUILTIN_VARS.contains(&name.as_ref()) {
        let diag = Diagnostic {
            msg: format!("Cannot redefine built-in var '{name}'"),
            spans: vec![var_decl.var_name.span]
        };
        Outcome::ShowDiagnostics(vec![diag])
    } else {
        ctxt.set_variable(name.clone(), answer);
        Outcome::AssignVar(name, answer)
    }
}

pub fn process_fn_decl(ctxt: &mut dyn Context, fn_decl: FnDecl) -> Outcome {
    let name = fn_decl.sig.fn_name.repr.clone().into_string();
    let body = fn_decl.body;
    let params: Vec<_> = fn_decl.sig.inputs
        .iter()
        .cloned()
        .map(|p| p.repr.into_string())
        .collect();

    let builder = FunctionBuilder::new(name.clone(), params, ctxt);
    let result = builder.build_function(body)
        .map_err(Diagnostic::from_vec);

    match result {
        Ok(f) => {
            if intrinsics::BUILTIN_FUNCS.contains(&name.as_ref()) {
                let diag = Diagnostic {
                    msg: format!("Cannot redefine built-in function '{name}'"),
                    spans: vec![fn_decl.sig.fn_name.span()],
                };
                Outcome::ShowDiagnostics(vec![diag])
            } else {
                ctxt.set_function(name.clone().into(), f);
                Outcome::DefineFn(name.into())
            }
        },
        Err(d) => Outcome::ShowDiagnostics(d)
    }
}
