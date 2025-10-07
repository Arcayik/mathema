use mathema_core::{
    Name,
    algebra::AlgebraBuilder,
    context::Context,
    intrinsics,
    parsing::{ast::{Stmt, Expr, VarDecl, FnDecl}}
};

use crate::Diagnostic;

pub enum Outcome {
    ShowDiagnostics(Vec<Diagnostic>),
    ShowAnswer(f64),
    AssignVar(Name, f64),
    DefineFn(Name)
}

pub fn process_statement(ctxt: &mut Context, stmt: Stmt) -> Outcome {
    match stmt {
        Stmt::Expr(expr) => process_expr(ctxt, expr),
        Stmt::VarDecl(var_decl) => process_var_decl(ctxt, var_decl),
        Stmt::FnDecl(fn_decl) => process_fn_decl(ctxt, fn_decl)
    }
}

pub fn process_expr(ctxt: &mut Context, expr: Expr) -> Outcome {
    let algebra = AlgebraBuilder::new(ctxt, vec![]).build(&expr);
    let algebra = match algebra {
        Ok(a) => a,
        Err(e) => return Outcome::ShowDiagnostics(Diagnostic::from_vec(e)),
    };

    let answer = match algebra.evaluate(ctxt, &[]) {
        Ok(a) => a,
        Err(_e) => todo!()
    };

    ctxt.set_var(String::from("ans"), answer);
    Outcome::ShowAnswer(answer)
}

pub fn process_var_decl(ctxt: &mut Context, var_decl: VarDecl) -> Outcome {
    let name = var_decl.var_name.name.clone();
    let expr = var_decl.expr;

    let algebra = AlgebraBuilder::new(ctxt, vec![]).build(&expr);
    let algebra = match algebra {
        Ok(a) => a,
        Err(e) => return Outcome::ShowDiagnostics(Diagnostic::from_vec(e)),
    };

    let answer = match algebra.evaluate(ctxt, &[]) {
        Ok(a) => a,
        Err(_e) => todo!()
    };

    if intrinsics::CONSTANTS.contains_key(&*name) {
        let diag = Diagnostic {
            msg: format!("Cannot redefine built-in var '{name}'"),
            spans: vec![var_decl.var_name.span]
        };
        Outcome::ShowDiagnostics(vec![diag])
    } else {
        ctxt.set_var(name.to_string(), answer);
        Outcome::AssignVar(name, answer)
    }
}

pub fn process_fn_decl(ctxt: &mut Context, fn_decl: FnDecl) -> Outcome {
    let name = fn_decl.sig.fn_name.name;
    let body = fn_decl.body;
    let params: Vec<_> = fn_decl.sig.inputs
        .iter()
        .cloned()
        .map(|p| p.name.to_string())
        .collect();

    let result = AlgebraBuilder::new(ctxt, params).build(&body);

    match result {
        Ok(f) => {
            if intrinsics::CONST_FNS.contains_key(&*name) {
                let diag = Diagnostic {
                    msg: format!("Cannot redefine built-in function '{name}'"),
                    spans: vec![fn_decl.sig.fn_name.span],
                };
                Outcome::ShowDiagnostics(vec![diag])
            } else {
                ctxt.set_func(name.to_string(), f);
                Outcome::DefineFn(name)
            }
        },
        Err(d) => Outcome::ShowDiagnostics(Diagnostic::from_vec(d))
    }
}
