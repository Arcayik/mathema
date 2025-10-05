use mathema_core::{
    Name,
    algebra::{AlgebraBuilder, Evaluator},
    context::Context,
    function::{FunctionBuilder},
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

    ctxt.set_variable(String::from("ans"), answer);
    Outcome::ShowAnswer(answer)
}

pub fn process_var_decl(ctxt: &mut dyn Context, var_decl: VarDecl) -> Outcome {
    let name = var_decl.var_name.name.clone();
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
        ctxt.set_variable(name.to_string(), answer);
        Outcome::AssignVar(name, answer)
    }
}

pub fn process_fn_decl(ctxt: &mut dyn Context, fn_decl: FnDecl) -> Outcome {
    let name = fn_decl.sig.fn_name.name;
    let body = fn_decl.body;
    let params: Vec<_> = fn_decl.sig.inputs
        .iter()
        .cloned()
        .map(|p| p.name.to_string())
        .collect();

    let builder = FunctionBuilder::new(name.to_string(), params, ctxt);
    let result = builder.build_function(body)
        .map_err(Diagnostic::from_vec);

    match result {
        Ok(f) => {
            if intrinsics::BUILTIN_FUNCS.contains(&name.as_ref()) {
                let diag = Diagnostic {
                    msg: format!("Cannot redefine built-in function '{name}'"),
                    spans: vec![fn_decl.sig.fn_name.span],
                };
                Outcome::ShowDiagnostics(vec![diag])
            } else {
                ctxt.set_function(name.to_string(), f);
                Outcome::DefineFn(name)
            }
        },
        Err(d) => Outcome::ShowDiagnostics(d)
    }
}
