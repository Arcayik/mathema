use std::{
    collections::HashMap,
    rc::Rc
};

use crate::{
    diagnostic::Diagnostic,
    function::{create_function, Function},
    intrinsics::{self, declare_constants, declare_functions}, token::Spanned,
};

pub struct Context {
    variables: HashMap<Box<str>, f64>,
    functions: HashMap<Box<str>, Rc<Function>>,
}

impl Default for Context {
    fn default() -> Self {
        let variables = declare_constants();
        let functions = declare_functions();
        Context { variables, functions }
    }
}

impl Context {
    pub fn get_variable(&self, name: &str) -> Option<f64> {
        self.variables.get(name).copied()
    }

    pub fn set_variable(&mut self, name: Box<str>, value: f64) {
        self.variables.insert(name, value);
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<Function>> {
        self.functions.get(name).map(Rc::clone)
    }

    pub fn set_function(&mut self, name: Box<str>, func: Function) {
        self.functions.insert(name, func.into());
    }
}

pub enum Outcome {
    ShowDiagnostics(Vec<Diagnostic>),
    ShowAnswer(f64),
    AssignVar(Box<str>, f64),
    DefineFn(Box<str>)
}

use crate::stmt::Stmt;

pub fn process_statement(ctxt: &mut Context, stmt: Stmt) -> Outcome {
    match stmt {
        Stmt::Expr(expr) => {
            let result = expr.eval(ctxt).map_err(Diagnostic::from_vec);
            match result {
                Ok(num) => {
                    ctxt.set_variable("ans".into(), num);
                    Outcome::ShowAnswer(num)
                },
                Err(diags) => Outcome::ShowDiagnostics(diags)
            }
        },
        Stmt::VarDecl(var_decl) => {
            let name = var_decl.var_name.repr.clone();
            match var_decl.expr.eval(ctxt) {
                Ok(ans) => {
                    if intrinsics::BUILTIN_VARS.contains(&name.as_ref()) {
                        let diag = Diagnostic {
                            msg: format!("Cannot redefine built-in var '{name}'"),
                            spans: vec![var_decl.var_name.span]
                        };
                        Outcome::ShowDiagnostics(vec![diag])
                    } else {
                        ctxt.set_variable(name.clone(), ans);
                        Outcome::AssignVar(name, ans)
                    }
                }
                Err(e) => Outcome::ShowDiagnostics(Diagnostic::from_vec(e))
            }
        }
        Stmt::FnDecl(fn_decl) => {
            let name = fn_decl.sig.fn_name.repr.clone().into_string();
            let body = fn_decl.body;
            let params: Vec<_> = fn_decl.sig.inputs
                .iter()
                .cloned()
                .map(|p| p.repr.into_string())
                .collect();

            let result = create_function(name.clone(), params, body, ctxt)
                .map_err(Diagnostic::from);
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
                Err(d) => Outcome::ShowDiagnostics(vec![d])
            }
        }
    }
}
