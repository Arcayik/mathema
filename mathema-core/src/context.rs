use std::collections::HashMap;

use crate::{
    algebra::{AlgExpr, AlgStmt, AlgebraVisit, EvalError, Evaluator}, error::Diagnostic, function::Function, intrinsics, parsing::{
        ast::Stmt,
        lexer::tokenize,
        parser::ParseBuffer,
    }, symbol::Symbol
};

#[derive(Default)]
pub struct Context {
    variables: HashMap<Symbol, AlgExpr>,
    functions: HashMap<Symbol, Function>,
}

impl Context {
    pub fn set_variable(&mut self, name: Symbol, value: AlgExpr) {
        self.variables.insert(name, value);
    }

    pub fn get_variable(&self, name: Symbol) -> Option<&AlgExpr> {
        self.variables.get(&name)
    }

    pub fn has_variable(&self, name: Symbol) -> bool {
        self.get_builtin_variable(name).is_some() || self.variables.contains_key(&name)
    }

    pub fn set_function(&mut self, name: Symbol, body: Function) {
        self.functions.insert(name, body);
    }

    fn get_builtin_variable(&self, name: Symbol) -> Option<&f64> {
        intrinsics::CONSTANTS.get(name.as_str())
    }

    fn get_builtin_function(&self, name: Symbol) -> Option<&fn(&[f64]) -> f64> {
        intrinsics::CONST_FNS.get(name.as_str())
    }

    pub fn call_func(&self, name: Symbol, args: &[f64]) -> Result<f64, CallError> {
        if let Some(func) = self.get_builtin_function(name) {
            return Ok((func)(args))
        }

        if let Some(func) = self.functions.get(&name) {
            func.evaluate(self, args)
        } else {
            Err(CallError::NotFound)
        }
    }

    pub fn has_func(&self, name: Symbol) -> bool {
        self.functions.contains_key(&name)
    }
}

#[derive(Debug)]
pub enum CallError {
    BadArgs(isize),
    Eval(Vec<EvalError>),
    NotFound,
}

impl std::fmt::Display for CallError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BadArgs(_off) => write!(f, "wrong number of arguments"),
            Self::NotFound => write!(f, "Function not found"),
            Self::Eval(_e) => write!(f, "Eval errors: TODO")
        }
    }
}

pub enum Outcome {
    Answer(f64),
    Var(Symbol),
    Fn(Symbol)
}

pub fn mathema_parse(context: &mut Context, input: &str) -> Result<Outcome, Vec<Box<dyn Diagnostic>>> {
    let (buffer, errors) = tokenize(input);
    if !errors.is_empty() {
        let err: Vec<Box<dyn Diagnostic>> = errors
            .into_iter()
            .map(|e| Box::new(e) as Box<dyn Diagnostic>)
            .collect();
        return Err(err);
    }

    let parsebuffer = ParseBuffer::new(buffer);
    let stmt = parsebuffer.parse::<Stmt>()
        .map_err(|e| vec![Box::new(e) as Box<dyn Diagnostic>])?;

    let alg_stmt = AlgStmt::from_expr_stmt(&stmt);
    process_algebra_stmt(context, alg_stmt)
}

fn process_algebra_stmt(context: &mut Context, alg_stmt: AlgStmt) -> Result<Outcome, Vec<Box<dyn Diagnostic>>> {
    match alg_stmt {
        AlgStmt::Expr(expr) => {
            let mut eval = Evaluator::new(context);
            if let Some(ans) = eval.visit_expr(&expr) {
                Ok(Outcome::Answer(ans))
            } else {
                let errs = eval.take_errors()
                    .into_iter()
                    .map(|e| Box::new(e) as Box<dyn Diagnostic>)
                    .collect();
                Err(errs)
            }
        },
        AlgStmt::VarDecl(symbol, expr) => {
            context.set_variable(symbol, expr);
            Ok(Outcome::Var(symbol))
        },
        AlgStmt::FnDecl(symbol, func) => {
            context.set_function(symbol, func);
            Ok(Outcome::Fn(symbol))
        }
    } 
}

