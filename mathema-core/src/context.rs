use std::collections::HashMap;

use crate::{
    algebra::{AlgExpr, AlgStmt, AlgebraVisit, EvalError, Evaluator},
    function::Function,
    intrinsics,
    parsing::{
        ast::Stmt,
        lexer::{tokenize, LexError},
        parser::{ParseBuffer, ParseError},
    },
    Name
};

#[derive(Default)]
pub struct Context {
    variables: HashMap<String, AlgExpr>,
    functions: HashMap<String, Function>,
}

impl Context {
    pub fn set_variable(&mut self, name: String, value: AlgExpr) {
        self.variables.insert(name, value);
    }

    pub fn get_variable(&self, name: &str) -> Option<&AlgExpr> {
        self.variables.get(name)
    }

    pub fn has_variable(&self, name: &str) -> bool {
        self.get_builtin_variable(name).is_some() || self.variables.contains_key(name)
    }

    pub fn set_function(&mut self, name: String, body: Function) {
        self.functions.insert(name, body);
    }

    fn get_builtin_variable(&self, name: &str) -> Option<&f64> {
        intrinsics::CONSTANTS.get(name)
    }

    fn get_builtin_function(&self, name: &str) -> Option< &fn(&[f64])->f64 > {
        intrinsics::CONST_FNS.get(name)
    }

    pub fn call_func(&self, name: &str, args: &[f64]) -> Result<f64, CallError> {
        let func = match self.functions.get(name) {
            Some(f) => f,
            None => return Err(CallError::NotFound)
        };

        func.evaluate(self, args)
            .map(Ok)?
    }

    pub fn has_func(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}

pub enum CallError {
    BadArgs(isize),
    Eval(Vec<EvalError>),
    NotFound,
}

pub enum MathemaError {
    Lexing(Vec<LexError>),
    Parsing(Vec<ParseError>),
    Eval(Vec<EvalError>)
}

pub enum Outcome {
    Answer(f64),
    Var(Name),
    Fn(Name)
}

pub fn mathema_parse(context: &mut Context, input: &str) -> Result<Outcome, MathemaError> {
    let (buffer, errors) = tokenize(input);
    if !errors.is_empty() {
        return Err(MathemaError::Lexing(errors));
    }

    let parsebuffer = ParseBuffer::new(buffer);
    let stmt = parsebuffer.parse::<Stmt>()
        .map_err(|e| MathemaError::Parsing(vec![e]))?;

    let alg_stmt = AlgStmt::from_expr_stmt(&stmt);
    process_algebra_stmt(context, alg_stmt)
}

fn process_algebra_stmt(context: &mut Context, alg_stmt: AlgStmt) -> Result<Outcome, MathemaError> {
    match alg_stmt {
        AlgStmt::Expr(expr) => {
            let mut eval = Evaluator::new(context, &[]);
            if let Some(ans) = eval.visit_expr(&expr) {
                Ok(Outcome::Answer(ans))
            } else {
                Err(MathemaError::Eval(eval.take_errors()))
            }
        },
        AlgStmt::VarDecl(name, expr) => {
            context.set_variable(name.to_string(), expr);
            Ok(Outcome::Var(name))
        },
        AlgStmt::FnDecl(name, func) => {
            context.set_function(name.to_string(), func);
            Ok(Outcome::Fn(name))
        }
    } 
}

