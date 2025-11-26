use std::collections::HashMap;

use crate::{
    algebra::{eval_algebra, AlgExpr, AlgStmt, EvalError, EvalErrorKind, Value},
    error::Diagnostic,
    function::Function,
    snippet::{create_algebra_snippet, create_function_snippet},
    intrinsics,
    parsing::{
        ast::AstStmt,
        lexer::tokenize,
        parser::ParseBuffer, token::Span,
    },
    symbol::Symbol,
    value::MathemaValue,
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
        self.variables.contains_key(&name)
    }

    pub fn set_function(&mut self, name: Symbol, body: Function) {
        self.functions.insert(name, body);
    }

    pub fn get_function(&self, name: Symbol) -> Option<&Function> {
        self.functions.get(&name)
    }

    pub fn has_function(&self, name: Symbol) -> bool {
        self.functions.contains_key(&name)
    }
}

#[derive(Debug)]
pub enum CallError {
    BadArgs(isize),
    Eval(Vec<EvalError>),
    NotDefined(Symbol)
}

impl std::fmt::Display for CallError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BadArgs(_off) => write!(f, "wrong number of arguments"),
            // TODO: show function or var decl source
            Self::Eval(e) => write!(f, "eval errors: {e:?}"),
            Self::NotDefined(name) => write!(f, "function '{}' not defined", name),
        }
    }
}

pub enum Outcome {
    Answer(MathemaValue),
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
    let stmt = parsebuffer.parse::<AstStmt>()
        .map_err(|e| vec![Box::new(e) as Box<dyn Diagnostic>])?;

    let alg_stmt = AlgStmt::from_expr_stmt(&stmt);
    match alg_stmt {
        AlgStmt::Expr(expr) => {
            match eval_algebra(context, &expr) {
                Ok(ans) => {
                    context.set_variable(Symbol::intern("ans"), AlgExpr::Value(Value::Num(ans.clone())));
                    Ok(Outcome::Answer(ans))
                },
                Err(e) => {
                    let diags = e.into_iter()
                        .map(|e| Box::new(e) as Box<dyn Diagnostic>)
                        .collect();
                    Err(diags)
                }
            }
        },
        AlgStmt::VarDecl(symbol, alg) => {
            // TODO: this ain't right, fix
            if intrinsics::CONSTANTS.contains_key(symbol.as_str()) {
                let snip = create_algebra_snippet(&alg);
                let source = snip.source();
                let span = Span { start: 0, end: symbol.as_str().len() };
                let error = EvalError {
                    kind: EvalErrorKind::UndefinedVar(symbol),
                    source: source.into(),
                    span
                };
                return Err(vec![Box::new(error) as Box<dyn Diagnostic>])
            }
            context.set_variable(symbol, alg);
            Ok(Outcome::Var(symbol))
        },
        AlgStmt::FnDecl(symbol, func) => {
            // TODO: neither is this, FIX
            if intrinsics::UNARY_FUNCS.contains_key(symbol.as_str()) || intrinsics::BINARY_FUNCS.contains_key(symbol.as_str()) {
                let snip = create_function_snippet(symbol, &func);
                let source = snip.source();
                let span = Span { start: 0, end: symbol.as_str().len() };
                let error = EvalError {
                    kind: EvalErrorKind::UndefinedVar(symbol),
                    source: source.into(),
                    span
                };
                return Err(vec![Box::new(error) as Box<dyn Diagnostic>])
            }
            context.set_function(symbol, func);
            Ok(Outcome::Fn(symbol))
        }
    }
}

