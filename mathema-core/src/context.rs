use std::collections::HashMap;

use crate::{
    algebra::{eval_algebra, AlgExpr, AlgStmt, EvalError, Value}, error::MathemaError, function::Function, intrinsics, parsing::{
        ast::AstStmt,
        lexer::tokenize,
        parser::ParseBuffer, token::Span,
    }, snippet::{create_algebra_snippet, create_function_snippet}, symbol::Symbol, value::MathemaValue
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
pub struct DefError {
    pub kind: DefErrorKind,
    pub name: Symbol,
    pub span: Span
}

#[derive(Debug)]
pub enum DefErrorKind {
    ReservedVar(Symbol),
    ReservedFunc(Symbol)
}

#[derive(Debug)]
pub enum FuncError {
    BadArgs(Symbol, isize),
    Eval {
        name: Symbol,
        origin: Box<str>,
        errors: Vec<EvalError>
    },
    NotDefined(Symbol)
}

#[derive(Debug)]
pub enum VarError {
    Eval {
        name: Symbol,
        origin: Box<str>,
        errors: Vec<EvalError>
    },
    NotDefined(Symbol)
}

pub enum Outcome {
    Answer(MathemaValue),
    Var(Symbol),
    Fn(Symbol)
}

pub fn mathema_parse(context: &mut Context, input: &str) -> Result<Outcome, MathemaError> {
    let (buffer, errors) = tokenize(input);
    if !errors.is_empty() {
        return Err(MathemaError::Lexer(errors))
    }

    let parsebuffer = ParseBuffer::new(buffer);
    let stmt = parsebuffer.parse::<AstStmt>()
        .map_err(|e| MathemaError::Parser(e))?;

    let alg_stmt = AlgStmt::from_expr_stmt(&stmt);
    match alg_stmt {
        AlgStmt::Expr(expr) => {
            match eval_algebra(context, &expr) {
                Ok(ans) => {
                    context.set_variable(Symbol::intern("ans"), AlgExpr::Value(Value::Num(ans.clone())));
                    Ok(Outcome::Answer(ans))
                },
                Err(e) => {
                    Err(MathemaError::Eval(e))
                }
            }
        },
        AlgStmt::VarDecl(name, alg) => {
            if intrinsics::CONSTANTS.contains_key(name.as_str()) {
                let snip = create_algebra_snippet(&alg);
                let _source = snip.source();
                let span = Span { start: 0, end: name.as_str().len() };
                let kind = DefErrorKind::ReservedVar(name);
                let error = DefError { kind, name, span };
                return Err(MathemaError::Definition(error))
            }
            context.set_variable(name, alg);
            Ok(Outcome::Var(name))
        },
        AlgStmt::FnDecl(func) => {
            let name = func.name;
            if intrinsics::UNARY_FUNCS.contains_key(name.as_str()) || intrinsics::BINARY_FUNCS.contains_key(name.as_str()) {
                let snip = create_function_snippet(&func);
                // TODO: incorporate this
                let _source = snip.source();
                let span = Span { start: 0, end: name.as_str().len() };
                let kind = DefErrorKind::ReservedFunc(name);
                let error = DefError { kind, name, span };
                return Err(MathemaError::Definition(error))
            }
            context.set_function(name, func);
            Ok(Outcome::Fn(name))
        }
    }
}

