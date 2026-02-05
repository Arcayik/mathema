use std::collections::HashMap;

use crate::{
    algebra::{
        ast::{expr_to_algebra, AlgebraTree},
        eval::{EvalError, Evaluate},
        display::Display,
    },
    arena::Arena,
    error::MathemaError,
    function::{FnParams, Function},
    intrinsics::{self, is_binary_func, is_constant, is_unary_func},
    parsing::{
        ast::AstStmt,
        lexer::tokenize,
        parser::ParseBuffer,
        token::Span,
    },
    symbol::Symbol
};

#[derive(Default)]
pub struct Context {
    pub arena: Arena,
    variables: HashMap<Symbol, AlgebraTree>,
    functions: HashMap<Symbol, Function>,
}

impl Context {
    pub fn set_variable(&mut self, name: Symbol, value: AlgebraTree) {
        self.variables.insert(name, value);
    }

    pub fn get_variable(&self, name: Symbol) -> Option<&AlgebraTree> {
        self.variables.get(&name)
    }

    pub fn has_variable(&self, name: Symbol) -> bool {
        self.variables.contains_key(&name)
    }

    pub fn set_function(&mut self, name: Symbol, func: Function) {
        self.functions.insert(name, func);
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
    pub span: Span
}

#[derive(Debug)]
pub enum DefErrorKind {
    ReservedVar(Symbol),
    ReservedFunc(Symbol)
}

#[derive(Debug)]
pub enum FuncError {
    BadArgs(isize),
    Eval(Vec<EvalError>),
    NotDefined(Symbol)
}

#[derive(Debug)]
pub enum VarError {
    Eval {
        name: Symbol,
        errors: Vec<EvalError>
    },
    NotDefined(Symbol)
}

pub enum Outcome {
    Answer(f64),
    Var(Symbol),
    Fn(Symbol)
}

pub fn call_variable(ctxt: &Context, name: Symbol) -> Result<f64, VarError> {
    if is_constant(name.as_str()) {
        Ok(intrinsics::CONSTANTS[name.as_str()])
    } else if let Some(var_alg) = ctxt.get_variable(name) {
        match var_alg.visit(ctxt, Evaluate) {
            Ok(ans) => Ok(ans),
            Err(errors) => {
                Err(VarError::Eval { name, errors })
            }
        }
    } else {
        Err(VarError::NotDefined(name))
    }
}

pub fn mathema_parse(ctxt: &mut Context, input: &str) -> Result<Outcome, MathemaError> {
    let buffer = tokenize(input).map_err(MathemaError::Lexer)?;

    let parsebuffer = ParseBuffer::new(buffer);
    let stmt = parsebuffer.parse::<AstStmt>()
        .map_err(MathemaError::Parser)?;

    match stmt {
        AstStmt::Expr(expr) => {
            let alg = expr_to_algebra(&expr, &mut ctxt.arena );
            println!("{}", alg.visit(ctxt, Display));
            match alg.visit(ctxt, Evaluate) {
                Ok(ans) => {
                    ctxt.set_variable(Symbol::intern("ans"), alg);
                    Ok(Outcome::Answer(ans))
                },
                Err(e) => {
                    Err(MathemaError::Eval(e))
                }
            }
        },
        AstStmt::VarDecl(decl) => {
            let name = decl.var_name.symbol;
            let alg = expr_to_algebra(&decl.expr, &mut ctxt.arena);

            if is_constant(name.as_str()) {
                // TODO: change, next reimplementation
                let span = Span { start: 0, end: name.as_str().len() };
                let kind = DefErrorKind::ReservedVar(name);
                let error = DefError { kind, span };
                return Err(MathemaError::Definition(error))
            }
            ctxt.set_variable(name, alg);
            Ok(Outcome::Var(name))
        },
        AstStmt::FnDecl(decl) => {
            let name = decl.sig.fn_name.symbol;
            let alg = expr_to_algebra(&decl.expr, &mut ctxt.arena);

            let arg_vec: Vec<Symbol> = decl.sig.inputs
                .iter()
                .map(|i| i.symbol)
                .collect();
            let params = FnParams::from_vec(arg_vec);

            let func = Function::new(alg, params);

            if is_unary_func(name.as_str()) || is_binary_func(name.as_str()) {
                let span = Span { start: 0, end: name.as_str().len() };
                let kind = DefErrorKind::ReservedFunc(name);
                let error = DefError { kind, span };
                return Err(MathemaError::Definition(error))
            }
            ctxt.set_function(name, func);
            Ok(Outcome::Fn(name))
        }
    }
}

