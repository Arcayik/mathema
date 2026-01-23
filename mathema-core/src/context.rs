use std::collections::HashMap;

use crate::{
    algebra::{
        ast::{expr_to_algebra, AlgebraTree},
        eval::{EvalError, Evaluate}
    },
    intrinsics::{self, is_binary_func, is_constant, is_unary_func},
    parsing::{
        ast::AstStmt,
        lexer::tokenize,
        parser::ParseBuffer,
        token::Span,
    },
    error::MathemaError,
    function::{FnParams, Function},
    symbol::Symbol,
    value::MathemaValue
};

pub trait ValueSource {
    fn get_variable(&self, name: Symbol) -> Option<&AlgebraTree>;
    fn get_function(&self, name: Symbol) -> Option<&Function>;
}

#[derive(Default)]
pub struct Context {
    variables: HashMap<Symbol, AlgebraTree>,
    functions: HashMap<Symbol, Function>,
}

impl ValueSource for Context {
    fn get_variable(&self, name: Symbol) -> Option<&AlgebraTree> {
        self.get_variable(name)
    }

    fn get_function(&self, name: Symbol) -> Option<&Function> {
        self.get_function(name)
    }
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
    Answer(MathemaValue),
    Var(Symbol),
    Fn(Symbol)
}

pub fn call_variable(context: &dyn ValueSource, name: Symbol) -> Result<MathemaValue, VarError> {
    if is_constant(name.as_str()) {
        Ok(intrinsics::CONSTANTS[name.as_str()].clone())
    } else if let Some(var_alg) = context.get_variable(name) {
        match var_alg.accept(Evaluate(context)) {
            Ok(ans) => Ok(ans),
            Err(errors) => {
                Err(VarError::Eval { name, errors })
            }
        }
    } else {
        Err(VarError::NotDefined(name))
    }
}

pub fn mathema_parse(context: &mut Context, input: &str) -> Result<Outcome, MathemaError> {
    let (buffer, errors) = tokenize(input);
    if !errors.is_empty() {
        return Err(MathemaError::Lexer(errors))
    }

    let parsebuffer = ParseBuffer::new(buffer);
    let stmt = parsebuffer.parse::<AstStmt>()
        .map_err(MathemaError::Parser)?;

    match stmt {
        AstStmt::Expr(expr) => {
            let alg = expr_to_algebra(&expr);
            match alg.accept(Evaluate(context)) {
                Ok(ans) => {
                    context.set_variable(Symbol::intern("ans"), alg);
                    Ok(Outcome::Answer(ans))
                },
                Err(e) => {
                    Err(MathemaError::Eval(e))
                }
            }
        },
        AstStmt::VarDecl(decl) => {
            let name = decl.var_name.symbol;
            let alg = expr_to_algebra(&decl.expr);

            if is_constant(name.as_str()) {
                // TODO: change, next reimplementation
                let span = Span { start: 0, end: name.as_str().len() };
                let kind = DefErrorKind::ReservedVar(name);
                let error = DefError { kind, span };
                return Err(MathemaError::Definition(error))
            }
            context.set_variable(name, alg);
            Ok(Outcome::Var(name))
        },
        AstStmt::FnDecl(decl) => {
            let name = decl.sig.fn_name.symbol;
            let alg = expr_to_algebra(&decl.expr);

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
            context.set_function(name, func);
            Ok(Outcome::Fn(name))
        }
    }
}

