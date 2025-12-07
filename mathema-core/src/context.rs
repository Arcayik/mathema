use std::collections::HashMap;

use crate::{
    algebra::{eval_algebra, AlgExpr, AlgStmt, EvalError, Value}, error::MathemaError, function::Function, intrinsics::{self, is_binary_func, is_constant, is_unary_func}, parsing::{
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

pub fn call_variable(context: &Context, name: Symbol) -> Result<MathemaValue, VarError> {
    if is_constant(name.as_str()) {
        Ok(intrinsics::CONSTANTS[name.as_str()].clone())
    } else if let Some(var) = context.get_variable(name) {
        match eval_algebra(context, var) {
            Ok(ans) => Ok(ans),
            Err(errors) => {
                let origin = create_algebra_snippet(var).source().into();
                Err(VarError::Eval { name, origin, errors })
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
        .map_err(|e| MathemaError::Parser(e))?;

    match stmt {
        AstStmt::Expr(expr) => {
            let (alg, span_map) = expr_to_algebra(&expr);
            let mut snip = SnippetLine::empty();
            snip.source = input.to_string();
            snip.span_map = span_map;
            context.snippets.insert_var(Symbol::intern("ans"), snip);

            match eval_algebra(context, &alg) {
                Ok(ans) => {
                    context.set_variable(Symbol::intern("ans"), AlgExpr::Value(Value::Num(ans.clone())));
                    Ok(Outcome::Answer(ans))
                },
                Err(e) => {
                    Err(MathemaError::Eval(e))
                }
            }
        },
        AstStmt::VarDecl(decl) => {
            let name = decl.var_name.symbol;
            let (alg, _span_map) = expr_to_algebra(&decl.expr);

            let snip = create_var_decl_snippet(name, &alg);
            context.snippets.insert_var(name, snip);

            if is_constant(name.as_str()) {
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
            let (alg, _span_map) = expr_to_algebra(&decl.expr);

            let arg_vec: Vec<Symbol> = decl.sig.inputs
                .iter()
                .map(|i| i.symbol)
                .collect();
            let args = FnArgs::from_vec(arg_vec);

            let func = Function::new(name, alg, args);

            let name = func.name;
            if is_unary_func(name.as_str()) || is_binary_func(name.as_str()) {
                let snip = create_function_snippet(&func);
                // TODO: incorporate this
                let _source = snip.source();
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

