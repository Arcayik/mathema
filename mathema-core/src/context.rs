use std::collections::HashMap;

use crate::{
    algebra::{Algebra, AlgebraConverter, EvalError},
    intrinsics::{self, ConstFunc},
    parsing::{
        ast::{Stmt, Expr, VarDecl, FnDecl},
        lexer::{tokenize, LexError},
        parser::{ParseBuffer, ParseError},
    },
    Name
};

#[derive(Default)]
pub struct Context {
    variables: HashMap<String, f64>,
    functions: HashMap<String, Algebra>,
}

impl Context {
    pub fn set_var(&mut self, name: String, value: f64) {
        self.variables.insert(name, value);
    }

    pub fn get_var(&self, name: &str) -> Option<f64> {
        self.variables.get(name).copied()
    }

    pub fn has_var(&self, name: &str) -> bool {
        self.get_builtin_var(name).is_some() || self.variables.contains_key(name)
    }

    pub fn set_func(&mut self, name: String, algebra: Algebra) {
        self.functions.insert(name, algebra);
    }

    fn get_builtin_var(&self, name: &str) -> Option<&f64> {
        intrinsics::CONSTANTS.get(name)
    }

    fn get_builtin_func(&self, name: &str) -> Option<&ConstFunc> {
        intrinsics::CONST_FNS.get(name)
    }

    pub fn get_func(&self, name: &str) -> Option<Function<'_>> {
        if let Some(func) = self.get_builtin_func(name) {
            return Some(Function::Builtin(func));
        }

        self.functions.get(name).map(Function::UserDefined)
    }

    pub fn call_func(&self, name: &str, args: &[f64]) -> FuncResult {
        let func = match self.functions.get(name) {
            Some(f) => f,
            None => return FuncResult::NotFound
        };

        func.evaluate(self, args)
            .map(FuncResult::Return)
            .unwrap_or_else(FuncResult::Errors)
    }

    pub fn has_func(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}

pub enum FuncResult {
    Return(f64),
    Errors(Vec<EvalError>),
    NotFound,
}

pub enum Function<'a> {
    UserDefined(&'a Algebra),
    Builtin(&'a ConstFunc)
}

impl Function<'_> {
    pub fn num_params(&self) -> usize {
        match self {
            Self::UserDefined(alg) => alg.num_params(),
            Self::Builtin(func) => func.num_params()
        }
    }
}

pub enum MathemaError {
    Lexing(LexError),
    Parsing(ParseError),
    Eval(EvalError)
}

pub enum Outcome {
    Answer(f64),
    Var(Name, f64),
    Fn(Name)
}

pub fn mathema_parse(context: &mut Context, input: &str) -> Result<Outcome, Vec<MathemaError>> {
    let (buffer, errors) = tokenize(input);
    if !errors.is_empty() {
        let errs = errors.into_iter().map(MathemaError::Lexing).collect();
        return Err(errs)
    }

    let parsebuffer = ParseBuffer::new(buffer);
    let stmt = parsebuffer.parse::<Stmt>()
        .map_err(|e| vec![MathemaError::Parsing(e)])?;

    let outcome = process_statement(context, stmt)
        .map_err(|e| e.into_iter().map(MathemaError::Eval).collect::<Vec<_>>());

    outcome
}

fn process_statement(context: &mut Context, stmt: Stmt) -> Result<Outcome, Vec<EvalError>> {
    match stmt {
        Stmt::Expr(expr) => process_expr(context, expr),
        Stmt::VarDecl(var_decl) => process_var_decl(context, var_decl),
        Stmt::FnDecl(fn_decl) => process_fn_decl(context, fn_decl)
    }
}

fn process_expr(context: &mut Context, expr: Expr) -> Result<Outcome, Vec<EvalError>> {
    let algebra = AlgebraConverter::new(vec![]).build(&expr);
    let answer = algebra.evaluate(context, &[])?;
    context.set_var(String::from("ans"), answer);
    Ok(Outcome::Answer(answer))
}

fn process_var_decl(context: &mut Context, var_decl: VarDecl) -> Result<Outcome, Vec<EvalError>> {
    let name = var_decl.var_name.name.clone();
    let expr = var_decl.expr;

    let algebra = AlgebraConverter::new(vec![]).build(&expr);
    let answer = algebra.evaluate(context, &[])?;

    if intrinsics::CONSTANTS.contains_key(&*name) {
        todo!()
    } else {
        context.set_var(name.to_string(), answer);
    }

    Ok(Outcome::Var(name, answer))
}

pub fn process_fn_decl(context: &mut Context, fn_decl: FnDecl) -> Result<Outcome, Vec<EvalError>> {
    let name = fn_decl.sig.fn_name.name;
    let body = fn_decl.body;
    let params: Vec<_> = fn_decl.sig.inputs
        .iter()
        .map(|p| &p.name)
        .cloned()
        .collect();

    let algebra = AlgebraConverter::new(params).build(&body);

    if intrinsics::CONST_FNS.contains_key(&*name) {
        todo!()
    } else {
        context.set_func(name.to_string(), algebra);
    }

    Ok(Outcome::Fn(name))
}
