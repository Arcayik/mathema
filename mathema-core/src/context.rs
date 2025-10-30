use std::collections::HashMap;

use crate::{
    algebra::{AlgExpr, AlgebraVisit, EvalError, Evaluator, ExprToAlgebra},
    function::Function,
    intrinsics,
    parsing::{
        ast::{Expr, ExprVisit, FnDecl, Stmt, VarDecl},
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
    Lexing(LexError),
    Parsing(ParseError),
    Eval(EvalError)
}

pub enum Outcome {
    Answer(f64),
    Var(Name),
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

    todo!()
}

fn process_expr(context: &mut Context, expr: Expr) -> Result<Outcome, Vec<EvalError>> {
    let algebra = ExprToAlgebra.visit_expr(&expr);

    let mut eval = Evaluator::new(context, &[]);
    let result = eval.visit_expr(&algebra);

    let ret = match result {
        Some(value) => Ok(Outcome::Answer(value)),
        None => Err(eval.take_errors())
    };

    context.set_variable(String::from("ans"), algebra);
    ret
}

fn process_var_decl(context: &mut Context, var_decl: VarDecl) -> Result<Outcome, CallError> {
    let name = var_decl.var_name.name.clone();
    let expr = var_decl.expr;

    let algebra = ExprToAlgebra.visit_expr(&expr);

    if intrinsics::CONSTANTS.contains_key(&*name) {
        todo!();
    } else {
        context.set_variable(name.to_string(), algebra);
    }

    Ok(Outcome::Var(name))
}

pub fn process_fn_decl(context: &mut Context, fn_decl: FnDecl) -> Result<Outcome, CallError> {
    let name = fn_decl.sig.fn_name.name;
    let body = fn_decl.body;
    let params: Vec<_> = fn_decl.sig.inputs
        .iter()
        .map(|p| &p.name)
        .cloned()
        .collect();

    let alg = ExprToAlgebra.visit_expr(&body);
    let algebra = Function::new(alg, params);

    if intrinsics::CONST_FNS.contains_key(&*name) {
        todo!()
    } else {
        context.set_function(name.to_string(), algebra);
    }

    Ok(Outcome::Fn(name))
}
