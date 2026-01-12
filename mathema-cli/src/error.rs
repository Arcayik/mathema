use mathema_core::{
    algebra::{EvalError, EvalErrorKind}, context::{Context, DefError, DefErrorKind, FuncError, VarError}, parsing::{
        lexer::LexError,
        parser::ParseError,
    } 
};

pub trait ErrorDisplay {
    fn display(&self, context: &Context) -> String;
}

impl ErrorDisplay for LexError {
    fn display(&self, _context: &Context) -> String {
        format!("{}", self)
    }
}

impl ErrorDisplay for ParseError {
    fn display(&self, _context: &Context) -> String {
        format!("{}", self)
    }
}

impl ErrorDisplay for EvalError {
    fn display(&self, context: &Context) -> String {
        match &self.kind {
            EvalErrorKind::BadVar(e) => {
                let var_err = e.display(context);
                format!("{var_err}")
            },
            EvalErrorKind::BadFnCall(e) => {
                let func_err = e.display(context);
                format!("Bad function call: {func_err}")
            }
        }
    }
}

impl ErrorDisplay for VarError {
    fn display(&self, context: &Context) -> String {
        match self {
            VarError::Eval { name, errors } => {
                let err = errors.first().unwrap().display(context);
                // TODO: show var definition
                format!("In var '{name}': {err}")
            },
            VarError::NotDefined(name) => {
                format!("Undefined var '{name}'")
            }
        }
    }
}

impl ErrorDisplay for FuncError {
    fn display(&self, context: &Context) -> String {
        match self {
            FuncError::Eval { name, errors } => {
                let err = errors.first().unwrap().display(context);
                format!("in func {name}: {err}")
            },
            FuncError::BadArgs(name, off) => {
                let many_few = if *off > 0 { "many" } else { "few" };
                format!("in func {name}: {off} too {many_few} args")
            },
            FuncError::NotDefined(name) => {
                format!("Undefined func: {name}")
            }
        }
    }
}

impl ErrorDisplay for DefError {
    fn display(&self, _context: &Context) -> String {
        match &self.kind {
            DefErrorKind::ReservedVar(name) => {
                format!("Reserved var: {name}")
            },
            DefErrorKind::ReservedFunc(name) => {
                format!("Reserved func: {name}")
            }
        }
    }
}
