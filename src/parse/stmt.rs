use crate::Token;
use super::{
    Expr,
    token::{Ident, Spanned, Parse, Span},
    parser::{ParseStream, ParseError}
};

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    VarDecl(VarDecl),
}

impl From<Expr> for Stmt {
    fn from(value: Expr) -> Self {
        Stmt::Expr(value)
    }
}

impl From<VarDecl> for Stmt {
    fn from(value: VarDecl) -> Self {
        Stmt::VarDecl(value)
    }
}

impl Spanned for Stmt {
    fn span(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.span(),
            Self::VarDecl(vardecl) => vardecl.span(),
        }
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        if input.peek::<Ident>() && input.peek2::<Token![=]>() {
            Ok(VarDecl {
                var_name: input.parse()?,
                equals: input.parse()?,
                expr: input.parse()?,
            }.into())
        } else {
            input.parse().map(Stmt::Expr)
        }
    }
}

#[derive(Debug)]
pub struct VarDecl {
    pub var_name: Ident,
    pub equals: Token![=],
    pub expr: Expr,
}

impl Spanned for VarDecl {
    fn span(&self) -> Span {
        Span {
            start: self.var_name.span().start,
            end: self.expr.span().end
        }
    }
}
