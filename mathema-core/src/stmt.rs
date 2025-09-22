use crate::{
    punctuated::Punctuated,
    expr::Expr,
    token::{Ident, Spanned, Parse, Span, End, Paren},
    parser::{ParseStream, ParseError}
};

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    VarDecl(VarDecl),
    FnDecl(FnDecl)
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

impl From<FnDecl> for Stmt {
    fn from(value: FnDecl) -> Self {
        Stmt::FnDecl(value)
    }
}

impl Spanned for Stmt {
    fn span(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.span(),
            Self::VarDecl(vardecl) => vardecl.span(),
            Self::FnDecl(fndecl) => fndecl.span(),
        }
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let result = if input.peek::<Ident>() && input.peek2::<Token![=]>() {
            input.parse().map(Stmt::VarDecl)
        } else if input.peek::<Ident>() && input.peek2::<Paren>() {
            // might be a declaration, might just be a call in an expr
            parse_ambiguous_fn(input)
        } else {
            input.parse().map(Stmt::Expr)
        };

        if !input.peek::<End>() {
            return Err(input.error("Trailing token"))
        }

        result
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

impl Parse for VarDecl {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        Ok(VarDecl {
            var_name: input.parse()?,
            equals: input.parse()?,
            expr: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct FnSig {
    pub fn_name: Ident,
    pub parens: Paren,
    pub inputs: Punctuated<Ident, Token![,]>
}

impl Spanned for FnSig {
    fn span(&self) -> Span {
        let end = self.inputs.try_span()
            .or_else(|| Some(self.parens.span()))
            .unwrap()
            .end;

        Span {
            start: self.fn_name.span().start,
            end
        }
    }
}

impl Parse for FnSig {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let fn_name = input.parse()?;
        let parens = input.parse()?;
        Ok(FnSig {
            fn_name,
            parens,
            inputs: parse_punctuated_group(input)?,
        })
    }
}

pub fn parse_punctuated_group<T: Parse, S: Parse>(input: ParseStream)
-> Result<Punctuated<T, S>, ParseError> {
    let mut punctuated = Punctuated::new();
    loop {
        if input.peek::<End>() {
            input.next_token();
            break
        }
        let ident = input.parse()?;
        punctuated.push_value(ident);

        if input.peek::<End>() {
            input.next_token();
            break
        }
        let comma = input.parse()?;
        punctuated.push_separator(comma);
    }
    Ok(punctuated)
}

#[derive(Debug)]
pub struct FnDecl {
    pub sig: FnSig,
    pub equals: Token![=],
    pub body: Expr,
}

impl Spanned for FnDecl {
    fn span(&self) -> Span {
        Span {
            start: self.sig.span().start,
            end: self.body.span().end
        }
    }
}

impl Parse for FnDecl {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let sig = input.parse()?;
        let equals: Token![=] = input.parse()?;
        let body = input.parse()?;
        Ok(FnDecl {
            sig,
            equals,
            body,
        })
    }
}

fn parse_ambiguous_fn(input: ParseStream) -> Result<Stmt, ParseError> {
    let begin = input.save_pos();

    let name = input.parse()?;

    if input.peek_ignore_group::<Token![=]>() {
        let parens = input.parse()?;

        let inputs = parse_punctuated_group(input)?;
        let sig = FnSig {
            fn_name: name,
            parens,
            inputs
        };
        Ok(FnDecl{
            sig,
            equals: input.parse()?,
            body: input.parse()?,
        }.into())
    } else {
        input.restore_pos(begin);
        input.parse().map(Stmt::Expr)
    }
}
