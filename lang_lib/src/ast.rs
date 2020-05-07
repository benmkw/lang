use crate::StringID;

#[derive(Debug, Clone)]
pub enum Expr {
    Block(Vec<Expr>),

    Add(Box<(Expr, Expr)>),
    Sub(Box<(Expr, Expr)>),
    Mult(Box<(Expr, Expr)>),
    Div(Box<(Expr, Expr)>),
    Exp(Box<(Expr, Expr)>),

    Negate(Box<Expr>),

    Assign(Box<(Expr, Expr)>),

    Identifier(StringID),

    Number(i64),
    // FUTURE
    // If(Box<Expr>, Box<Expr>, Box<Expr>), // cond, if, else
}
