use crate::StringID;

#[derive(Debug, Clone)]
pub enum Expr {
    Block(Vec<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Exp(Box<Expr>, Box<Expr>),

    Negate(Box<Expr>),

    Assign(Box<Expr>, Box<Expr>),

    Identifier(StringID),

    Number(i64),
    // FUTURE
    // If(Box<Expr>, Box<Expr>, Box<Expr>), // cond, if, else
}
