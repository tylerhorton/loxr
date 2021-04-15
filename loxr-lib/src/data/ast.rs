use super::token::Token;
use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    // token
    Literal(Token),

    // op, expr
    Unary(Token, Box<Expr>),

    /// left, op, right
    Binary(Box<Expr>, Token, Box<Expr>),

    // (, expr, )
    Grouping(Box<Expr>),

    // errors
    Error,
}

impl Expr {
    pub fn new_literal(token: &Token) -> Self {
        Expr::Literal(token.clone())
    }

    pub fn new_unary(unary: (&Token, Expr)) -> Self {
        let (op, expr) = unary;
        Expr::Unary(op.clone(), Box::new(expr))
    }

    pub fn new_binary(binary: (Expr, &Token, Expr)) -> Self {
        let (left, op, right) = binary;
        Expr::Binary(Box::new(left), op.clone(), Box::new(right))
    }

    pub fn new_grouping(expr: Expr) -> Self {
        Expr::Grouping(Box::new(expr))
    }

    pub fn new_error() -> Self {
        Expr::Error
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expr::Literal(expr) => write!(f, "{}", expr),
            Expr::Unary(op, expr) => write!(f, "({} {})", op, expr),
            Expr::Binary(right, op, left) => write!(f, "({} {} {})", op, right, left),
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
            Expr::Error => write!(f, "error"),
        }
    }
}
