use crate::token::Token;
use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Expr<'a> {
    // token
    Literal(&'a Token<'a>),

    // op, expr
    Unary(&'a Token<'a>, Box<Expr<'a>>),

    /// left, op, right
    Binary(Box<Expr<'a>>, &'a Token<'a>, Box<Expr<'a>>),

    // (, expr, )
    Grouping(Box<Expr<'a>>),
}

impl<'a> Expr<'a> {
    pub fn new_literal(token: &'a Token) -> Self {
        Expr::Literal(token)
    }

    pub fn new_unary(unary: (&'a Token, Expr<'a>)) -> Self {
        let (op, expr) = unary;
        Expr::Unary(op, Box::new(expr))
    }

    pub fn new_binary(binary: (Expr<'a>, &'a Token, Expr<'a>)) -> Self {
        let (left, op, right) = binary;
        Expr::Binary(Box::new(left), op, Box::new(right))
    }

    pub fn new_grouping(expr: Expr<'a>) -> Self {
        Expr::Grouping(Box::new(expr))
    }
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expr::Literal(expr) => write!(f, "{}", expr),
            Expr::Unary(op, expr) => write!(f, "({} {})", op, expr),
            Expr::Binary(right, op, left) => write!(f, "({} {} {})", op, right, left),
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
        }
    }
}
