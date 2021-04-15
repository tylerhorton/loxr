use super::input::TokenSpan;
use crate::data::ast::Expr;
use crate::data::token::{Token, TokenKind};
use crate::error::Error;
use nom::{
    branch::alt,
    combinator::{map, value, verify},
    error::{Error as NomError, ErrorKind as NomErrorKind},
    multi::many0,
    sequence::{delimited, pair},
    Err, IResult,
};
use std::cell::RefCell;

pub fn parse(tokens: &[Token]) -> (Expr, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let input = TokenSpan::new(tokens, &errors);

    let (_, expr) = expr(input).expect("parser cannot fail");

    (expr, errors.into_inner())
}

type ParseRes<'a> = IResult<TokenSpan<'a>, Expr>;

fn expr(input: TokenSpan) -> ParseRes {
    alt((equality, error))(input)
}

fn any_token(input: TokenSpan) -> IResult<TokenSpan, &Token> {
    let mut index = 0;

    while let Some(token) = input.tokens.get(index) {
        index += 1;
        match token.kind {
            TokenKind::Comment(_) => continue,
            _ => return Ok((input.index(index..), token)),
        }
    }

    Err(Err::Error(NomError::new(input, NomErrorKind::Eof)))
}

macro_rules! token_kind (
	($($p:pat),+ $(,)?) => ({
        |input| {
            let is_kind = |t: &Token| match t.kind {
                $($p)|* => true,
                _ => false,
            };
            verify(any_token, is_kind)(input)
        }
	});
);

fn error(input: TokenSpan) -> ParseRes {
    value(Expr::Error, any_token)(input)
}

fn equality(input: TokenSpan) -> ParseRes {
    let ops = token_kind!(TokenKind::BangEqual, TokenKind::EqualEqual);

    let (input, mut left) = comparison(input)?;
    let (input, pairs) = many0(pair(ops, comparison))(input)?;

    for (op, right) in pairs {
        left = Expr::new_binary((left, op, right));
    }

    Ok((input, left))
}

fn comparison(input: TokenSpan) -> ParseRes {
    let ops = token_kind!(
        TokenKind::Greater,
        TokenKind::GreaterEqual,
        TokenKind::Less,
        TokenKind::LessEqual,
    );

    let (input, mut left) = term(input)?;
    let (input, pairs) = many0(pair(ops, term))(input)?;

    for (op, right) in pairs {
        left = Expr::new_binary((left, op, right));
    }

    Ok((input, left))
}

fn term(input: TokenSpan) -> ParseRes {
    let ops = token_kind!(TokenKind::Minus, TokenKind::Plus);

    let (input, mut left) = factor(input)?;
    let (input, pairs) = many0(pair(ops, factor))(input)?;

    for (op, right) in pairs {
        left = Expr::new_binary((left, op, right));
    }

    Ok((input, left))
}

fn factor(input: TokenSpan) -> ParseRes {
    let ops = token_kind!(TokenKind::Slash, TokenKind::Star);

    let (input, mut left) = unary(input)?;
    let (input, pairs) = many0(pair(ops, unary))(input)?;

    for (op, right) in pairs {
        left = Expr::new_binary((left, op, right));
    }

    Ok((input, left))
}

fn unary(input: TokenSpan) -> ParseRes {
    let ops = token_kind!(TokenKind::Bang, TokenKind::Minus);
    let unary_expr = map(pair(ops, unary), Expr::new_unary);

    alt((unary_expr, primary))(input)
}

fn primary(input: TokenSpan) -> ParseRes {
    alt((literal, grouping))(input)
}

fn literal(input: TokenSpan) -> ParseRes {
    let literal_token = token_kind!(
        TokenKind::False,
        TokenKind::True,
        TokenKind::Nil,
        TokenKind::Number(_),
        TokenKind::String(_),
    );

    map(literal_token, Expr::new_literal)(input)
}

fn grouping(input: TokenSpan) -> ParseRes {
    map(
        delimited(
            token_kind!(TokenKind::LeftParen),
            expr,
            token_kind!(TokenKind::RightParen),
        ),
        Expr::new_grouping,
    )(input)
}
