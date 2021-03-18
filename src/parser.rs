use crate::ast::Expr;
use crate::error::LoxError;
use crate::token::{Token, TokenKind};
use nom::{
    branch::alt,
    combinator::{map, verify},
    error::VerboseError,
    error::{ErrorKind as EK, VerboseError as VE, VerboseErrorKind as VEK},
    multi::many0,
    sequence::{delimited, pair},
    Err as NE, IResult,
};

type Res<I, O> = IResult<I, O, VerboseError<I>>;
type ParseRes<'a> = Res<&'a [Token<'a>], Expr<'a>>;

pub fn parse<'a>(tokens: &'a Vec<Token<'a>>) -> Result<Expr, LoxError> {
    expr(tokens)
        .map(|(_, expr)| expr)
        .map_err(|e| LoxError::SyntaxError {
            details: e.to_string(),
        })
}

macro_rules! nom_err {
    ( $( $x:expr ),+ $(,)?) => {{
        NE::Error(VE {
            errors: vec![$($x),*]
        })
    }};
}

fn expr<'a>(input: &'a [Token]) -> ParseRes<'a> {
    equality(input)
}

fn any_token<'a>(input: &'a [Token<'a>]) -> Res<&'a [Token], &'a Token> {
    let mut index = 0;

    while let Some(token) = input.get(index) {
        index += 1;
        match token.kind {
            TokenKind::Comment(_) => continue,
            _ => return Ok((&input[index..], token)),
        }
    }

    Err(nom_err![(input, VEK::Nom(EK::Tag))])
}

macro_rules! token_kind (
	($($p:pat),+ $(,)?) => ({
        |input| {
            let is_type = |t: &Token| match t.kind {
                $($p)|* => true,
                _ => false,
            };
            verify(any_token, is_type)(input)
        }
	});
);

fn equality<'a>(input: &'a [Token]) -> ParseRes<'a> {
    let ops = token_kind!(TokenKind::BangEqual, TokenKind::EqualEqual);

    let (input, mut left) = comparison(input)?;
    let (input, pairs) = many0(pair(ops, comparison))(input)?;

    for (op, right) in pairs {
        left = Expr::new_binary((left, op, right));
    }

    Ok((input, left))
}

fn comparison<'a>(input: &'a [Token]) -> ParseRes<'a> {
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

fn term<'a>(input: &'a [Token]) -> ParseRes<'a> {
    let ops = token_kind!(TokenKind::Minus, TokenKind::Plus);

    let (input, mut left) = factor(input)?;
    let (input, pairs) = many0(pair(ops, factor))(input)?;

    for (op, right) in pairs {
        left = Expr::new_binary((left, op, right));
    }

    Ok((input, left))
}

fn factor<'a>(input: &'a [Token]) -> ParseRes<'a> {
    let ops = token_kind!(TokenKind::Slash, TokenKind::Star);

    let (input, mut left) = unary(input)?;
    let (input, pairs) = many0(pair(ops, unary))(input)?;

    for (op, right) in pairs {
        left = Expr::new_binary((left, op, right));
    }

    Ok((input, left))
}

fn unary<'a>(input: &'a [Token]) -> ParseRes<'a> {
    let ops = token_kind!(TokenKind::Bang, TokenKind::Minus);
    let unary_expr = map(pair(ops, unary), Expr::new_unary);

    alt((unary_expr, primary))(input)
}

fn primary<'a>(input: &'a [Token]) -> ParseRes<'a> {
    alt((literal, grouping))(input)
}

fn literal<'a>(input: &'a [Token]) -> ParseRes<'a> {
    let literal_token = token_kind!(
        TokenKind::False,
        TokenKind::True,
        TokenKind::Nil,
        TokenKind::Number(_),
        TokenKind::String(_),
    );

    map(literal_token, Expr::new_literal)(input)
}

fn grouping<'a>(input: &'a [Token]) -> ParseRes<'a> {
    map(
        delimited(
            token_kind!(TokenKind::LeftParen),
            expr,
            token_kind!(TokenKind::RightParen),
        ),
        Expr::new_grouping,
    )(input)
}
