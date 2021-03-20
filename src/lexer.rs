use crate::error::LoxError;
use crate::token::{Span, Token, TokenKind, CHARACTER_SEQUENCES, KEYWORDS};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, anychar, char, multispace0, one_of},
    combinator::{eof, map, opt, peek, recognize, value},
    error::VerboseError,
    multi::{many0, many1, many_till},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use nom_locate::position;
use std::str::FromStr;

pub fn lex(source: &str) -> Result<Vec<Token>, LoxError> {
    let (_, tokens) = tokens(Span::new(source)).map_err(|e| LoxError::SyntaxError {
        details: e.to_string(),
    })?;

    let errors = tokens
        .iter()
        .filter(|t| match t.kind {
            TokenKind::UnexpectedCharacters(_) => true,
            _ => false,
        })
        .map(|t| t.to_string())
        .collect::<Vec<String>>();

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(LoxError::UnexpectedCharacters { details: errors })
    }
}

// Result type used by the token parsers.
type LexRes<'a, O> = IResult<Span<'a>, O, VerboseError<Span<'a>>>;

fn tokens(input: Span) -> LexRes<Vec<Token>> {
    map(many_till(token_or_unexpected, eof), |(tokens, _)| tokens)(input)
}

fn token_or_unexpected(input: Span) -> LexRes<Token> {
    let (input, pos) = position(input)?;

    // Handle unexpected characters just like any other token. After all
    // tokens have been lexed, a pass will be made through the tokens to
    // determine if any unexpected characters exist. If they do, the
    // interpreter will report the errors and not proceed to parsing.
    let (input, kind) = alt((token, unexpected_characters))(input)?;

    Ok((input, Token::new(kind, pos)))
}

fn token(input: Span) -> LexRes<TokenKind> {
    // Order of two character tokens before one character tokens is
    // important or else two character tokens will never be chosen.
    // Additionally, comments need to be before signal character tokens so
    // that "//" is chosen.
    ws(alt((
        comment,
        double_character_tokens,
        single_character_tokens,
        number,
        string,
        identifier_or_keyword,
    )))(input)
}

fn unexpected_characters(input: Span) -> LexRes<TokenKind> {
    // Take all of the unexpected characters until a valid token can be lexed or
    // until the end of file is reached.
    map(
        recognize(many_till(
            anychar,
            peek(alt((discard(token), discard(eof)))),
        )),
        |s| TokenKind::UnexpectedCharacters(s.fragment().to_string()),
    )(input)
}

fn double_character_tokens(input: Span) -> LexRes<TokenKind> {
    alt((
        character_sequence("!="),
        character_sequence("=="),
        character_sequence(">="),
        character_sequence("<="),
    ))(input)
}

fn single_character_tokens(input: Span) -> LexRes<TokenKind> {
    alt((
        character_sequence("!"),
        character_sequence("="),
        character_sequence(">"),
        character_sequence("<"),
        character_sequence("("),
        character_sequence(")"),
        character_sequence("["),
        character_sequence("]"),
        character_sequence(","),
        character_sequence("."),
        character_sequence("+"),
        character_sequence("-"),
        character_sequence(";"),
        character_sequence("/"),
        character_sequence("*"),
    ))(input)
}

fn ws<'a, O, F>(inner: F) -> impl FnMut(Span<'a>) -> LexRes<O>
where
    F: FnMut(Span<'a>) -> LexRes<O> + 'a,
{
    delimited(multispace0, inner, multispace0)
}

fn discard<'a, O, F>(parser: F) -> impl FnMut(Span<'a>) -> LexRes<()>
where
    F: FnMut(Span<'a>) -> LexRes<O> + 'a,
{
    value((), parser)
}

fn comment(input: Span) -> LexRes<TokenKind> {
    map(preceded(tag("//"), ws(is_not("\n\r"))), |s| {
        TokenKind::Comment(s.fragment().trim().to_string())
    })(input)
}

fn character_sequence<'a>(seq: &'static str) -> impl FnMut(Span<'a>) -> LexRes<TokenKind> {
    let kind = CHARACTER_SEQUENCES
        .get(seq)
        .expect("invalid character sequence given");
    value(kind.clone(), tag(seq))
}

fn decimal(input: Span) -> LexRes<Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

fn exponent(input: Span) -> LexRes<Span> {
    recognize(tuple((one_of("eE"), opt(one_of("+-")), decimal)))(input)
}

fn dot_number(input: Span) -> LexRes<Span> {
    // Case one: .42
    recognize(tuple((char('.'), decimal, opt(exponent))))(input)
}

fn exponent_number(input: Span) -> LexRes<Span> {
    // Case two: 42e42 and 42.42e42
    recognize(tuple((
        decimal,
        opt(preceded(char('.'), decimal)),
        exponent,
    )))(input)
}

fn number_dot_number(input: Span) -> LexRes<Span> {
    // Case three: 42. and 42.42
    recognize(tuple((decimal, char('.'), opt(decimal))))(input)
}

fn number(input: Span) -> LexRes<TokenKind> {
    map(
        alt((dot_number, exponent_number, number_dot_number, decimal)),
        // Conversion from string to f64 won't fail assuming parsing isn't buggy
        |s| TokenKind::Number(f64::from_str(s.fragment()).unwrap()),
    )(input)
}

fn string(input: Span) -> LexRes<TokenKind> {
    map(delimited(tag("\""), is_not("\""), tag("\"")), |s: Span| {
        TokenKind::String(s.fragment().to_string())
    })(input)
}

fn identifier_or_keyword(input: Span) -> LexRes<TokenKind> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: Span| match KEYWORDS.get(*s.fragment()).cloned() {
            Some(keyword) => keyword,
            None => TokenKind::Identifier(s.to_string()),
        },
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{
        error::{ErrorKind as EK, VerboseError as VE, VerboseErrorKind as VEK},
        Err as NE,
    };

    macro_rules! nom_err {
        ( $( $x:expr ),+ $(,)?) => {{
            NE::Error(VE {
                errors: vec![$($x),*]
            })
        }};
    }

    macro_rules! assert_approx_eq {
        ($a:expr, $b:expr) => {{
            let (a, b) = (&$a, &$b);
            assert!(
                (*a - *b).abs() < 1.0e-6,
                "{} is not approximately equal to {}",
                *a,
                *b
            );
        }};
    }

    macro_rules! inner_from_variant {
        ($value:expr, $variant:path) => {{
            let value = &$value;
            match value {
                $variant(val) => val,
                _ => panic!(
                    "Expected the {} variant from the enum",
                    stringify!($variant)
                ),
            }
        }};
    }

    #[test]
    fn test_comment() {
        let s = Span::new("// hello, world \n foo");
        let (s, tt) = comment(s).unwrap();
        assert_eq!(s.fragment(), &"foo");
        assert_eq!(inner_from_variant![tt, TokenKind::Comment], "hello, world");
    }

    #[test]
    fn test_single_character() {
        let s1 = Span::new("(foo)");
        let (s2, tt) = character_sequence("(")(s1).unwrap();
        assert_eq!(s2.fragment(), &"foo)");
        assert_eq!(tt, TokenKind::LeftParen);
        let err = character_sequence("(")(s2).unwrap_err();
        assert_eq!(err, nom_err![(s2, VEK::Nom(EK::Tag))]);
    }

    #[test]
    fn test_decimal() {
        let s1 = Span::new("1234");
        let (remaining, consumed) = decimal(s1).unwrap();
        assert_eq!(remaining.fragment(), &"");
        assert_eq!(consumed.fragment(), &"1234");
        let s2 = Span::new("1234_1234");
        let (remaining, consumed) = decimal(s2).unwrap();
        assert_eq!(remaining.fragment(), &"");
        assert_eq!(consumed.fragment(), &"1234_1234");
        let s3 = Span::new("1234_");
        let (remaining, consumed) = decimal(s3).unwrap();
        assert_eq!(remaining.fragment(), &"");
        assert_eq!(consumed.fragment(), &"1234_");
        let s4 = Span::new("_1234");
        let err = decimal(s4).unwrap_err();
        assert_eq!(
            err,
            nom_err![(s4, VEK::Nom(EK::OneOf)), (s4, VEK::Nom(EK::Many1))]
        );
        let s5 = Span::new("1234abc");
        let (remaining, consumed) = decimal(s5).unwrap();
        assert_eq!(remaining.fragment(), &"abc");
        assert_eq!(consumed.fragment(), &"1234");
    }

    #[test]
    fn test_number() {
        let s = Span::new(".10");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(0.1 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new("10e10");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10e10 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new("10.10e10");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10.10e10 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new("10.");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new("10");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new("10.10");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10.10 as f64, inner_from_variant![tt, TokenKind::Number]);
    }

    #[test]
    fn test_string() {
        let s = Span::new("\"hello, world\"");
        let (s, tt) = string(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_eq!(inner_from_variant![tt, TokenKind::String], "hello, world");
    }
}
