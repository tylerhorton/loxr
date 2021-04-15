use super::input::Span;
use crate::data::token::{Token, TokenKind, CHARACTER_SEQUENCES, KEYWORDS};
use crate::error::{expect, Error, ReportError};
use crate::location::ToLocation;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::complete::{alpha1, alphanumeric1, anychar, char, multispace0, one_of},
    combinator::{consumed, eof, map, opt, peek, recognize, value},
    multi::{many0, many1, many_till},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::cell::RefCell;
use std::str::FromStr;

pub fn lex(source: &str) -> (Vec<Token>, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let input = Span::new_extra(source, &errors);

    // Use expect here because the token lexer should be infallible. All error
    // cases are parsed the same as good tokens (into unexpected characters)
    let (_, tokens) = tokens(input).expect("lexer cannot fail");

    (tokens, errors.into_inner())
}

// Result type used by the token parsers.
type LexRes<'a, O> = IResult<Span<'a>, O>;

fn tokens(input: Span) -> LexRes<Vec<Token>> {
    map(many_till(token_or_unexpected, eof), |(tokens, _)| tokens)(input)
}

fn token_or_unexpected(input: Span) -> LexRes<Token> {
    // Handle unexpected errors just like any other token
    alt((token, unexpected_characters))(input)
}

fn token(input: Span) -> LexRes<Token> {
    // Order of two character tokens before one character tokens is
    // important or else two character tokens will never be chosen.
    // Additionally, comments need to be before signal character tokens so
    // that "//" is chosen.
    let (output, (span, kind)) = ws(consumed(alt((
        comment,
        double_character_tokens,
        single_character_tokens,
        number,
        string,
        identifier_or_keyword,
    ))))(input)?;

    Ok((output, Token::new(kind, span.to_location())))
}

fn unexpected_characters(input: Span) -> LexRes<Token> {
    // Take all of the unexpected characters until a valid token can be lexed or
    // until the end of file is reached.
    let chars_until_token = recognize(many_till(
        anychar,
        peek(alt((discard(token), discard(eof)))),
    ));

    let report_error = |s: Span| {
        let err = Error::new(
            s.to_location(),
            format!("unexpected characters: \"{}\"", s.fragment()),
        );
        s.report_error(err);
        Token::new(TokenKind::Error, s.to_location())
    };

    map(chars_until_token, report_error)(input)
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
        .expect("invalid character sequence");
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
    let string = tuple((
        tag("\""),
        opt(take_until("\"")),
        expect(tag("\""), "missing closing \'\"\'"),
    ));

    let check_string =
        |(_, content, closing): (_, Option<Span>, Option<Span>)| match (content, closing) {
            (Some(s), Some(_)) => TokenKind::String(s.fragment().to_string()),
            _ => TokenKind::Error,
        };

    map(string, check_string)(input)
}

fn identifier(input: Span) -> LexRes<Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn identifier_or_keyword(input: Span) -> LexRes<TokenKind> {
    let get_ident_or_keyword = |s: Span| match KEYWORDS.get(*s.fragment()).cloned() {
        Some(keyword) => keyword,
        None => TokenKind::Identifier(s.to_string()),
    };

    map(identifier, get_ident_or_keyword)(input)
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let errors = RefCell::new(Vec::new());
        let s = Span::new_extra("// hello, world \n foo", &errors);
        let (s, tt) = comment(s).unwrap();
        assert_eq!(s.fragment(), &"foo");
        assert_eq!(inner_from_variant![tt, TokenKind::Comment], "hello, world");
    }

    #[test]
    fn test_single_character() {
        let errors = RefCell::new(Vec::new());
        let s1 = Span::new_extra("(foo)", &errors);
        let (s2, tt) = character_sequence("(")(s1).unwrap();
        assert_eq!(s2.fragment(), &"foo)");
        assert_eq!(tt, TokenKind::LeftParen);
        let _ = character_sequence("(")(s2).unwrap_err();
    }

    #[test]
    fn test_decimal() {
        let errors = RefCell::new(Vec::new());
        let s1 = Span::new_extra("1234", &errors);
        let (remaining, consumed) = decimal(s1).unwrap();
        assert_eq!(remaining.fragment(), &"");
        assert_eq!(consumed.fragment(), &"1234");
        let s2 = Span::new_extra("1234_1234", &errors);
        let (remaining, consumed) = decimal(s2).unwrap();
        assert_eq!(remaining.fragment(), &"");
        assert_eq!(consumed.fragment(), &"1234_1234");
        let s3 = Span::new_extra("1234_", &errors);
        let (remaining, consumed) = decimal(s3).unwrap();
        assert_eq!(remaining.fragment(), &"");
        assert_eq!(consumed.fragment(), &"1234_");
        let s4 = Span::new_extra("_1234", &errors);
        let _ = decimal(s4).unwrap_err();
        let s5 = Span::new_extra("1234abc", &errors);
        let (remaining, consumed) = decimal(s5).unwrap();
        assert_eq!(remaining.fragment(), &"abc");
        assert_eq!(consumed.fragment(), &"1234");
    }

    #[test]
    fn test_number() {
        let errors = RefCell::new(Vec::new());
        let s = Span::new_extra(".10", &errors);
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(0.1 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new_extra("10e10", &errors);
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10e10 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new_extra("10.10e10", &errors);
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10.10e10 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new_extra("10.", &errors);
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new_extra("10", &errors);
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10 as f64, inner_from_variant![tt, TokenKind::Number]);
        let s = Span::new_extra("10.10", &errors);
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10.10 as f64, inner_from_variant![tt, TokenKind::Number]);
    }

    #[test]
    fn test_string() {
        let errors = RefCell::new(Vec::new());
        let s = Span::new_extra("\"hello, world\"", &errors);
        let (s, tt) = string(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_eq!(inner_from_variant![tt, TokenKind::String], "hello, world");
    }
}
