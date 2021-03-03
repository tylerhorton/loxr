use crate::error::LoxError;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, multispace0, one_of},
    combinator::{eof, map, opt, recognize, value},
    error::VerboseError,
    multi::{many0, many1, many_till},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use nom_locate::{position, LocatedSpan};
use phf::phf_map;
use std::str::FromStr;

/// Input type for the token parsers that tracks position information.
pub type Span<'a> = LocatedSpan<&'a str>;

// Result type used by the token parsers.
type Res<I, O> = IResult<I, O, VerboseError<I>>;

/// Token definition
#[derive(Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub span: Span<'a>,
}

impl<'a> Token<'a> {
    /// Create a new token.
    pub fn new(token_type: TokenType, span: Span<'a>) -> Self {
        Token { token_type, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Plus,
    Minus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier(String),
    String(String),
    Number(f64),
    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // Discardable tokens
    Comment(String),
    // End of file
    Eof,
}

/// Map for the single character tokens.
static CHARACTER_SEQUENCES: phf::Map<&'static str, TokenType> = phf_map! {
    "(" => TokenType::LeftParen,
    ")" => TokenType::RightParen,
    "[" => TokenType::LeftBrace,
    "]" => TokenType::RightBrace,
    "," => TokenType::Comma,
    "." => TokenType::Dot,
    "+" => TokenType::Plus,
    "-" => TokenType::Minus,
    ";" => TokenType::Semicolon,
    "/" => TokenType::Slash,
    "*" => TokenType::Star,
    "!" => TokenType::Bang,
    "!=" => TokenType::BangEqual,
    "=" => TokenType::Equal,
    "==" => TokenType::EqualEqual,
    ">" => TokenType::Greater,
    ">=" => TokenType::GreaterEqual,
    "<" => TokenType::Less,
    "<=" => TokenType::LessEqual,
};

/// Map for the reserved keywords.
static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "and" => TokenType::And,
    "class" => TokenType::Class,
    "else" => TokenType::Else,
    "false" => TokenType::False,
    "fun" => TokenType::Fun,
    "for" => TokenType::For,
    "if" => TokenType::If,
    "nil" => TokenType::Nil,
    "or" => TokenType::Or,
    "print" => TokenType::Print,
    "return" => TokenType::Return,
    "super" => TokenType::Super,
    "this" => TokenType::This,
    "true" => TokenType::True,
    "var" => TokenType::Var,
    "while" => TokenType::While,
};

pub fn get_tokens(s: Span) -> Result<(Span, Vec<Token>), LoxError> {
    tokens(s).map_err(|e| LoxError::SyntaxError {
        details: e.to_string(),
    })
}

fn tokens(s: Span) -> Res<Span, Vec<Token>> {
    map(many_till(token, eof), |(tokens, _)| tokens)(s)
}

fn token(s: Span) -> Res<Span, Token> {
    let (s, pos) = position(s)?;
    let (s, token_type) = ws(alt((
        // Order of two character tokens before one character tokens is
        // important or else two character tokens will never be chosen.
        // Additionally, comments need to be before signal character tokens so
        // that "//" is chosen.
        comment,
        double_character_tokens,
        single_character_tokens,
        number,
        string,
        identifier_or_keyword,
    )))(s)?;

    Ok((s, Token::new(token_type, pos)))
}

fn double_character_tokens(s: Span) -> Res<Span, TokenType> {
    alt((
        character_sequence("!="),
        character_sequence("=="),
        character_sequence(">="),
        character_sequence("<="),
    ))(s)
}

fn single_character_tokens(s: Span) -> Res<Span, TokenType> {
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
    ))(s)
}

fn ws<'a, O, F>(inner: F) -> impl FnMut(Span<'a>) -> Res<Span<'a>, O>
where
    F: FnMut(Span<'a>) -> Res<Span, O> + 'a,
{
    delimited(multispace0, inner, multispace0)
}

fn comment(input: Span) -> Res<Span, TokenType> {
    map(preceded(tag("//"), ws(is_not("\n\r"))), |s| {
        TokenType::Comment(s.fragment().trim().to_string())
    })(input)
}

fn character_sequence<'a>(seq: &'static str) -> impl FnMut(Span<'a>) -> Res<Span<'a>, TokenType> {
    let token_type = CHARACTER_SEQUENCES
        .get(seq)
        .expect("invalid character sequence given");
    value(token_type.clone(), tag(seq))
}

fn decimal(input: Span) -> Res<Span, Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

fn exponent(input: Span) -> Res<Span, Span> {
    recognize(tuple((one_of("eE"), opt(one_of("+-")), decimal)))(input)
}

fn dot_number(input: Span) -> Res<Span, Span> {
    // Case one: .42
    recognize(tuple((char('.'), decimal, opt(exponent))))(input)
}

fn exponent_number(input: Span) -> Res<Span, Span> {
    // Case two: 42e42 and 42.42e42
    recognize(tuple((
        decimal,
        opt(preceded(char('.'), decimal)),
        exponent,
    )))(input)
}

fn number_dot_number(input: Span) -> Res<Span, Span> {
    // Case three: 42. and 42.42
    recognize(tuple((decimal, char('.'), opt(decimal))))(input)
}

fn number(input: Span) -> Res<Span, TokenType> {
    map(
        alt((dot_number, exponent_number, number_dot_number)),
        // Conversion from string to f64 won't fail assuming parsing isn't buggy
        |s| TokenType::Number(f64::from_str(s.fragment()).unwrap()),
    )(input)
}

fn string(input: Span) -> Res<Span, TokenType> {
    map(delimited(tag("\""), is_not("\""), tag("\"")), |s: Span| {
        TokenType::String(s.fragment().to_string())
    })(input)
}

fn identifier_or_keyword(input: Span) -> Res<Span, TokenType> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: Span| match KEYWORDS.get(*s.fragment()).cloned() {
            Some(keyword) => keyword,
            None => TokenType::Identifier(s.to_string()),
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
        assert_eq!(inner_from_variant![tt, TokenType::Comment], "hello, world");
    }

    #[test]
    fn test_single_character() {
        let s1 = Span::new("(foo)");
        let (s2, tt) = character_sequence("(")(s1).unwrap();
        assert_eq!(s2.fragment(), &"foo)");
        assert_eq!(tt, TokenType::LeftParen);
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
        assert_approx_eq!(0.1 as f64, inner_from_variant![tt, TokenType::Number]);
        let s = Span::new("10e10");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10e10 as f64, inner_from_variant![tt, TokenType::Number]);
        let s = Span::new("10.10e10");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10.10e10 as f64, inner_from_variant![tt, TokenType::Number]);
        let s = Span::new("10.");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10 as f64, inner_from_variant![tt, TokenType::Number]);
        let s = Span::new("10.10");
        let (s, tt) = number(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_approx_eq!(10.10 as f64, inner_from_variant![tt, TokenType::Number]);
    }

    #[test]
    fn test_string() {
        let s = Span::new("\"hello, world\"");
        let (s, tt) = string(s).unwrap();
        assert_eq!(s.fragment(), &"");
        assert_eq!(inner_from_variant![tt, TokenType::String], "hello, world");
    }
}
