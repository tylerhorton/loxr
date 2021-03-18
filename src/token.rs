use nom_locate::LocatedSpan;
use phf::phf_map;
use std::fmt;

/// Input type for the token parsers that tracks position information.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Token definition
#[derive(PartialEq, Clone, Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub span: Span<'a>,
}

impl<'a> Token<'a> {
    /// Create a new token.
    pub fn new(kind: TokenKind, span: Span<'a>) -> Self {
        Token { kind, span }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
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

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrace => write!(f, "["),
            TokenKind::RightBrace => write!(f, "]"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Identifier(s) => write!(f, "{}", s),
            TokenKind::String(s) => write!(f, "{}", s),
            TokenKind::Number(n) => write!(f, "{}", n),
            TokenKind::And => write!(f, "and"),
            TokenKind::Class => write!(f, "class"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Fun => write!(f, "fun"),
            TokenKind::For => write!(f, "for"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Nil => write!(f, "nil"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Print => write!(f, "print"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Super => write!(f, "super"),
            TokenKind::This => write!(f, "this"),
            TokenKind::True => write!(f, "true"),
            TokenKind::Var => write!(f, "var"),
            TokenKind::While => write!(f, "while"),
            TokenKind::Comment(s) => write!(f, "{}", s),
            TokenKind::Eof => write!(f, ""),
        }
    }
}

/// Map for the single character tokens.
pub static CHARACTER_SEQUENCES: phf::Map<&'static str, TokenKind> = phf_map! {
    "(" => TokenKind::LeftParen,
    ")" => TokenKind::RightParen,
    "[" => TokenKind::LeftBrace,
    "]" => TokenKind::RightBrace,
    "," => TokenKind::Comma,
    "." => TokenKind::Dot,
    "+" => TokenKind::Plus,
    "-" => TokenKind::Minus,
    ";" => TokenKind::Semicolon,
    "/" => TokenKind::Slash,
    "*" => TokenKind::Star,
    "!" => TokenKind::Bang,
    "!=" => TokenKind::BangEqual,
    "=" => TokenKind::Equal,
    "==" => TokenKind::EqualEqual,
    ">" => TokenKind::Greater,
    ">=" => TokenKind::GreaterEqual,
    "<" => TokenKind::Less,
    "<=" => TokenKind::LessEqual,
};

/// Map for the reserved keywords.
pub static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "and" => TokenKind::And,
    "class" => TokenKind::Class,
    "else" => TokenKind::Else,
    "false" => TokenKind::False,
    "fun" => TokenKind::Fun,
    "for" => TokenKind::For,
    "if" => TokenKind::If,
    "nil" => TokenKind::Nil,
    "or" => TokenKind::Or,
    "print" => TokenKind::Print,
    "return" => TokenKind::Return,
    "super" => TokenKind::Super,
    "this" => TokenKind::This,
    "true" => TokenKind::True,
    "var" => TokenKind::Var,
    "while" => TokenKind::While,
};
