use crate::error::LoxError;
use crate::token::{get_tokens, Span, Token, TokenKind};

pub fn lex(source: &str) -> Result<Vec<Token>, LoxError> {
    let (span, mut tokens) = get_tokens(Span::new(source))?;
    tokens.push(Token::new(TokenKind::Eof, span));
    Ok(tokens)
}
