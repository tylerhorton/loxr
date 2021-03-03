use crate::error::LoxError;
use crate::token::{get_tokens, Span, Token, TokenType};

pub fn lex<'a>(source: &str) -> Result<Vec<Token>, LoxError> {
    let (span, mut tokens) = get_tokens(Span::new(source))?;
    tokens.push(Token::new(TokenType::Eof, span));
    Ok(tokens)
}
