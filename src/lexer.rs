use crate::error::LoxError;
use crate::token::{get_next_token, Span, Token, TokenType};

pub fn lex<'a>(source: &str) -> Result<Vec<Token>, LoxError> {
    let mut tokens = vec![];
    let mut span = Span::new(source);

    loop {
        let (s, t) = get_next_token(span)?;
        span = s;

        let eof = t.token_type == TokenType::Eof;

        tokens.push(t);

        if eof {
            break;
        }
    }

    // tokens.reverse();
    Ok(tokens)
}
