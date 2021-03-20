use thiserror::Error;

#[derive(Error, Debug)]
pub enum LoxError {
    #[error("Invalid syntax: {details}")]
    SyntaxError { details: String },

    #[error("Unexpected characters: {details:?}")]
    UnexpectedCharacters { details: Vec<String> },
}
