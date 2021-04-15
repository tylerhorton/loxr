use crate::data::token::Token;
use crate::error::{Error, ReportError};
use crate::location::{Location, ToLocation};
use std::{cell::RefCell, ops::RangeBounds};

#[derive(Debug, Clone, PartialEq)]
pub struct TokenSpan<'a> {
    pub tokens: &'a [Token],
    pub errors: &'a RefCell<Vec<Error>>,
}

impl<'a> TokenSpan<'a> {
    pub fn new(tokens: &'a [Token], errors: &'a RefCell<Vec<Error>>) -> Self {
        TokenSpan { tokens, errors }
    }

    pub fn index<R>(&self, range: R) -> Self
    where
        R: RangeBounds<usize> + std::slice::SliceIndex<[Token], Output = [Token]>,
    {
        TokenSpan::new(&self.tokens[range], self.errors)
    }
}

impl<'a> ToLocation for TokenSpan<'a> {
    fn to_location(&self) -> Location {
        match (self.tokens.first(), self.tokens.last()) {
            (Some(first), Some(last)) => Location::new(
                first.location.line,
                first.location.column,
                first.location.range.start..last.location.range.end,
            ),
            _ => Location::default(),
        }
    }
}

impl<'a> ReportError for TokenSpan<'a> {
    fn report_error(&self, error: Error) {
        self.errors.borrow_mut().push(error);
    }
}
