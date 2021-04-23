use nom::{InputLength, Offset, Slice};

use crate::data::token::Token;
use crate::error::{Error, ReportError};
use crate::location::{Location, ToLocation};
use std::{cell::RefCell, fmt, ops::RangeBounds};

#[derive(Debug, Clone, PartialEq)]
pub struct TokenSpan<'a> {
    pub tokens: &'a [Token],
    pub errors: &'a RefCell<Vec<Error>>,
}

impl<'a> TokenSpan<'a> {
    pub fn new(tokens: &'a [Token], errors: &'a RefCell<Vec<Error>>) -> Self {
        TokenSpan { tokens, errors }
    }
}

impl<'a> fmt::Display for TokenSpan<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for t in self.tokens {
            if first {
                first = false;
                write!(f, "{}", t)?;
            } else {
                write!(f, ", {}", t)?;
            }
        }
        Ok(())
    }
}

impl<'a> InputLength for TokenSpan<'a> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a> Offset for TokenSpan<'a> {
    fn offset(&self, second: &Self) -> usize {
        let fst = self.to_location().range.start;
        let snd = second.to_location().range.start;
        snd - fst
    }
}

impl<'a, R> Slice<R> for TokenSpan<'a>
where
    R: RangeBounds<usize> + std::slice::SliceIndex<[Token], Output = [Token]>,
{
    fn slice(&self, range: R) -> Self {
        TokenSpan::new(&self.tokens[range], self.errors)
    }
}

impl ToLocation for [Token] {
    fn to_location(&self) -> Location {
        match (self.first(), self.last()) {
            (Some(first), Some(last)) => Location::new(
                first.location.line,
                first.location.column,
                first.location.range.start..last.location.range.end,
            ),
            _ => Location::default(),
        }
    }
}

impl ToLocation for [&Token] {
    fn to_location(&self) -> Location {
        self.to_owned().to_location()
    }
}

impl<'a> ToLocation for TokenSpan<'a> {
    fn to_location(&self) -> Location {
        self.tokens.to_location()
    }
}

impl<'a> ReportError for TokenSpan<'a> {
    fn report_error(&self, error: Error) {
        self.errors.borrow_mut().push(error);
    }
}
