use crate::error::{Error, ReportError};
use crate::location::{Location, ToLocation};
use nom_locate::LocatedSpan;
use std::cell::RefCell;

/// Input type for the token parsers that tracks position information.
pub type Span<'a> = LocatedSpan<&'a str, &'a RefCell<Vec<Error>>>;

impl<'a> ToLocation for Span<'a> {
    fn to_location(&self) -> Location {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        Location::new(self.location_line() as usize, self.get_column(), start..end)
    }
}

impl<'a> ReportError for Span<'a> {
    fn report_error(&self, error: Error) {
        self.extra.borrow_mut().push(error);
    }
}
