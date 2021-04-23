use crate::location::{Location, ToLocation};
use nom::IResult;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub location: Location,
    pub message: String,
}

impl Error {
    pub fn new(location: Location, message: String) -> Self {
        Error { location, message }
    }
}

pub trait ReportError {
    fn report_error(&self, error: Error);
}

pub fn expect<'a, I, F, E, T>(mut parser: F, error: E) -> impl FnMut(I) -> IResult<I, Option<T>>
where
    I: ReportError + ToLocation + 'a,
    F: FnMut(I) -> IResult<I, T>,
    E: ToString,
{
    move |input| match parser(input) {
        Ok((remaining, out)) => Ok((remaining, Some(out))),
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            let err = Error::new(e.input.to_location(), error.to_string());
            e.input.report_error(err);
            Ok((e.input, None))
        }
        Err(err) => Err(err),
    }
}
