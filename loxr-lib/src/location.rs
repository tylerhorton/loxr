use std::ops::Range;

/// Location in a source file
#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub range: Range<usize>,
}

impl Location {
    pub fn new(line: usize, column: usize, range: Range<usize>) -> Self {
        Location {
            line,
            column,
            range,
        }
    }
}

impl Default for Location {
    fn default() -> Self {
        Location::new(0, 0, 0..0)
    }
}

pub trait ToLocation {
    fn to_location(&self) -> Location;
}
