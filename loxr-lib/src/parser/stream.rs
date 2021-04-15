use crate::data::token::Token;
use crate::location::{Location, ToLocation};
use std::ops::RangeBounds;
// use nom::{
//     error::ParseError, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Slice,
// };
// use nom::{InputIter, InputLength, InputTake, Slice};
// use std::{
//     iter::Enumerate,
//     ops::{RangeFrom, RangeTo},
// };

#[derive(Debug, Clone, PartialEq)]
pub struct TokenStream<'a, X: Clone = ()> {
    pub tokens: &'a [Token],
    pub extra: X,
}

impl<'a, X: Clone> TokenStream<'a, X> {
    pub fn new(tokens: &'a [Token], extra: X) -> Self {
        TokenStream { tokens, extra }
    }

    pub fn index<R>(&self, range: R) -> Self
    where
        R: RangeBounds<usize> + std::slice::SliceIndex<[Token], Output = [Token]>,
    {
        TokenStream::new(&self.tokens[range], self.extra.clone())
    }
}

impl<'a, X: Clone> ToLocation for TokenStream<'a, X> {
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

// impl<'a, X> InputIter for TokenStream<'a, X> {
//     type Item = &'a Token;
//     type Iter = Enumerate<Self::IterElem>;
//     type IterElem = std::slice::Iter<'a, Token>;

//     #[inline]
//     fn iter_indices(&self) -> Self::Iter {
//         self.tokens.iter().enumerate()
//     }

//     #[inline]
//     fn iter_elements(&self) -> Self::IterElem {
//         self.tokens.iter()
//     }

//     #[inline]
//     fn position<P>(&self, predicate: P) -> Option<usize>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         self.tokens.iter().position(predicate)
//     }

//     #[inline]
//     fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
//         match self.tokens.iter().nth(count) {
//             Some(t) => Ok(t.location.range.start),
//             None => Err(nom::Needed::Unknown),
//         }
//     }
// }

// impl<'a, X> InputLength for TokenStream<'a, X> {
//     fn input_len(&self) -> usize {
//         self.tokens.len()
//     }
// }

// impl<'a, X> InputTake for TokenStream<'a, X>
// where
//     Self: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
// {
//     fn take(&self, count: usize) -> Self {
//         self.tokens[..count]
//     }

//     fn take_split(&self, count: usize) -> (Self, Self) {
//         (self.tokens[count..], self.tokens[..count])
//     }
// }

// impl<'a, X> InputTakeAtPosition for TokenStream<'a, X>
// where
//     T: InputTakeAtPosition + InputLength + InputIter,
//     Self: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Clone,
// {
//     type Item = <T as InputIter>::Item;

//     fn split_at_position_complete<P, E: ParseError<Self>>(
//         &self,
//         predicate: P,
//     ) -> IResult<Self, Self, E>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         match self.split_at_position(predicate) {
//             Err(Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
//             res => res,
//         }
//     }

//     fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         match self.fragment.position(predicate) {
//             Some(n) => Ok(self.take_split(n)),
//             None => Err(Err::Incomplete(nom::Needed::new(1))),
//         }
//     }

//     fn split_at_position1<P, E: ParseError<Self>>(
//         &self,
//         predicate: P,
//         e: ErrorKind,
//     ) -> IResult<Self, Self, E>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         match self.fragment.position(predicate) {
//             Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
//             Some(n) => Ok(self.take_split(n)),
//             None => Err(Err::Incomplete(nom::Needed::new(1))),
//         }
//     }

//     fn split_at_position1_complete<P, E: ParseError<Self>>(
//         &self,
//         predicate: P,
//         e: ErrorKind,
//     ) -> IResult<Self, Self, E>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         match self.fragment.position(predicate) {
//             Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
//             Some(n) => Ok(self.take_split(n)),
//             None => {
//                 if self.fragment.input_len() == 0 {
//                     Err(Err::Error(E::from_error_kind(self.clone(), e)))
//                 } else {
//                     Ok(self.take_split(self.input_len()))
//                 }
//             }
//         }
//     }
// }
