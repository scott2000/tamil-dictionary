use std::convert::Infallible;
use std::fmt::{self, Display};

use crate::tamil::{Word, Letter, LetterSet};
use crate::search::Search;

#[derive(Clone, Debug)]
pub struct DebugSearch(String);

impl DebugSearch {
    pub fn new() -> Self {
        Self(format!("new()"))
    }
}

impl Search for DebugSearch {
    type Output = String;
    type Error = Infallible;

    fn empty() -> Self {
        Self(format!("empty()"))
    }

    fn is_empty(&self) -> bool {
        false
    }

    fn asserting_start(&self) -> Self {
        Self(format!("{}.asserting_start()", self))
    }

    fn asserting_middle(&self) -> Self {
        Self(format!("{}.asserting_middle()", self))
    }

    fn asserting_end(&self) -> Self {
        Self(format!("{}.asserting_end()", self))
    }

    fn asserting(&self, lts: LetterSet) -> Self {
        Self(format!("{}.asserting({})", self, lts))
    }

    fn literal(&self, word: &Word) -> Self {
        Self(format!("{}.literal({})", self, Letter::to_str(word)))
    }

    fn matching(&self, lts: LetterSet) -> Result<Self, Self::Error> {
        Ok(Self(format!("{}.matching({})", self, lts)))
    }

    fn join(&mut self, other: &Self) -> Result<(), Self::Error> {
        self.0 = format!("{}.joining({})", self, other);
        Ok(())
    }

    fn end(self) -> Result<Self::Output, Self::Error> {
        Ok(self.0)
    }
}

impl Display for DebugSearch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
