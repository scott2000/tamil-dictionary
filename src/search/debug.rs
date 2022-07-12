use std::convert::Infallible;
use std::fmt::Display;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::tamil::{Letter, LetterSet, Word};

use super::Search;

#[derive(Clone)]
pub struct DebugSearch(Rc<AtomicUsize>, usize);

impl DebugSearch {
    pub fn start() -> Self {
        eprintln!("0 = input");
        Self(Rc::new(AtomicUsize::new(1)), 0)
    }

    fn method(&self, name: &'static str, rhs: impl Display) -> Self {
        let Self(max, id) = self;
        let new_id = max.fetch_add(1, Ordering::SeqCst);
        eprintln!("{new_id} = {id}.{name}({rhs})");
        Self(Rc::clone(max), new_id)
    }
}

impl Search for DebugSearch {
    type Output = usize;
    type Error = Infallible;

    fn is_empty(&self) -> bool {
        false
    }

    fn clearing(&self) -> Self {
        let Self(max, _) = self;
        let new_id = max.fetch_add(1, Ordering::SeqCst);
        eprintln!("{new_id} = empty");
        Self(Rc::clone(max), new_id)
    }

    fn asserting_start(&self) -> Self {
        self.method("asserting_start", "")
    }

    fn asserting_middle(&self) -> Self {
        self.method("asserting_middle", "")
    }

    fn asserting_end(&self) -> Self {
        self.method("asserting_end", "")
    }

    fn asserting_next(&self, lts: LetterSet) -> Self {
        self.method("asserting_next", lts)
    }

    fn asserting_prev(&self, lt: Letter) -> Self {
        self.method("asserting_prev", lt)
    }

    fn asserting_prev_matching(&self, lts: LetterSet) -> Result<Self, Self::Error> {
        Ok(self.method("asserting_prev_matching", lts))
    }

    fn literal(&self, word: &Word) -> Self {
        self.method("literal", word)
    }

    fn matching(&self, lts: LetterSet) -> Result<Self, Self::Error> {
        Ok(self.method("matching", lts))
    }

    fn join(&mut self, other: &Self) -> Result<(), Self::Error> {
        *self = self.method("joining", other.1);
        Ok(())
    }

    fn marking_expanded(self) -> Self {
        self.method("marking_expanded", "")
    }

    fn freezing(self) -> Self {
        self.method("freezing", "")
    }

    fn end(self) -> Result<Self::Output, Self::Error> {
        let Self(_, id) = self;
        eprintln!("res = {id}");
        Ok(id)
    }
}
