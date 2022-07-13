use std::convert::Infallible;

use crate::tamil::{Letter, LetterSet, Word};

use super::Search;

#[derive(Clone, Debug)]
pub struct WordSearch<'a> {
    word: &'a Word,
    branches: Vec<usize>,
}

impl<'a> WordSearch<'a> {
    pub fn new(word: &'a Word) -> Self {
        Self {
            word,
            branches: vec![0],
        }
    }
}

impl<'a> Search for WordSearch<'a> {
    type Output = bool;
    type Error = Infallible;

    fn is_empty(&self) -> bool {
        self.branches.is_empty()
    }

    fn clearing(&self) -> Self {
        Self {
            word: Word::new(),
            branches: Vec::new(),
        }
    }

    fn asserting_start(&self) -> Self {
        let branches = self
            .branches
            .iter()
            .copied()
            .filter(|&branch| branch == 0)
            .collect();

        Self { branches, ..*self }
    }

    fn asserting_middle(&self) -> Self {
        let branches = self
            .branches
            .iter()
            .copied()
            .filter(|&branch| branch != 0 && branch != self.word.len())
            .collect();

        Self { branches, ..*self }
    }

    fn asserting_end(&self) -> Self {
        let branches = self
            .branches
            .iter()
            .copied()
            .filter(|&branch| branch == self.word.len())
            .collect();

        Self { branches, ..*self }
    }

    fn asserting_next(&self, lts: LetterSet) -> Self {
        let branches = self
            .branches
            .iter()
            .copied()
            .filter(|&branch| {
                self.word
                    .get(branch)
                    .map(|lt| lts.matches(lt))
                    .unwrap_or(false)
            })
            .collect();

        Self { branches, ..*self }
    }

    fn asserting_prev(&self, lt: Letter) -> Self {
        self.asserting_prev_matching(letterset![lt]).unwrap()
    }

    fn asserting_prev_matching(&self, lts: LetterSet) -> Result<Self, Self::Error> {
        let branches = self
            .branches
            .iter()
            .copied()
            .filter(|&branch| {
                branch
                    .checked_sub(1)
                    .and_then(|branch| self.word.get(branch))
                    .map(|lt| lts.matches(lt))
                    .unwrap_or(false)
            })
            .collect();

        Ok(Self { branches, ..*self })
    }

    fn literal(&self, word: &Word) -> Self {
        word.iter().fold(self.clone(), |search, lt| {
            search.matching(letterset![lt]).unwrap()
        })
    }

    fn matching(&self, lts: LetterSet) -> Result<Self, Self::Error> {
        let branches = self
            .branches
            .iter()
            .copied()
            .filter_map(|branch| {
                self.word
                    .get(branch)
                    .and_then(|lt| lts.matches(lt).then(|| branch + 1))
            })
            .collect();

        Ok(Self { branches, ..*self })
    }

    fn join(&mut self, other: &Self) -> Result<(), Self::Error> {
        if self.is_empty() {
            *self = other.clone();
        } else if !other.is_empty() {
            assert_eq!(self.word, other.word);
            self.branches.extend_from_slice(&other.branches);
        }

        Ok(())
    }

    fn marking_expanded(mut self) -> Self {
        self.branches.clear();
        self
    }

    fn freezing(mut self) -> Self {
        self.branches.clear();
        self
    }

    fn end(self) -> Result<Self::Output, Self::Error> {
        Ok(self.branches.contains(&self.word.len()))
    }
}
