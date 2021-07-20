use std::collections::HashSet;

use thiserror::Error;

use crate::dictionary::{ENTRIES, Entry, Word, LetterSet};

pub mod tree;

#[derive(Error, Debug)]
pub enum SearchError {
    #[error("The search query is too complex. Try using fewer wildcards.")]
    TooComplex,
    #[error("The search query isn't specific enough.")]
    TooManyResults,
    #[error("An excluded word in the search is too common to be excluded.")]
    CommonExclusion,
    #[error("No results found. Try again as a definition.")]
    TryDefinition,
}

pub trait Search: Clone {
    fn is_empty(&self) -> bool;

    fn asserting_start(&self) -> Self;

    fn asserting_end(&self) -> Self;

    fn literal(&self, word: &Word) -> Self;

    fn matching(&self, lts: LetterSet) -> Result<Self, SearchError>;

    fn join(&mut self, other: &Self) -> Result<(), SearchError>;

    fn joining(mut self, other: &Self) -> Result<Self, SearchError> {
        self.join(other)?;
        Ok(self)
    }

    fn end(self) -> Result<SearchResult, SearchError>;
}

#[derive(Default, Debug)]
pub struct SearchResult {
    pub exact: HashSet<u32>,
    pub prefix: HashSet<u32>,
    pub affix: HashSet<u32>,
    pub other: HashSet<u32>,
}

impl SearchResult {
    pub fn normalize(&mut self) {
        let exact = &self.exact;
        self.prefix.retain(|i| !exact.contains(i));
        let prefix = &self.prefix;
        self.affix.retain(|i| !exact.contains(i) && !prefix.contains(i));
        let affix = &self.affix;
        self.other.retain(|i| !exact.contains(i) && !prefix.contains(i) && !affix.contains(i));
    }

    pub fn contains(&self, i: &u32) -> bool {
        self.exact.contains(i)
            || self.prefix.contains(i)
            || self.affix.contains(i)
            || self.other.contains(i)
    }

    pub fn is_empty(&self) -> bool {
        self.exact.is_empty()
            && self.prefix.is_empty()
            && self.affix.is_empty()
            && self.other.is_empty()
    }

    pub fn len(&self) -> usize {
        self.exact.len()
            + self.prefix.len()
            + self.affix.len()
            + self.other.len()
    }

    pub fn prefix_to_affix(mut self) -> Self {
        self.affix = self.affix.union(&self.prefix).cloned().collect();
        self.prefix = HashSet::new();
        self
    }

    pub fn exact(&self) -> Vec<&'static Entry> {
        Self::matches(self.exact.iter())
    }

    pub fn prefix(&self) -> Vec<&'static Entry> {
        Self::matches(self.prefix.iter())
    }

    pub fn affix(&self) -> Vec<&'static Entry> {
        Self::matches(self.affix.iter())
    }

    pub fn other(&self) -> Vec<&'static Entry> {
        Self::matches(self.other.iter())
    }

    fn all(&self) -> impl Iterator<Item = &u32> {
        self.exact.iter()
            .chain(&self.prefix)
            .chain(&self.affix)
            .chain(&self.other)
    }

    pub fn intersect_difference(intersect: Vec<Self>, difference: Vec<Self>) -> Self {
        assert!(!intersect.is_empty());
        if intersect.len() == 1 && difference.is_empty() {
            return intersect.into_iter().next().unwrap();
        }

        let mut intersect_set: HashSet<u32> = intersect[0]
            .all()
            .cloned()
            .collect();

        for result in &intersect[1..] {
            intersect_set.retain(|i| result.contains(i));
        }

        let difference_set: HashSet<u32> = difference.iter()
            .flat_map(Self::all)
            .cloned()
            .collect();

        let remove = |count: &mut u32, set: &mut HashSet<u32>| {
            set.retain(|i| intersect_set.contains(i) && !difference_set.contains(i));
            if !set.is_empty() {
                *count += 1;
            }
        };

        let mut skipped = Self::default();
        for mut result in intersect {
            let mut count = 0;
            remove(&mut count, &mut result.exact);
            remove(&mut count, &mut result.prefix);
            remove(&mut count, &mut result.affix);
            remove(&mut count, &mut result.other);

            match count {
                0 => break,
                1 => skipped = result,
                _ => return result,
            }
        }

        skipped
    }

    fn matches<'a>(iter: impl Iterator<Item = &'a u32>) -> Vec<&'static Entry> {
        let mut vec: Vec<_> = iter
            .map(|&i| &ENTRIES[i as usize])
            .collect();
        vec.sort();
        vec
    }
}
