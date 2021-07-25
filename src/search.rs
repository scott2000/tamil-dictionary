use std::collections::{HashSet, BTreeMap, BTreeSet};

use thiserror::Error;

use crate::dictionary::{ENTRIES, NO_WORD, Entry, EntryIndex, WordIndex, Loc, Word, LetterSet};

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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum SearchPrecedence {
    Top,
    A,
    B,
    C,
    Bottom,
}

#[derive(Debug)]
struct SearchResultEntry {
    precedence: SearchPrecedence,
    words: BTreeSet<WordIndex>,
}

impl SearchResultEntry {
    fn new(word: WordIndex, start: bool, end: bool) -> Self {
        let precedence = if word == NO_WORD {
            match (start, end) {
                (true, true) => SearchPrecedence::Top,
                (true, false) => SearchPrecedence::A,
                (false, true) => SearchPrecedence::B,
                (false, false) => SearchPrecedence::Bottom,
            }
        } else {
            match (start, end) {
                (true, true) => SearchPrecedence::A,
                (true, false) => SearchPrecedence::B,
                (false, true) => SearchPrecedence::C,
                (false, false) => SearchPrecedence::Bottom,
            }
        };

        let mut words = BTreeSet::new();
        words.insert(word);

        Self {
            precedence,
            words,
        }
    }

    fn word_match(&self) -> bool {
        self.words.contains(&NO_WORD)
    }

    fn append(&mut self, mut other: Self) {
        self.precedence = match (self.word_match(), other.word_match()) {
            (true, true) => self.precedence.min(other.precedence),
            (true, false) => self.precedence,
            (false, true) => other.precedence,
            (false, false) => self.precedence.max(other.precedence),
        };

        self.words.append(&mut other.words);
    }
}

#[derive(Default, Debug)]
pub struct SearchResult {
    map: BTreeMap<EntryIndex, SearchResultEntry>,
}

impl SearchResult {
    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn insert(&mut self, loc: Loc, start: bool, end: bool) {
        self.insert_entry(loc.entry, SearchResultEntry::new(loc.word, start, end));
    }

    fn insert_entry(&mut self, index: EntryIndex, entry: SearchResultEntry) {
        if let Some(existing) = self.map.get_mut(&index) {
            existing.append(entry);
        } else {
            self.map.insert(index, entry);
        }
    }

    fn entry_intersection(&self, set: &mut HashSet<EntryIndex>) {
        set.retain(|entry| self.map.contains_key(entry));
    }

    pub fn intersect_difference(intersect: Vec<Self>, difference: Vec<Self>) -> Self {
        assert!(!intersect.is_empty());

        if intersect.len() == 1 && difference.is_empty() {
            return intersect.into_iter().next().unwrap()
        }

        let mut intersect_set = intersect[0].map.keys().cloned().collect();
        for i in 1..intersect.len() {
            intersect[i].entry_intersection(&mut intersect_set);
        }

        let difference_set: HashSet<_> = difference.iter()
            .flat_map(|result| result.map.keys().cloned())
            .collect();

        let mut intersect_difference = Self::default();
        for result in intersect {
            for (index, entry) in result.map {
                if intersect_set.contains(&index) && !difference_set.contains(&index) {
                    intersect_difference.insert_entry(index, entry);
                }
            }
        }

        intersect_difference
    }

    pub fn rank(self) -> SearchRanking {
        use SearchRank::*;
        use SearchPrecedence::*;

        let mut has_a = false;
        let mut has_b = false;
        let mut has_c_or_top = false;
        for entry in self.map.values() {
            match entry.precedence {
                Top => has_c_or_top = true,
                A => has_a = true,
                B => has_b = true,
                C => has_c_or_top = true,
                _ => {},
            }
        }

        let (a, b, c, other) = match (has_a, has_b, has_c_or_top) {
            (true, true, _) => (Best, Related, Other, Other),
            (true, false, _) => (Best, Ignore, Related, Other),
            (false, true, _) => (Ignore, Best, Related, Other),
            (false, false, true) => (Ignore, Ignore, Best, Other),
            (false, false, false) => (Ignore, Ignore, Ignore, Best),
        };

        let mut ranking = SearchRanking::default();
        for (index, entry) in self.map {
            let rank = match entry.precedence {
                Top => Exact,
                A => a,
                B => b,
                C => c,
                Bottom => other,
            };

            ranking.insert(rank, index, entry.words);
        }

        ranking
    }
}

#[derive(Copy, Clone, Debug)]
enum SearchRank {
    Exact,
    Best,
    Related,
    Other,
    Ignore,
}

#[derive(Debug)]
pub struct SearchRankingEntry {
    pub entry: &'static Entry,
    pub words: BTreeSet<WordIndex>,
}

#[derive(Default, Debug)]
pub struct SearchRanking {
    pub exact: Vec<SearchRankingEntry>,
    pub best: Vec<SearchRankingEntry>,
    pub related: Vec<SearchRankingEntry>,
    pub other: Vec<SearchRankingEntry>,
}

impl SearchRanking {
    pub fn is_empty(&self) -> bool {
        self.exact.is_empty()
            && self.best.is_empty()
            && self.related.is_empty()
            && self.other.is_empty()
    }

    fn insert(&mut self, rank: SearchRank, index: EntryIndex, words: BTreeSet<WordIndex>) {
        use SearchRank::*;

        let entry = SearchRankingEntry {
            entry: &ENTRIES[index as usize],
            words,
        };

        match rank {
            Exact => self.exact.push(entry),
            Best => self.best.push(entry),
            Related => self.related.push(entry),
            Other => self.other.push(entry),
            Ignore => {},
        }
    }
}
