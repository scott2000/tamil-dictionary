use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::dictionary::{Entry, EntryIndex, Loc, WordData, WordIndex, ENTRIES, NO_WORD};
use crate::tamil::{Letter, LetterSet, Word};

pub use crate::dictionary::KindSet;

pub mod tree;
pub mod word;

pub trait Search: Clone {
    type Output;
    type Error;

    fn empty() -> Self;

    fn is_empty(&self) -> bool;

    fn asserting_start(&self) -> Self;

    fn asserting_middle(&self) -> Self;

    fn asserting_end(&self) -> Self;

    fn asserting_next(&self, lts: LetterSet) -> Self;

    fn asserting_prev(&self, lt: Letter) -> Self;

    fn asserting_prev_matching(&self, lts: LetterSet) -> Result<Self, Self::Error>;

    fn literal(&self, word: &Word) -> Self;

    fn matching(&self, lts: LetterSet) -> Result<Self, Self::Error>;

    fn join(&mut self, other: &Self) -> Result<(), Self::Error>;

    fn joining(mut self, other: &Self) -> Result<Self, Self::Error> {
        self.join(other)?;
        Ok(self)
    }

    fn mark_expanded(&mut self);

    fn marking_expanded(mut self) -> Self {
        self.mark_expanded();
        self
    }

    fn freeze(&mut self);

    fn freezing(mut self) -> Self {
        self.freeze();
        self
    }

    fn end(self) -> Result<Self::Output, Self::Error>;
}

pub trait Suggest {
    fn suggest(self, count: u32) -> SuggestionList;
}

pub struct SuggestionList {
    count_requested: u32,
    seen_leaves: HashSet<&'static str>,
    seen_branches: HashSet<&'static str>,
    seen_expanded: HashSet<&'static str>,
    from_leaves: BTreeSet<EntryIndex>,
    from_branches: BTreeSet<EntryIndex>,
    from_expanded: BTreeSet<EntryIndex>,
}

impl SuggestionList {
    pub fn new(count_requested: u32) -> Self {
        Self {
            count_requested,
            seen_leaves: HashSet::new(),
            seen_branches: HashSet::new(),
            seen_expanded: HashSet::new(),
            from_leaves: BTreeSet::new(),
            from_branches: BTreeSet::new(),
            from_expanded: BTreeSet::new(),
        }
    }

    pub fn suggestions(mut self) -> impl Iterator<Item = &'static Entry> {
        let mut extend_count =
            (self.count_requested as usize).saturating_sub(self.from_leaves.len());

        let expanded = if extend_count == 0 {
            Vec::new()
        } else {
            // Take some branches and add them to try to reach the requested count
            let to_add: Vec<_> = self
                .from_branches
                .difference(&self.from_leaves)
                .take(extend_count)
                .cloned()
                .collect();

            extend_count -= to_add.len();
            self.from_leaves.extend(to_add);

            // Add any left over expanded branches if there's room
            self.from_expanded
                .difference(&self.from_leaves)
                .take(extend_count)
                .cloned()
                .collect()
        };

        // Take the first suggestions alphabetically
        self.from_leaves
            .into_iter()
            .chain(expanded)
            .take(self.count_requested as usize)
            .map(|index| &ENTRIES[index as usize])
    }

    pub fn ignore_branches(&self) -> bool {
        self.from_leaves.len() >= self.count_requested as usize
    }

    pub fn add_suggestion(&mut self, index: EntryIndex, from_leaf: bool, expanded: bool) -> bool {
        // Treat all expanded leaves as branches
        let from_leaf = from_leaf && !expanded;

        // If this is a branch and there are enough leaves, return early
        if !from_leaf && self.ignore_branches() {
            return true;
        }

        // Pick which sets to use based on whether this is a leaf
        let (seen, set) = if expanded {
            (&mut self.seen_expanded, &mut self.from_expanded)
        } else if from_leaf {
            (&mut self.seen_leaves, &mut self.from_leaves)
        } else {
            (&mut self.seen_branches, &mut self.from_branches)
        };

        // If the word was seen (even if it's a different entry), return early
        let word = &ENTRIES[index as usize].word[..];
        if seen.contains(&word) {
            return false;
        }

        // Insert the word and index into the sets
        seen.insert(word);
        set.insert(index);

        // If the number of leaf suggestions reaches the limit, clear the branch suggestions
        if from_leaf && set.len() == self.count_requested as usize {
            self.seen_branches = HashSet::new();
            self.seen_expanded = HashSet::new();
            self.from_branches = BTreeSet::new();
            self.from_expanded = BTreeSet::new();
        }

        false
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
enum SearchPrecedence {
    // Type A: exact word, exact definition, prefix word
    A,
    // Type B: prefix definition, suffix word
    B,
    // Type C: suffix definition
    C,
    // Always bottom: other word, other definition
    Bottom,
}

#[derive(Debug)]
struct SearchResultEntry {
    exact: bool,
    precedence: SearchPrecedence,
    words: BTreeSet<WordIndex>,
}

impl SearchResultEntry {
    fn new(word: WordData, start: bool, end: bool, expanded: bool) -> Self {
        let is_word = word.index() == NO_WORD;
        let exact = is_word && start && end;

        // Pick the precedence depending on if it is a definition
        let precedence = if is_word && !expanded {
            match (start, end) {
                (true, _) => SearchPrecedence::A,
                (false, true) => SearchPrecedence::B,
                (false, false) => SearchPrecedence::Bottom,
            }
        } else if !word.in_paren() {
            match (start, end && !expanded) {
                (true, true) => SearchPrecedence::A,
                (true, false) => SearchPrecedence::B,
                (false, true) => SearchPrecedence::C,
                (false, false) => SearchPrecedence::Bottom,
            }
        } else if start {
            SearchPrecedence::C
        } else {
            SearchPrecedence::Bottom
        };

        let mut words = BTreeSet::new();
        words.insert(word.index());

        Self {
            exact,
            precedence,
            words,
        }
    }

    fn word_match(&self) -> bool {
        self.words.contains(&NO_WORD)
    }

    fn append(&mut self, mut other: Self, intersect: bool) {
        self.exact |= other.exact;

        self.precedence = match (self.word_match(), other.word_match()) {
            // Combine definition searches by taking the worst precedence when intersecting
            (false, false) if intersect => self.precedence.max(other.precedence),

            // Combine mixed word and definition searches by using word precedence
            (true, false) => self.precedence,
            (false, true) => other.precedence,

            // Combine other searches by taking the best precedence
            _ => self.precedence.min(other.precedence),
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

    pub fn insert(&mut self, loc: Loc, start: bool, end: bool, expanded: bool) {
        self.insert_entry(
            loc.entry,
            SearchResultEntry::new(loc.word, start, end, expanded),
            false,
        );
    }

    fn insert_entry(&mut self, index: EntryIndex, entry: SearchResultEntry, intersect: bool) {
        if let Some(existing) = self.map.get_mut(&index) {
            existing.append(entry, intersect);
        } else {
            self.map.insert(index, entry);
        }
    }

    fn entry_intersection(&self, set: &mut HashSet<EntryIndex>) {
        set.retain(|entry| self.map.contains_key(entry));
    }

    pub fn filter(intersect: Vec<Self>, difference: Vec<Self>, kinds: KindSet) -> Self {
        // If the search is exactly one result with no filters, return it
        if intersect.len() == 1 && difference.is_empty() && kinds.is_empty() {
            return intersect.into_iter().next().unwrap();
        }

        // Convert an empty set of kinds to match anything
        let kinds = kinds.to_non_empty();

        // Find the union of the negative results
        let difference_set: HashSet<_> = difference
            .into_iter()
            .flat_map(|result| result.map.into_keys())
            .collect();

        // If the intersection is empty, use entries directly
        if intersect.is_empty() {
            return Self::filter_all(difference_set, kinds);
        }

        // Find the intersection of the positive results
        let mut intersect_set = intersect[0].map.keys().cloned().collect();
        for intersect in &intersect[1..] {
            intersect.entry_intersection(&mut intersect_set);
        }

        // Get a direct slice for the entries array
        let entries: &[Entry] = &ENTRIES;

        // Combine results which are in intersection but not union, and match in kind
        let mut intersect_difference = Self::default();
        for result in intersect {
            for (index, entry) in result.map {
                if !intersect_set.contains(&index) {
                    continue;
                }

                if difference_set.contains(&index) {
                    continue;
                }

                if !entries[index as usize].kind_set.matches_any(kinds) {
                    continue;
                }

                intersect_difference.insert_entry(index, entry, true);
            }
        }

        intersect_difference
    }

    fn filter_all(difference_set: HashSet<EntryIndex>, kinds: KindSet) -> Self {
        // Similar logic to filter(), but with full set of entries
        let mut result = Self::default();
        for (i, entry) in ENTRIES.iter().enumerate() {
            let index = i as EntryIndex;

            if difference_set.contains(&index) {
                continue;
            }

            if !entry.kind_set.matches_any(kinds) {
                continue;
            }

            let word = WordData::new(NO_WORD, false);
            let entry = SearchResultEntry::new(word, true, false, false);
            result.insert_entry(index, entry, true);
        }

        result
    }

    pub fn rank(self) -> SearchRanking {
        use SearchPrecedence::*;
        use SearchRank::*;

        // Check for which precedence levels are present
        let mut has_exact = false;
        let mut a_count = 0;
        let mut b_count = 0;
        let mut c_count = 0;
        let mut other_count = 0;
        for entry in self.map.values() {
            has_exact |= entry.exact;

            match entry.precedence {
                A => a_count += 1,
                B => b_count += 1,
                C => c_count += 1,
                Bottom => other_count += 1,
            }
        }

        let (a, mut b, mut c, mut other) = match (a_count > 0, b_count > 0, c_count > 0) {
            // Default ranking of precedences
            (true, true, _) => (Best, Related, Other, Other),

            // Shift up due to missing precedence level
            (true, false, _) => (Best, Ignore, Related, Other),
            (false, true, _) => (Ignore, Best, Related, Other),
            (false, false, true) => (Ignore, Ignore, Best, Other),

            // Shift other up since there's nothing else above it
            (false, false, false) => (Ignore, Ignore, Ignore, Best),
        };

        // Hide lower-ranked results if there are too many higher-ranked results
        let mut total_count = a_count;
        let mut limit = |count, flag: &mut SearchRank| {
            const SOFT_LIMIT: u32 = 1000;
            const HARD_LIMIT: u32 = 5000;
            const CATEGORY_MIN: u32 = 10;

            if total_count > HARD_LIMIT || (count > CATEGORY_MIN && total_count > SOFT_LIMIT) {
                *flag = Ignore;
            }

            total_count += count;
        };

        limit(b_count, &mut b);
        limit(c_count, &mut c);
        limit(other_count, &mut other);

        // Insert results into the ranking at the appropriate level
        let mut ranking = SearchRanking::new(has_exact);
        for (index, entry) in self.map {
            let rank = match entry.precedence {
                A => a,
                B => b,
                C => c,
                Bottom => other,
            };

            // Highlight the entry as exact only if it is in the best ranking
            let exact = rank == Best && entry.exact;

            ranking.insert(rank, exact, index, entry.words);
        }

        ranking
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum SearchRank {
    Best,
    Related,
    Other,
    Ignore,
}

#[derive(Debug)]
pub struct SearchRankingEntry {
    pub entry: &'static Entry,
    pub words: BTreeSet<WordIndex>,
    pub exact: bool,
}

#[derive(Debug)]
pub struct SearchRanking {
    pub good_search: bool,
    pub best: Vec<SearchRankingEntry>,
    pub related: Vec<SearchRankingEntry>,
    pub other: Vec<SearchRankingEntry>,
}

impl SearchRanking {
    pub fn is_empty(&self) -> bool {
        self.best.is_empty() && self.related.is_empty() && self.other.is_empty()
    }

    fn new(good_search: bool) -> Self {
        Self {
            good_search,
            best: Vec::new(),
            related: Vec::new(),
            other: Vec::new(),
        }
    }

    fn insert(
        &mut self,
        rank: SearchRank,
        exact: bool,
        index: EntryIndex,
        words: BTreeSet<WordIndex>,
    ) {
        use SearchRank::*;

        let entry = SearchRankingEntry {
            entry: &ENTRIES[index as usize],
            words,
            exact,
        };

        match rank {
            Best => self.best.push(entry),
            Related => self.related.push(entry),
            Other => self.other.push(entry),
            Ignore => {}
        }
    }
}
