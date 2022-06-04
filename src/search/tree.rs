use std::collections::BTreeMap;
use std::mem;

use thiserror::Error;

use crate::dictionary::{self, Loc, ENTRIES};
use crate::tamil::{Letter, LetterSet, Word};

use super::{Search, SearchResult, Suggest, SuggestionList};

lazy_static! {
    static ref WORD_TREES: SearchTrees = build_trees("word", dictionary::words());
    static ref DEFINITION_TREES: SearchTrees =
        build_trees("definition", dictionary::definition_words());
    static ref EMPTY_MAP: BTreeMap<Letter, Tree<Loc>> = BTreeMap::new();
}

#[derive(Debug)]
struct SearchTrees {
    prefix_tree: Tree<Loc>,
    suffix_tree: Tree<Loc>,
}

pub fn search_word() -> TreeSearch {
    TreeSearch::new(&[&WORD_TREES])
}

pub fn search_definition() -> TreeSearch {
    TreeSearch::new(&[&DEFINITION_TREES, &WORD_TREES])
}

pub fn search_word_prefix() -> TreeSearch {
    TreeSearch {
        branches: vec![SearchBranch::new(&WORD_TREES.prefix_tree, true)],
    }
}

fn build_trees(kind: &str, iter: impl Iterator<Item = (&'static Word, Loc)>) -> SearchTrees {
    lazy_static::initialize(&ENTRIES);
    eprintln!("Building {} trees...", kind);

    let mut prefix_tree = Tree::default();
    let mut suffix_tree = Tree::default();
    for (word, loc) in iter {
        for i in 1..word.len() {
            suffix_tree = suffix_tree.union(Tree::singleton(&word[i..], loc));
        }
        prefix_tree = prefix_tree.union(Tree::singleton(word, loc));
    }

    // Shrink the leaves of the trees to reduce unused space
    prefix_tree.shrink();
    suffix_tree.shrink();

    eprintln!(
        " => {} {} prefix nodes (depth = {})",
        prefix_tree.size(),
        kind,
        prefix_tree.depth()
    );
    eprintln!(
        " => {} {} suffix nodes (depth = {})",
        suffix_tree.size(),
        kind,
        suffix_tree.depth()
    );

    SearchTrees {
        prefix_tree,
        suffix_tree,
    }
}

const SEARCH_MAX_RESULTS: usize = 32768;
const SEARCH_MAX_BRANCHES: usize = 8192;

#[derive(Error, Debug)]
pub enum SearchError {
    #[error("Error: the search query is too complex. Try using fewer wildcards.")]
    TooComplex,
    #[error("Error: the search query has too many matches. Try being more specific.")]
    TooManyResults,
    #[error("Error: an excluded word in the search has too many matches.")]
    CommonExclusion,
}

#[derive(Clone, Debug)]
pub struct TreeSearch {
    branches: Vec<SearchBranch<Loc>>,
}

impl TreeSearch {
    fn new(trees: &[&'static SearchTrees]) -> Self {
        let branches = trees
            .iter()
            .flat_map(|trees| {
                [
                    SearchBranch::new(&trees.prefix_tree, true),
                    SearchBranch::new(&trees.suffix_tree, false),
                ]
            })
            .collect();

        Self { branches }
    }
}

impl Search for TreeSearch {
    type Output = SearchResult;
    type Error = SearchError;

    fn empty() -> Self {
        Self {
            branches: Vec::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.branches.is_empty()
    }

    fn asserting_start(&self) -> Self {
        let branches = self
            .branches
            .iter()
            .filter(|branch| branch.is_start())
            .copied()
            .collect();

        Self { branches }
    }

    fn asserting_middle(&self) -> Self {
        let branches = self
            .branches
            .iter()
            .filter_map(|branch| branch.middle())
            .collect();

        Self { branches }
    }

    fn asserting_end(&self) -> Self {
        let empty_map = &EMPTY_MAP;

        let branches = self
            .branches
            .iter()
            .filter_map(|branch| branch.suffix(empty_map))
            .collect();

        Self { branches }
    }

    fn asserting_next(&self, lts: LetterSet) -> Self {
        let branches = self
            .branches
            .iter()
            .filter_map(|branch| branch.assert_next(lts))
            .collect();

        Self { branches }
    }

    fn asserting_prev(&self, lt: Letter) -> Self {
        let branches = self
            .branches
            .iter()
            .filter_map(|branch| {
                match branch.prev_letter {
                    // The branch isn't initial, so keep it if it matches
                    Some(prev) if prev == lt => Some(*branch),
                    Some(_) => None,

                    // The branch is initial, so consume the letter directly
                    None => branch.ignore_prefix().literal(word![lt]),
                }
            })
            .collect();

        Self { branches }
    }

    fn asserting_prev_matching(&self, lts: LetterSet) -> Result<Self, Self::Error> {
        let mut branches = Vec::with_capacity(self.branches.len());

        for branch in &self.branches {
            match branch.prev_letter {
                // The branch isn't initial, so keep it if it matches
                Some(prev) if lts.matches(prev) => branches.push(*branch),
                Some(_) => {}

                // The branch is initial, so consume the letter directly
                None => branch.ignore_prefix().append_matches(&mut branches, lts)?,
            }
        }

        Ok(Self { branches })
    }

    fn literal(&self, word: &Word) -> Self {
        let branches = self
            .branches
            .iter()
            .filter_map(|branch| branch.literal(word))
            .collect();

        Self { branches }
    }

    fn matching(&self, lts: LetterSet) -> Result<Self, Self::Error> {
        let mut branches = Vec::new();
        for branch in &self.branches {
            branch.append_matches(&mut branches, lts)?;
        }

        Ok(Self { branches })
    }

    fn join(&mut self, other: &Self) -> Result<(), Self::Error> {
        if self.branches.len() + other.branches.len() > SEARCH_MAX_BRANCHES {
            return Err(SearchError::TooComplex);
        }

        self.branches.extend_from_slice(&other.branches);
        Ok(())
    }

    fn mark_expanded(&mut self) {
        for branch in &mut self.branches {
            branch.mark_expanded();
        }
    }

    fn freeze(&mut self) {
        for branch in &mut self.branches {
            branch.freeze();
        }
    }

    fn end(self) -> Result<Self::Output, Self::Error> {
        let mut result = SearchResult::default();
        let mut total_count = 0;
        for branch in self.branches {
            branch.add_to_result(&mut result, &mut total_count, branch.prefix.is_empty())?;
        }

        Ok(result)
    }
}

impl Suggest for TreeSearch {
    fn suggest(self, count: u32) -> SuggestionList {
        let mut list = SuggestionList::new(count);

        let is_single_branch = self.branches.len() == 1;
        for branch in self.branches {
            if branch.prev_letter.is_some() && !branch.is_frozen {
                branch.append_suggestions(
                    &mut list,
                    true,
                    is_single_branch && branch.prefix.is_empty(),
                );
            }
        }

        list
    }
}

#[derive(Copy, Clone, Debug)]
struct SearchBranch<T: Eq + 'static> {
    is_prefix_tree: bool,
    is_expanded: bool,
    is_frozen: bool,
    prev_letter: Option<Letter>,
    require: Option<LetterSet>,
    prefix: &'static Word,
    leaves: &'static [T],
    branches: &'static BTreeMap<Letter, Tree<T>>,
}

impl<T: Eq + Copy> SearchBranch<T> {
    fn new(tree: &'static Tree<T>, is_prefix_tree: bool) -> Self {
        Self {
            is_prefix_tree,
            is_expanded: false,
            is_frozen: false,
            prev_letter: None,
            require: None,
            prefix: tree.prefix,
            leaves: &tree.leaves,
            branches: &tree.branches,
        }
    }

    fn update(&self, lt: Letter, tree: &'static Tree<T>) -> Self {
        Self {
            prev_letter: Some(lt),
            require: None,
            prefix: tree.prefix,
            leaves: &tree.leaves,
            branches: &tree.branches,
            ..*self
        }
    }

    fn step(&self) -> Self {
        Self {
            prev_letter: Some(self.prefix[0]),
            require: None,
            prefix: &self.prefix[1..],
            ..*self
        }
    }

    fn ignore_prefix(mut self) -> Self {
        self.is_prefix_tree = false;
        self
    }

    fn assert_next(&self, mut lts: LetterSet) -> Option<Self> {
        if self.is_frozen {
            return None;
        }

        // Intersect the assertion with any existing assertion
        if let Some(require) = self.require {
            lts = lts.intersect(require);

            // If the resulting assertion is the same, return early
            if lts == require {
                return Some(*self);
            }
        }

        // If the assertion always fails, fail immediately
        if lts.is_empty() {
            return None;
        }

        if self.prefix.is_empty() {
            if self.branches.is_empty() {
                // There are no branches, so the assertion can be ignored
                None
            } else {
                // Remember the assertion for when the search advances
                Some(Self {
                    require: Some(lts),
                    leaves: &[],
                    ..*self
                })
            }
        } else {
            // There is a prefix, so just check if it matches the prefix
            if lts.matches(self.prefix[0]) {
                Some(*self)
            } else {
                None
            }
        }
    }

    fn is_start(&self) -> bool {
        self.is_prefix_tree && self.prev_letter.is_none()
    }

    fn middle(&self) -> Option<Self> {
        if self.is_start() {
            None
        } else if !self.prefix.is_empty() {
            Some(*self)
        } else if self.branches.is_empty() {
            None
        } else {
            Some(Self {
                leaves: &[],
                ..*self
            })
        }
    }

    fn suffix(&self, empty: &'static BTreeMap<Letter, Tree<T>>) -> Option<Self> {
        if self.prefix.is_empty() && !self.leaves.is_empty() && !self.is_frozen {
            Some(Self {
                branches: empty,
                ..*self
            })
        } else {
            None
        }
    }

    fn mark_expanded(&mut self) {
        self.is_expanded = true;
    }

    fn freeze(&mut self) {
        self.is_frozen = true;
    }

    fn literal(mut self, word: &Word) -> Option<Self> {
        if word.is_empty() {
            return Some(self);
        }

        if self.is_frozen {
            return None;
        }

        // Check for a requirement on the first letter
        if let Some(require) = self.require {
            if !require.matches(word[0]) {
                return None;
            }

            self.require = None;
        }

        // Advance one letter at a time through the word
        for lt in word {
            if self.prefix.is_empty() {
                self = self.update(lt, self.branches.get(&lt)?);
            } else if self.prefix[0] == lt {
                self = self.step();
            } else {
                return None;
            }
        }

        Some(self)
    }

    fn append_matches(
        &self,
        output: &mut Vec<Self>,
        mut lts: LetterSet,
    ) -> Result<(), SearchError> {
        // Check for a requirement on the next letter
        if let Some(require) = self.require {
            lts = lts.intersect(require);
        }

        if self.is_frozen || lts.is_empty() {
            return Ok(());
        }

        if self.prefix.is_empty() {
            // Iterate through the branches, keeping those that match
            for (&lt, tree) in self.branches {
                if lts.matches(lt) {
                    output.push(self.update(lt, tree));

                    if output.len() > SEARCH_MAX_BRANCHES {
                        return Err(SearchError::TooComplex);
                    }
                }
            }
        } else if lts.matches(self.prefix[0]) {
            // There is a prefix, so drop the first letter
            output.push(self.step());

            if output.len() > SEARCH_MAX_BRANCHES {
                return Err(SearchError::TooComplex);
            }
        }

        Ok(())
    }
}

impl SearchBranch<Loc> {
    fn add_to_result(
        self,
        result: &mut SearchResult,
        total_count: &mut usize,
        suffix: bool,
    ) -> Result<(), SearchError> {
        // Make sure the total count doesn't exceed the limit
        *total_count += self.leaves.len();
        if *total_count > SEARCH_MAX_RESULTS {
            return Err(SearchError::TooManyResults);
        }

        // Add all of the leaves to the result
        for &loc in self.leaves {
            result.insert(loc, self.is_prefix_tree, suffix, self.is_expanded);
        }

        // Recursively add all of the matching branches to the result
        for (&lt, branch) in self.branches {
            match &self.require {
                // If there is a requirement and it fails, then skip this branch
                Some(require) if !require.matches(lt) => {}

                // Otherwise, add the branch to the result
                _ => {
                    self.update(lt, branch)
                        .add_to_result(result, total_count, false)?;
                }
            }
        }

        Ok(())
    }

    fn append_suggestions(
        self,
        list: &mut SuggestionList,
        mut from_leaf: bool,
        exact: bool,
    ) -> bool {
        // Add suggestions for all of the leaves, returning early if one fails
        for leaf in self.leaves {
            if list.add_suggestion(leaf.entry, from_leaf, self.is_expanded) {
                return true;
            }
        }

        // If there was a successful match that wasn't exact, then the branches aren't important
        if !exact && !self.leaves.is_empty() {
            from_leaf = false;
        }

        // Recursively add suggestions for all of the child branches
        for (&lt, branch) in self.branches {
            match &self.require {
                // If there is a requirement and it fails, then skip this branch
                Some(require) if !require.matches(lt) => {}

                // Otherwise, add the branch to the suggestion, returning early if it fails
                _ => {
                    if self
                        .update(lt, branch)
                        .append_suggestions(list, from_leaf, false)
                    {
                        break;
                    }
                }
            }
        }

        false
    }
}

#[derive(Debug)]
struct Tree<T: Eq> {
    prefix: &'static Word,
    leaves: Vec<T>,
    branches: BTreeMap<Letter, Tree<T>>,
}

impl<T: Eq> Tree<T> {
    fn singleton(key: &'static Word, value: T) -> Self {
        Self {
            prefix: key,
            leaves: vec![value],
            branches: BTreeMap::new(),
        }
    }

    fn size(&self) -> usize {
        self.branches.values().map(Self::size).sum::<usize>() + 1
    }

    fn depth(&self) -> usize {
        self.branches.values().map(Self::depth).max().unwrap_or(0) + 1
    }

    fn strip_to(self, index: usize) -> Self {
        Self {
            prefix: &self.prefix[(index + 1)..],
            ..self
        }
    }

    fn shrink(&mut self) {
        self.leaves.shrink_to_fit();
        for branch in self.branches.values_mut() {
            branch.shrink();
        }
    }

    fn append_branch(&mut self, key: Letter, tree: Self) {
        if let Some(old) = self.branches.remove(&key) {
            self.branches.insert(key, old.union(tree));
        } else {
            self.branches.insert(key, tree);
        }
    }

    fn union(mut self, mut rhs: Self) -> Self {
        // Make sure that self isn'static longer than rhs
        if self.prefix.len() > rhs.prefix.len() {
            mem::swap(&mut self, &mut rhs);
        }

        // Check for a difference to split on
        for (i, (a, b)) in self.prefix.iter().zip(rhs.prefix).enumerate() {
            if a == b {
                continue;
            }

            // Find the matching prefix
            let prefix = &self.prefix[..i];

            // Create a branch at the difference
            let mut branches = BTreeMap::new();
            branches.insert(a, self.strip_to(i));
            branches.insert(b, rhs.strip_to(i));

            return Self {
                prefix,
                leaves: Vec::new(),
                branches,
            };
        }

        if self.prefix.len() == rhs.prefix.len() {
            // The keys are equal, so merge the leaves and branches
            self.leaves.append(&mut rhs.leaves);
            for (key, branch) in rhs.branches {
                self.append_branch(key, branch);
            }
        } else {
            // The first key is a prefix, so add the other as a branch
            self.append_branch(
                rhs.prefix[self.prefix.len()],
                rhs.strip_to(self.prefix.len()),
            );
        }

        self
    }
}

impl<T: Eq> Default for Tree<T> {
    fn default() -> Self {
        Self {
            prefix: Word::new(),
            leaves: Vec::new(),
            branches: BTreeMap::new(),
        }
    }
}
