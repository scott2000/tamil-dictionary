use std::collections::BTreeMap;
use std::mem;

use once_cell::sync::OnceCell;

use thiserror::Error;

use crate::dictionary::{self, Loc};
use crate::tamil::{Letter, LetterSet, Word};

use super::{Search, SearchResult, Suggest, SuggestionList};

fn word_trees() -> &'static SearchTrees {
    static INSTANCE: OnceCell<SearchTrees> = OnceCell::new();

    INSTANCE.get_or_init(|| build_trees("word", dictionary::words()))
}

fn definition_trees() -> &'static SearchTrees {
    static INSTANCE: OnceCell<SearchTrees> = OnceCell::new();

    INSTANCE.get_or_init(|| build_trees("definition", dictionary::definition_words()))
}

fn empty_map() -> &'static BTreeMap<Letter, Tree> {
    static INSTANCE: OnceCell<BTreeMap<Letter, Tree>> = OnceCell::new();

    INSTANCE.get_or_init(BTreeMap::new)
}

#[derive(Debug)]
struct SearchTrees {
    prefix_tree: Tree,
    suffix_tree: Tree,
}

pub fn search_word() -> TreeSearch {
    TreeSearch::new(&[word_trees()])
}

pub fn search_definition() -> TreeSearch {
    TreeSearch::new(&[definition_trees(), word_trees()])
}

pub fn search_word_prefix() -> TreeSearch {
    TreeSearch {
        branches: vec![SearchBranch::new(
            &word_trees().prefix_tree,
            PrefixMode::Asserted,
        )],
    }
}

fn build_trees(kind: &str, iter: impl Iterator<Item = (&'static Word, Loc)>) -> SearchTrees {
    let _ = dictionary::entries();

    eprintln!("Building {kind} trees...");

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
    branches: Vec<SearchBranch>,
}

impl TreeSearch {
    fn new(trees: &[&'static SearchTrees]) -> Self {
        let branches = trees
            .iter()
            .flat_map(|trees| {
                [
                    SearchBranch::new(&trees.prefix_tree, PrefixMode::Prefix),
                    SearchBranch::new(&trees.suffix_tree, PrefixMode::Suffix),
                ]
            })
            .collect();

        Self { branches }
    }

    fn add_branches(
        result: &mut SearchResult,
        total_count: &mut usize,
        branches: Vec<SearchBranch>,
    ) -> Result<(), SearchError> {
        for branch in branches {
            if branch.prev_letter.is_some() {
                branch.add_to_result(result, total_count, branch.prefix.is_empty())?;
            }
        }

        Ok(())
    }
}

impl Search for TreeSearch {
    type Output = SearchResult;
    type Error = SearchError;

    fn is_empty(&self) -> bool {
        self.branches.is_empty()
    }

    fn clearing(&self) -> Self {
        Self {
            branches: Vec::new(),
        }
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
        let empty_map = empty_map();

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
                    None => branch
                        .ignore_prefix()
                        .and_then(|branch| branch.literal(word![lt])),
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
                None => {
                    if let Some(branch) = branch.ignore_prefix() {
                        branch.append_matches(&mut branches, lts)?;
                    }
                }
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

    fn joining(mut self, mut other: Self) -> Result<Self, Self::Error> {
        if self.branches.len() + other.branches.len() > SEARCH_MAX_BRANCHES {
            return Err(SearchError::TooComplex);
        }

        if other.branches.len() > self.branches.len() {
            mem::swap(&mut self, &mut other);
        }

        self.branches.append(&mut other.branches);
        Ok(self)
    }

    fn marking_expanded(mut self) -> Self {
        for branch in &mut self.branches {
            branch.mark_expanded();
        }

        self
    }

    fn freezing(mut self) -> Self {
        for branch in &mut self.branches {
            branch.freeze();
        }

        self
    }

    fn end(self) -> Result<Self::Output, Self::Error> {
        let mut prefix_branches = Vec::new();
        let mut suffix_branches = Vec::new();

        // Split the branches into prefix and suffix branches
        for branch in self.branches {
            if branch.prefix_mode.is_prefix() {
                prefix_branches.push(branch);
            } else {
                suffix_branches.push(branch);
            }
        }

        // If the prefix branches are empty, treat suffix as prefix
        if prefix_branches.is_empty() {
            mem::swap(&mut prefix_branches, &mut suffix_branches);
        }

        let mut total_count = 0;
        let mut result = SearchResult::default();

        // Always require the prefix branches
        Self::add_branches(&mut result, &mut total_count, prefix_branches)?;

        // If there are suffix branches, attempt to add them too
        if !suffix_branches.is_empty() {
            let mut suffix_result = SearchResult::default();

            if Self::add_branches(&mut suffix_result, &mut total_count, suffix_branches).is_ok() {
                return Ok(result.union(suffix_result));
            }
        }

        Ok(result)
    }
}

impl Suggest for TreeSearch {
    fn suggest(self, count: u32) -> SuggestionList {
        let mut list = SuggestionList::new(count);

        let is_single_branch = self.branches.len() == 1;
        for branch in self.branches {
            if branch.prev_letter.is_some() && branch.expand_mode != ExpandMode::Frozen {
                branch.append_suggestions(
                    &mut list,
                    is_single_branch || (branch.prefix.is_empty() && !branch.leaves.is_empty()),
                    is_single_branch && branch.prefix.is_empty(),
                );
            }
        }

        list
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u8)]
enum PrefixMode {
    Suffix,
    Prefix,
    Asserted,
}

impl PrefixMode {
    #[inline(always)]
    fn is_prefix(self) -> bool {
        self != Self::Suffix
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u8)]
enum ExpandMode {
    NotExpanded,
    Expanded,
    Frozen,
}

impl ExpandMode {
    #[inline(always)]
    fn is_expanded(self) -> bool {
        self != Self::NotExpanded
    }
}

#[derive(Copy, Clone, Debug)]
struct SearchBranch<T: Eq + 'static = Loc> {
    prefix_mode: PrefixMode,
    expand_mode: ExpandMode,
    prev_letter: Option<Letter>,
    require: Option<LetterSet>,
    prefix: &'static Word,
    leaves: &'static [T],
    branches: &'static BTreeMap<Letter, Tree<T>>,
}

impl<T: Eq + Copy> SearchBranch<T> {
    fn new(tree: &'static Tree<T>, prefix_mode: PrefixMode) -> Self {
        Self {
            prefix_mode,
            expand_mode: ExpandMode::NotExpanded,
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

    fn ignore_prefix(mut self) -> Option<Self> {
        if self.prefix_mode != PrefixMode::Asserted {
            self.prefix_mode = PrefixMode::Suffix;
            Some(self)
        } else {
            None
        }
    }

    fn assert_next(&self, mut lts: LetterSet) -> Option<Self> {
        if self.expand_mode == ExpandMode::Frozen {
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
        self.prefix_mode.is_prefix() && self.prev_letter.is_none()
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
        if self.prefix.is_empty()
            && !self.leaves.is_empty()
            && self.expand_mode != ExpandMode::Frozen
        {
            Some(Self {
                branches: empty,
                ..*self
            })
        } else {
            None
        }
    }

    fn mark_expanded(&mut self) {
        if self.expand_mode != ExpandMode::Frozen {
            self.expand_mode = ExpandMode::Expanded;
        }
    }

    fn freeze(&mut self) {
        self.expand_mode = ExpandMode::Frozen;
    }

    fn literal(mut self, word: &Word) -> Option<Self> {
        if word.is_empty() {
            return Some(self);
        }

        if self.expand_mode == ExpandMode::Frozen {
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

        if self.expand_mode == ExpandMode::Frozen || lts.is_empty() {
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

impl SearchBranch {
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
            result.insert(
                loc,
                self.prefix_mode.is_prefix(),
                suffix,
                self.expand_mode.is_expanded(),
            );
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
            if list.add_suggestion(leaf.entry, from_leaf, self.expand_mode.is_expanded()) {
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
struct Tree<T: Eq = Loc> {
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
