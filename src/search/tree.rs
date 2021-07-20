use std::mem;
use std::hash::Hash;
use std::collections::{BTreeMap, HashSet};

use crate::dictionary::{ENTRIES, Entry, Word, Letter, LetterSet};

use super::{SearchError, SearchResult};

lazy_static! {
    static ref WORD_TREES: SearchTrees = build_trees("word", |entry| &entry.parsed_words);
    static ref DEFINITION_TREES: SearchTrees = build_trees("definition", |entry| &entry.parsed_definition);
}

#[derive(Debug)]
struct SearchTrees {
    prefix_tree: Tree<u32>,
    suffix_tree: Tree<u32>,
}

pub fn search_word() -> Search {
    Search::new(&[&WORD_TREES])
}

pub fn search_definition() -> Search {
    Search::new(&[&DEFINITION_TREES, &WORD_TREES])
}

fn build_trees(kind: &str, f: fn(&'static Entry) -> &'static [Box<Word>]) -> SearchTrees {
    lazy_static::initialize(&ENTRIES);
    eprintln!("Building {} trees...", kind);

    let iter = ENTRIES.iter()
        .enumerate()
        .flat_map(|(i, entry)|
            f(entry).iter().map(move |word| (word.as_ref(), i as u32)));

    let mut prefix_seen = HashSet::new();
    let mut suffix_seen = HashSet::new();
    let mut prefix_tree = Tree::default();
    let mut suffix_tree = Tree::default();
    for (key, value) in iter {
        for i in 1..key.len() {
            suffix_tree = suffix_tree.insert_unseen(&mut suffix_seen, &key[i..], value);
        }
        prefix_tree = prefix_tree.insert_unseen(&mut prefix_seen, key, value);
    }

    eprintln!(" => {} {} prefix nodes (depth = {})", prefix_tree.size(), kind, prefix_tree.depth());
    eprintln!(" => {} {} suffix nodes (depth = {})", suffix_tree.size(), kind, suffix_tree.depth());

    SearchTrees {
        prefix_tree,
        suffix_tree,
    }
}

const SEARCH_MAX_RESULTS: usize = 16384;
const SEARCH_MAX_BRANCHES: usize = 8192;
const SEARCH_MAX_FINAL_BRANCHES: usize = 64;

#[derive(Clone, Debug)]
pub struct Search {
    branches: Vec<SearchBranch<u32>>,
}

impl Search {
    fn new(trees: &[&'static SearchTrees]) -> Self {
        let branches = trees.iter()
            .flat_map(|trees| [
                SearchBranch::new(&trees.prefix_tree, true),
                SearchBranch::new(&trees.suffix_tree, false),
            ])
            .collect();

        Self { branches }
    }
}

impl super::Search for Search {
    fn is_empty(&self) -> bool {
        self.branches.is_empty()
    }

    fn asserting_start(&self) -> Self {
        let branches = self.branches.iter()
            .filter(|branch| branch.is_start())
            .cloned()
            .collect();

        Self { branches }
    }

    fn asserting_end(&self) -> Self {
        // Workaround: BTreeMap::new is not const
        lazy_static! {
            static ref EMPTY: BTreeMap<Letter, Tree<u32>> = BTreeMap::new();
        };

        let branches = self.branches.iter()
            .filter_map(|branch| branch.suffix(&EMPTY))
            .collect();

        Self { branches }
    }

    fn literal(&self, word: &Word) -> Self {
        let branches = self.branches.iter()
            .cloned()
            .filter_map(|branch| branch.literal(word))
            .collect();

        Self { branches }
    }

    fn matching(&self, lts: LetterSet) -> Result<Search, SearchError> {
        let mut branches = Vec::new();
        for branch in &self.branches {
            branch.append_match(&mut branches, lts)?;
        }

        Ok(Self { branches })
    }

    fn join(&mut self, other: &Self) -> Result<(), SearchError> {
        if self.branches.len() + other.branches.len() > SEARCH_MAX_BRANCHES {
            return Err(SearchError::TooComplex);
        }

        self.branches.extend_from_slice(&other.branches);
        Ok(())
    }

    fn end(mut self) -> Result<SearchResult, SearchError> {
        self.branches.retain(|branch| !branch.is_initial);

        if self.branches.len() > SEARCH_MAX_FINAL_BRANCHES {
            return Err(SearchError::TooComplex);
        }

        let mut result = SearchResult::default();
        for branch in self.branches {
            if result.len() > SEARCH_MAX_RESULTS {
                return Err(SearchError::TooManyResults);
            }

            add_branch_to_result(&mut result, branch.prefix.is_empty(), branch);
        }

        result.normalize();
        Ok(result)
    }
}

fn add_branch_to_result(result: &mut SearchResult, suffix: bool, tree: SearchBranch<u32>) {
    for &index in tree.leaves {
        let vec = match (tree.is_prefix_tree, suffix) {
            (true, true) => &mut result.exact,
            (true, false) => &mut result.prefix,
            (false, true) => &mut result.affix,
            (false, false) => &mut result.other,
        };

        vec.insert(index);
    }

    for (_, branch) in tree.branches {
        add_branch_to_result(result, false, tree.update(branch));
    }
}

#[derive(Copy, Clone, Debug)]
struct SearchBranch<T: Eq + 'static> {
    is_prefix_tree: bool,
    is_initial: bool,
    prefix: &'static Word,
    leaves: &'static [T],
    branches: &'static BTreeMap<Letter, Tree<T>>,
}

impl<T: Eq> SearchBranch<T> {
    fn new(tree: &'static Tree<T>, is_prefix_tree: bool) -> Self {
        Self {
            is_prefix_tree,
            is_initial: true,
            prefix: tree.prefix,
            leaves: &tree.leaves,
            branches: &tree.branches,
        }
    }

    fn update(&self, tree: &'static Tree<T>) -> Self {
        Self {
            is_prefix_tree: self.is_prefix_tree,
            is_initial: false,
            prefix: tree.prefix,
            leaves: &tree.leaves,
            branches: &tree.branches,
        }
    }

    fn step(&self) -> Self {
        Self {
            is_initial: false,
            prefix: &self.prefix[1..],
            ..*self
        }
    }

    fn is_start(&self) -> bool {
        self.is_initial && self.is_prefix_tree
    }

    fn suffix(&self, empty: &'static BTreeMap<Letter, Tree<T>>) -> Option<Self> {
        if self.prefix.is_empty() && !self.leaves.is_empty() {
            Some(Self {
                branches: empty,
                ..*self
            })
        } else {
            None
        }
    }

    fn literal(mut self, word: &Word) -> Option<Self> {
        for &lt in word {
            if self.prefix.is_empty() {
                self = self.update(self.branches.get(&lt)?);
            } else if self.prefix[0] == lt {
                self = self.step();
            } else {
                return None;
            }
        }

        Some(self)
    }

    fn append_match(&self, output: &mut Vec<Self>, lts: LetterSet) -> Result<(), SearchError> {
        if self.prefix.is_empty() {
            for (&lt, tree) in self.branches {
                if lts.matches(lt) {
                    output.push(self.update(tree));

                    if output.len() > SEARCH_MAX_BRANCHES {
                        return Err(SearchError::TooComplex);
                    }
                }
            }
        } else if lts.matches(self.prefix[0]) {
            output.push(self.step());

            if output.len() > SEARCH_MAX_BRANCHES {
                return Err(SearchError::TooComplex);
            }
        }

        Ok(())
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

    fn insert_unseen(self, seen: &mut HashSet<(&'static Word, T)>, key: &'static Word, value: T) -> Self
        where T: Clone + Hash
    {
        if seen.insert((key, value.clone())) {
            self.union(Self::singleton(key, value))
        } else {
            self
        }
    }

    fn strip_to(self, index: usize) -> Self {
        Self {
            prefix: &self.prefix[(index + 1)..],
            ..self
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
        for (i, (&a, &b)) in self.prefix.iter().zip(rhs.prefix).enumerate() {
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
                rhs.strip_to(self.prefix.len()));
        }

        self
    }
}

impl<T: Eq> Default for Tree<T> {
    fn default() -> Self {
        Self {
            prefix: &[],
            leaves: Vec::new(),
            branches: BTreeMap::new(),
        }
    }
}
