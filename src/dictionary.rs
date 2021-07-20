use std::fs::File;
use std::cmp::Ordering;
use std::fmt::{self, Display, Debug};
use std::collections::{HashMap, HashSet};

use serde::Deserialize;

lazy_static! {
    pub static ref ENTRIES: Box<[Entry]> = {
        eprintln!("Loading dictionary...");

        let file = File::open("dictionary.json")
            .expect("missing dictionary.json");

        let entries: Vec<_> = serde_json::from_reader(file)
            .expect("dictionary parse error");

        let mut entries: Box<[Entry]> = entries.into_iter()
            .enumerate()
            .map(|(i, entry)| Entry::parse(i, entry))
            .filter(|entry| !entry.parsed_words.is_empty() && !entry.definition.is_empty())
            .collect();

        entries.sort();

        eprintln!(" => {} entries", entries.len());
        entries
    };
}

#[derive(Deserialize)]
struct RawEntry {
    word: String,
    definition: String,
}

impl RawEntry {
    pub fn words(word: &str) -> impl Iterator<Item = &str> {
        word.split(&[',', ';'][..])
            .map(str::trim)
    }
}

#[derive(Eq, Debug)]
pub struct Entry {
    pub word: String,
    pub definition: String,
    pub parsed_words: Vec<Box<Word>>,
    pub parsed_definition: Vec<Box<Word>>,
    original_index: usize,
}

impl Entry {
    pub fn words(&self) -> impl Iterator<Item = &str> {
        RawEntry::words(&self.word)
    }

    fn parse(original_index: usize, entry: RawEntry) -> Self {
        let parsed_words = RawEntry::words(&entry.word)
            .map(|s| Letter::parse_str(s))
            .collect();

        let parsed_definition = entry.definition
            .split(|ch: char| ch.is_ascii() && !ch.is_ascii_alphabetic())
            .filter(|s| !s.is_empty())
            .map(Letter::parse_str)
            .collect();

        Self {
            word: entry.word,
            definition: entry.definition,
            parsed_words,
            parsed_definition,
            original_index,
        }
    }
}

impl PartialEq for Entry {
    fn eq(&self, rhs: &Self) -> bool {
        self.parsed_words == rhs.parsed_words && self.original_index == rhs.original_index
    }
}

impl Ord for Entry {
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.parsed_words.cmp(&rhs.parsed_words)
            .then_with(|| self.original_index.cmp(&rhs.original_index))
    }
}

impl PartialOrd for Entry {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}

const TAMIL_VOWELS: &'static [char] = &[
    'அ', 'ஆ',
    'இ', 'ஈ',
    'உ', 'ஊ',
    'எ', 'ஏ', 'ஐ',
    'ஒ', 'ஓ', 'ஔ',
    'ஃ',
];

const TAMIL_VOWEL_SIGNS: &'static [char] = &[
    '\u{bbe}',
    '\u{bbf}', '\u{bc0}',
    '\u{bc1}', '\u{bc2}',
    '\u{bc6}', '\u{bc7}', '\u{bc8}',
    '\u{bca}', '\u{bcb}', '\u{bcc}',
];

const TAMIL_CONSONANTS: &'static [char] = &[
    'க', 'ங',
    'ச', 'ஞ',
    'ட', 'ண',
    'த', 'ந',
    'ப', 'ம',
    'ய', 'ர', 'ல', 'வ', 'ழ', 'ள',
    'ற', 'ன',
    'ஜ', 'ஸ', 'ஷ', 'ஹ', 'ஶ',
];

pub const PULLI: char = '\u{bcd}';

lazy_static! {
    static ref TAMIL_VOWEL_MAP: HashMap<char, Letter> = to_map(0, TAMIL_VOWELS);
    static ref TAMIL_VOWEL_SIGN_MAP: HashMap<char, Letter> = to_map(1, TAMIL_VOWEL_SIGNS);
    static ref TAMIL_CONSONANT_MAP: HashMap<char, Letter> = to_map(13, TAMIL_CONSONANTS);
    static ref VALID_LETTERS: HashSet<char> = {
        let mut set = HashSet::new();
        for ch in TAMIL_VOWELS.iter()
            .chain(TAMIL_VOWEL_SIGNS)
            .chain(TAMIL_CONSONANTS)
            .cloned()
            .chain([PULLI, '\u{200b}', '\u{200c}', '\u{200d}'])
            .chain('a'..='z')
            .chain('A'..='Z')
        {
            set.insert(ch);
        }
        set
    };
}

fn to_map(mut offset: u8, chars: &'static [char]) -> HashMap<char, Letter> {
    let mut map = HashMap::new();
    for &ch in chars {
        map.insert(ch, Letter(offset));
        offset += 1;
    }
    map
}

pub type Word = [Letter];

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Letter(u8);

impl Letter {
    pub fn is_valid(ch: char) -> bool {
        VALID_LETTERS.contains(&ch)
    }

    pub fn parse(ch: char) -> Option<Self> {
        match ch {
            'a'..='z' => Some(Self(ch as u8 - b'a' + 36)),
            'A'..='Z' => Some(Self(ch as u8 - b'A' + 36)),
            _ => {
                TAMIL_VOWEL_MAP.get(&ch)
                    .or_else(|| TAMIL_CONSONANT_MAP.get(&ch))
                    .or_else(|| TAMIL_VOWEL_SIGN_MAP.get(&ch))
                    .copied()
            }
        }
    }

    pub fn parse_str(s: &str) -> Box<Word> {
        let mut word = Vec::new();
        for ch in s.chars() {
            match ch {
                'a'..='z' => word.push(Self(ch as u8 - b'a' + 36)),
                'A'..='Z' => word.push(Self(ch as u8 - b'A' + 36)),
                PULLI => {
                    if let Some(x) = word.pop() {
                        if x != Self(0) {
                            word.push(x);
                        }
                    }
                }
                _ => {
                    if let Some(&n) = TAMIL_VOWEL_MAP.get(&ch) {
                        word.push(n);
                    } else if let Some(&n) = TAMIL_CONSONANT_MAP.get(&ch) {
                        word.push(n);
                        word.push(Self(0));
                    } else if let Some(&n) = TAMIL_VOWEL_SIGN_MAP.get(&ch) {
                        match word.pop() {
                            None => {},
                            Some(Self(0)) => {},
                            Some(Self(6)) if n == Self(1) => {
                                word.push(Self(9));
                                continue;
                            },
                            Some(Self(7)) if n == Self(1) => {
                                word.push(Self(10));
                                continue;
                            },
                            Some(x) => word.push(x),
                        }
                        word.push(n);
                    }
                }
            }
        }
        word.into_boxed_slice()
    }

    pub fn to_char(self) -> char {
        let ch = self.0;
        match ch {
            0..=12 => TAMIL_VOWELS[ch as usize],
            13..=35 => TAMIL_CONSONANTS[ch as usize - 13],
            36..=61 => (ch - 36 + b'a') as char,
            _ => unreachable!("invalid character: {}", ch),
        }
    }

    pub fn to_str(s: &[Self]) -> String {
        let mut word = String::new();
        for &Letter(ch) in s {
            match ch {
                0..=12 => {
                    match word.pop() {
                        None => {},
                        Some(PULLI) => {
                            if ch > 0 {
                                word.push(TAMIL_VOWEL_SIGNS[ch as usize - 1]);
                            }
                            continue;
                        },
                        Some(x) => word.push(x),
                    }
                    word.push(TAMIL_VOWELS[ch as usize]);
                },
                13..=35 => {
                    word.push(TAMIL_CONSONANTS[ch as usize - 13]);
                    word.push(PULLI);
                },
                36..=61 => {
                    word.push((ch - 36 + b'a') as char);
                },
                _ => unreachable!("invalid character: {}", ch),
            }
        }
        word
    }

    pub fn category(self) -> Category {
        match self.0 {
            0..=11 => Category::TamilVowel,
            12 => Category::TamilAaydham,
            13..=30 => Category::TamilConsonant,
            31..=35 => Category::TamilGrantha,
            36..=61 => Category::LatinAlpha,
            ch => unreachable!("invalid character: {}", ch),
        }
    }

    pub fn range(self, to: Self) -> Result<impl Iterator<Item = Self>, (Category, Category)> {
        let a = self.category();
        let b = to.category();
        if a == b {
            Ok((self.0..=to.0).map(Self))
        } else {
            Err((a, b))
        }
    }
}

impl Display for Letter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_char())
    }
}

impl Debug for Letter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Category {
    TamilVowel,
    TamilAaydham,
    TamilConsonant,
    TamilGrantha,
    LatinAlpha,
}

impl Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TamilVowel => write!(f, "vowel"),
            Self::TamilAaydham => write!(f, "aaydham"),
            Self::TamilConsonant => write!(f, "consonant"),
            Self::TamilGrantha => write!(f, "grantha"),
            Self::LatinAlpha => write!(f, "english letter"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct LetterSet(pub u64);

impl LetterSet {
    pub fn empty() -> Self {
        Self(0)
    }

    pub fn any() -> Self {
        Self(!0)
    }

    pub fn single(lt: Letter) -> Self {
        assert!(lt.0 < 63);

        Self(1 << lt.0)
    }

    pub fn complement(self) -> Self {
        Self(!self.0)
    }

    pub fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub fn intersect(self, other: Self) -> Self {
        Self(self.0 & other.0)
    }

    pub fn matches(self, lt: Letter) -> bool {
        assert!(lt.0 < 63);

        (self.0 & (1 << lt.0)) != 0
    }
}

impl Display for LetterSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;

        let flip = (self.0 & (1 << 63)) != 0;
        if flip {
            write!(f, "!")?;
        }

        for i in 0..63 {
            if self.matches(Letter(i)) != flip {
                write!(f, "{}", Letter(i))?;
            }
        }

        write!(f, "]")
    }
}
