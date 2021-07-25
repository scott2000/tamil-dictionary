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

        let mut entries: Box<[Entry]> = serde_json::from_reader(file)
            .expect("dictionary parse error");

        entries.sort();

        eprintln!(" => {} entries", entries.len());
        entries
    };
}

pub fn words() -> impl Iterator<Item = (&'static Word, Loc)> {
    ENTRIES.iter()
        .enumerate()
        .flat_map(|(a, entry)|
            entry.parsed_word.iter()
                .map(move |word| (word.as_ref(), Loc { entry: a as u16, word: NO_WORD })))
}

pub fn definition_words() -> impl Iterator<Item = (&'static Word, Loc)> {
    ENTRIES.iter()
        .enumerate()
        .flat_map(|(a, entry)|
            entry.parsed_text.iter()
                .enumerate()
                .map(move |(b, word)| (word.as_ref(), Loc { entry: a as u16, word: b as u16 })))
}

pub type EntryIndex = u16;
pub type WordIndex = u16;

pub const NO_WORD: WordIndex = u16::MAX;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Loc {
    // The index of the Entry in ENTRIES
    pub entry: EntryIndex,
    // The index of the word in the Entry (0 = word match)
    pub word: WordIndex,
}

#[derive(Deserialize)]
struct RawEntry {
    word: String,
    sub: Option<String>,
    secs: Vec<Vec<Vec<(SegmentKind, String)>>>,
}

impl RawEntry {
    fn words(word: &str) -> impl Iterator<Item = &str> {
        word.split(&[',', ';'][..]).map(str::trim)
    }

    fn skip_char(ch: char) -> bool {
        ch.is_ascii() && !ch.is_ascii_alphabetic()
    }
}

#[derive(Deserialize, Copy, Clone, PartialEq, Eq, Debug)]
pub enum SegmentKind {
    #[serde(rename = "txt")]
    Text,
    #[serde(rename = "ref")]
    Reference,
    #[serde(rename = "sup")]
    Superscript,
    #[serde(rename = "bld")]
    Bold,
}

#[derive(Deserialize, Debug)]
#[serde(from = "RawEntry")]
pub struct Entry {
    pub word: Box<str>,
    pub parsed_word: Box<[Box<Word>]>,
    pub subword: Option<u8>,
    pub text: Box<str>,
    pub parsed_text: Box<[Box<Word>]>,
    pub word_ranges: Box<[(u32, u32)]>,
    pub sections: Box<[Section]>,
}

impl Entry {
    pub fn words(&self) -> impl Iterator<Item = &str> {
        RawEntry::words(&self.word)
    }
}

impl From<RawEntry> for Entry {
    fn from(raw: RawEntry) -> Self {
        let parsed_word: Box<[_]> = RawEntry::words(&raw.word)
            .map(|s| Letter::parse_str(s))
            .collect();

        assert!(!parsed_word.is_empty());

        let subword = raw.sub.and_then(|sub| sub.parse().ok());

        let mut text = String::new();
        let sections = raw.secs.into_iter()
            .map(|sec| {
                sec.into_iter()
                    .map(|para| {
                        para.into_iter()
                            .map(|(kind, s)| {
                                let start = text.len() as u32;
                                text.push_str(&s);
                                let end = text.len() as u32;
                                text.push('\0');
                                Segment {
                                    kind,
                                    start,
                                    end,
                                }
                            })
                            .collect()
                    })
                    .collect()
            })
            .collect();

        let mut parsed_text = Vec::new();
        let mut word_ranges = Vec::new();
        let mut chars = text.char_indices().peekable();
        loop {
            while let Some(_) = chars.next_if(|&(_, ch)| RawEntry::skip_char(ch)) {}

            if let Some((start, _)) = chars.next() {
                while let Some(_) = chars.next_if(|&(_, ch)| !RawEntry::skip_char(ch)) {}

                let end = chars.next()
                    .map(|(i, _)| i)
                    .unwrap_or(text.len());

                parsed_text.push(Letter::parse_str(&text[start..end]));
                word_ranges.push((start as u32, end as u32));
            } else {
                break;
            }
        }

        Self {
            word: raw.word.into_boxed_str(),
            parsed_word,
            subword,
            text: text.into_boxed_str(),
            parsed_text: parsed_text.into_boxed_slice(),
            word_ranges: word_ranges.into_boxed_slice(),
            sections,
        }
    }
}

impl PartialEq for Entry {
    fn eq(&self, rhs: &Self) -> bool {
        self.parsed_word == rhs.parsed_word && self.subword == rhs.subword
    }
}

impl Eq for Entry {}

impl Ord for Entry {
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.parsed_word.cmp(&rhs.parsed_word)
            .then_with(|| self.subword.cmp(&rhs.subword))
    }
}

impl PartialOrd for Entry {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}

pub type Section = Box<[Paragraph]>;

pub type Paragraph = Box<[Segment]>;

#[derive(Debug)]
pub struct Segment {
    pub kind: SegmentKind,
    pub start: u32,
    pub end: u32,
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

    pub fn to_str(word: &Word) -> String {
        let mut s = String::new();
        for &Letter(ch) in word {
            match ch {
                0..=12 => {
                    match s.pop() {
                        None => {},
                        Some(PULLI) => {
                            if ch > 0 {
                                s.push(TAMIL_VOWEL_SIGNS[ch as usize - 1]);
                            }
                            continue;
                        },
                        Some(x) => s.push(x),
                    }
                    s.push(TAMIL_VOWELS[ch as usize]);
                },
                13..=35 => {
                    s.push(TAMIL_CONSONANTS[ch as usize - 13]);
                    s.push(PULLI);
                },
                36..=61 => {
                    s.push((ch - 36 + b'a') as char);
                },
                _ => unreachable!("invalid character: {}", ch),
            }
        }
        s
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
