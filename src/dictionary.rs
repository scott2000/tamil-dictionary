use std::fs::File;
use std::io::BufReader;
use std::ops::*;

use rand::seq::SliceRandom;

use serde::Deserialize;

use crate::intern;
use crate::tamil::{Letter, Word};

lazy_static! {
    pub static ref ENTRIES: Box<[Entry]> = {
        eprintln!("Loading dictionary...");

        let file = match File::open("dictionary.json") {
            Ok(file) => file,
            Err(_) => {
                // Exit nicely with an error message if the file doesn't exist
                eprintln!("Cannot open 'dictionary.json'. Make sure the file exists and is in the current directory.");
                std::process::exit(1)
            }
        };

        let mut entries: Box<[Entry]> = serde_json::from_reader(BufReader::new(file))
            .expect("dictionary parse error");

        // Clear the interning metadata since it won't be used anymore
        intern::done();

        // Sort by the parsed words (using natural joining only)
        entries.sort_by(|a, b| {
            a.parsed_words().cmp(b.parsed_words())
                .then_with(|| a.subword.cmp(&b.subword))
        });

        eprintln!(" => {} entries", entries.len());
        entries
    };
}

pub fn words() -> impl Iterator<Item = (&'static Word, Loc)> {
    ENTRIES.iter().enumerate().flat_map(|(a, entry)| {
        entry.parsed_word.iter().map(move |&word| {
            (
                word,
                Loc {
                    entry: a as u16,
                    word: WordData::new(NO_WORD, false),
                },
            )
        })
    })
}

pub fn definition_words() -> impl Iterator<Item = (&'static Word, Loc)> {
    ENTRIES.iter().enumerate().flat_map(|(a, entry)| {
        entry
            .parsed_text
            .iter()
            .zip(entry.word_ranges.iter())
            .enumerate()
            .map(move |(b, (&word, range))| {
                (
                    word,
                    Loc {
                        entry: a as u16,
                        word: WordData::new(b as u16, range.in_paren()),
                    },
                )
            })
    })
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Loc {
    pub entry: EntryIndex,
    pub word: WordData,
}

pub type EntryIndex = u16;
pub type WordIndex = u16;

pub const NO_WORD: WordIndex = WordData::PAREN_MASK - 1;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[repr(transparent)]
pub struct WordData(u16);

impl WordData {
    const PAREN_MASK: u16 = 1 << 15;

    pub fn new(index: WordIndex, in_paren: bool) -> Self {
        debug_assert!(index < Self::PAREN_MASK);

        if in_paren {
            Self(index | Self::PAREN_MASK)
        } else {
            Self(index)
        }
    }

    pub fn index(&self) -> WordIndex {
        self.0 & !Self::PAREN_MASK
    }

    pub fn in_paren(&self) -> bool {
        (self.0 & Self::PAREN_MASK) != 0
    }
}

#[derive(Deserialize)]
struct RawEntry {
    word: String,
    sub: Option<u8>,
    hint: Option<String>,
    kind: Vec<RawEntryKind>,
    secs: Vec<Vec<Vec<(SegmentKind, String)>>>,
}

#[derive(Deserialize, Copy, Clone, PartialEq, Eq, Debug)]
pub enum RawEntryKind {
    #[serde(rename = "v")]
    VinaiChol,
    #[serde(rename = "va")]
    VinaiAdai,
    #[serde(rename = "vm")]
    VinaiMutru,
    #[serde(rename = "tv")]
    ThunaiVinai,
    #[serde(rename = "p")]
    PeyarChol,
    #[serde(rename = "pa")]
    PeyarAdai,
    #[serde(rename = "sp")]
    SuttuPeyarChol,
    #[serde(rename = "spa")]
    SuttuPeyarAdai,
    #[serde(rename = "vp")]
    VinaaPeyarChol,
    #[serde(rename = "vpa")]
    VinaaPeyarAdai,
    #[serde(rename = "i")]
    IdaiChol,
    #[serde(rename = "ii")]
    InaiIdaiChol,
    #[serde(rename = "vi")]
    ViliIdaiChol,
}

impl RawEntryKind {
    pub fn to_str(self) -> &'static str {
        use RawEntryKind::*;
        match self {
            VinaiChol => "வி.",
            VinaiAdai => "வி.அ.",
            VinaiMutru => "வி.மு.",
            ThunaiVinai => "து.வி.",
            PeyarChol => "பெ.",
            PeyarAdai => "பெ.அ.",
            SuttuPeyarChol => "சு.பெ.",
            SuttuPeyarAdai => "சு.பெ.அ.",
            VinaaPeyarChol => "வினா பெ.",
            VinaaPeyarAdai => "வினா பெ.அ.",
            IdaiChol => "இ.சொ.",
            InaiIdaiChol => "இணை இ.சொ.",
            ViliIdaiChol => "விளி இ.சொ.",
        }
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
    #[serde(rename = "wbr")]
    WordBreak,
}

#[derive(Deserialize, Debug)]
#[serde(from = "RawEntry")]
pub struct Entry {
    pub word: Box<str>,
    pub parsed_word: Box<[&'static Word]>,
    pub subword: Option<u8>,
    pub hint: Option<Box<str>>,
    pub kind_strs: Box<[&'static str]>,
    pub kind_set: KindSet,
    pub text: Box<str>,
    pub parsed_text: Box<[&'static Word]>,
    pub word_ranges: Box<[WordRange]>,
    pub sections: Box<[Section]>,
}

impl Entry {
    pub fn words(word: &str) -> impl Iterator<Item = &str> {
        word.split(&[',', ';'][..])
            .map(str::trim)
            .filter(|s| !s.is_empty())
    }

    pub fn joined_subwords(word: &str) -> Vec<Box<Word>> {
        // Split the word into parts
        let mut parts = Self::parts(word);

        // Take the first part as the base word
        let first = parts.next().expect("word has no parts");
        let mut parsed = vec![Word::parse_unboxed(first)];

        // Add on all suffixes as necessary
        for part in parts {
            let mut result = Vec::new();
            for word in parsed {
                result.append(&mut Letter::join(word, &Word::parse(part)));
            }
            parsed = result;
        }

        // Intern the resulting words
        parsed.into_iter().map(|word| word.into()).collect()
    }

    pub fn random() -> &'static Self {
        ENTRIES.choose(&mut rand::thread_rng()).unwrap()
    }

    pub fn primary_word(&self) -> &str {
        Self::words(&self.word).next().unwrap()
    }

    fn parsed_words(&self) -> impl Iterator<Item = Box<Word>> + '_ {
        Self::words(&self.word).map(Word::parse)
    }

    fn kind_strs(kind: &[RawEntryKind]) -> Vec<&'static str> {
        kind.iter().copied().map(RawEntryKind::to_str).collect()
    }

    fn kind_set(kind: &[RawEntryKind]) -> KindSet {
        kind.iter()
            .copied()
            .map(KindSet::from)
            .reduce(KindSet::bitor)
            .unwrap_or(KindSet::empty())
    }

    fn parts(word: &str) -> impl Iterator<Item = &str> {
        word.split(Self::skip_char).filter(|s| !s.is_empty())
    }

    fn skip_char(ch: char) -> bool {
        ch.is_ascii() && !ch.is_ascii_alphabetic()
    }
}

impl From<RawEntry> for Entry {
    fn from(raw: RawEntry) -> Self {
        let parsed_word: Box<[_]> = Self::words(&raw.word)
            .flat_map(Self::joined_subwords)
            .map(intern::word)
            .collect();

        assert!(!parsed_word.is_empty());

        // Load all of the text of the sections, and set the segments to refer to indices
        let mut text = String::new();
        let sections: Box<[_]> = raw
            .secs
            .into_iter()
            .map(|sec| {
                let section: Box<[_]> = sec
                    .into_iter()
                    .map(|para| {
                        para.into_iter()
                            .flat_map(|(kind, s)| {
                                let start = text.len();
                                text.push_str(&s);
                                let end = text.len();
                                text.push('\0');
                                Segment::new(kind, start, end)
                            })
                            .collect()
                    })
                    .collect();

                assert!(!section.is_empty());

                section
            })
            .collect();

        assert!(!sections.is_empty());

        // Parse the words in the text, recording their start and end indices
        let mut parsed_text = Vec::new();
        let mut word_ranges = Vec::new();
        let mut paren_depth = 0;
        let mut chars = text.char_indices().peekable();
        loop {
            // Skip non-word characters
            while let Some((_, ch)) = chars.next_if(|&(_, ch)| Self::skip_char(ch)) {
                match ch {
                    '(' => paren_depth += 1,
                    ')' => {
                        if paren_depth == 0 {
                            eprintln!("Warning: unmatched closing parenthesis in {}", raw.word);
                        } else {
                            paren_depth -= 1;
                        }
                    }
                    _ => {}
                }
            }

            if let Some((start, _)) = chars.next() {
                // Skip word characters
                while chars.next_if(|&(_, ch)| !Self::skip_char(ch)).is_some() {}

                let end = chars.peek().map(|&(i, _)| i).unwrap_or_else(|| text.len());

                // Push the parsed word and the indices
                parsed_text.push(intern::word(Word::parse(&text[start..end])));
                word_ranges.push(WordRange::new(start, end, paren_depth > 0));
            } else {
                break;
            }
        }

        if paren_depth != 0 {
            eprintln!("Warning: unmatched opening parenthesis in {}", raw.word);
        }

        let kind_strs = Self::kind_strs(&raw.kind).into_boxed_slice();
        let kind_set = Self::kind_set(&raw.kind);

        Self {
            word: raw.word.into_boxed_str(),
            parsed_word,
            subword: raw.sub,
            hint: raw.hint.map(String::into_boxed_str),
            kind_strs,
            kind_set,
            text: text.into_boxed_str(),
            parsed_text: parsed_text.into_boxed_slice(),
            word_ranges: word_ranges.into_boxed_slice(),
            sections,
        }
    }
}

pub const ALL_KINDS: [EntryKind; 11] = [
    EntryKind::VinaiChol,
    EntryKind::VinaiAdai,
    EntryKind::VinaiMutru,
    EntryKind::ThunaiVinai,
    EntryKind::PeyarChol,
    EntryKind::PeyarAdai,
    EntryKind::SuttuPeyar,
    EntryKind::VinaaPeyar,
    EntryKind::IdaiChol,
    EntryKind::InaiIdaiChol,
    EntryKind::ViliIdaiChol,
];

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u32)]
pub enum EntryKind {
    VinaiChol = 1 << 0,
    VinaiAdai = 1 << 1,
    VinaiMutru = 1 << 2,
    ThunaiVinai = 1 << 3,
    PeyarChol = 1 << 4,
    PeyarAdai = 1 << 5,
    SuttuPeyar = 1 << 6,
    VinaaPeyar = 1 << 7,
    IdaiChol = 1 << 8,
    InaiIdaiChol = 1 << 9,
    ViliIdaiChol = 1 << 10,
}

impl EntryKind {
    pub fn parse(s: &str) -> Option<Self> {
        use EntryKind::*;
        match s {
            "v" => Some(VinaiChol),
            "va" => Some(VinaiAdai),
            "vm" => Some(VinaiMutru),
            "tv" => Some(ThunaiVinai),
            "p" => Some(PeyarChol),
            "pa" => Some(PeyarAdai),
            "sp" => Some(SuttuPeyar),
            "vp" => Some(VinaaPeyar),
            "i" => Some(IdaiChol),
            "ii" => Some(InaiIdaiChol),
            "vi" => Some(ViliIdaiChol),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct KindSet(pub u32);

impl KindSet {
    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn any() -> Self {
        Self(!0)
    }

    pub const fn single(kind: EntryKind) -> Self {
        Self(kind as u32)
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub const fn to_non_empty(self) -> Self {
        if self.is_empty() {
            Self::any()
        } else {
            self
        }
    }

    pub const fn matches(self, kind: EntryKind) -> bool {
        (self.0 & kind as u32) != 0
    }

    pub const fn matches_any(self, other: Self) -> bool {
        (self.0 & other.0) != 0
    }
}

impl From<RawEntryKind> for KindSet {
    fn from(kind: RawEntryKind) -> Self {
        use EntryKind::*;
        match kind {
            RawEntryKind::VinaiChol => Self(VinaiChol as u32),
            RawEntryKind::VinaiAdai => Self(VinaiAdai as u32),
            RawEntryKind::VinaiMutru => Self(VinaiMutru as u32),
            RawEntryKind::ThunaiVinai => Self(ThunaiVinai as u32 | VinaiChol as u32),
            RawEntryKind::PeyarChol => Self(PeyarChol as u32),
            RawEntryKind::PeyarAdai => Self(PeyarAdai as u32),
            RawEntryKind::SuttuPeyarChol => Self(SuttuPeyar as u32 | PeyarChol as u32),
            RawEntryKind::SuttuPeyarAdai => Self(SuttuPeyar as u32 | PeyarAdai as u32),
            RawEntryKind::VinaaPeyarChol => Self(VinaaPeyar as u32 | PeyarChol as u32),
            RawEntryKind::VinaaPeyarAdai => Self(VinaaPeyar as u32 | PeyarAdai as u32),
            RawEntryKind::IdaiChol => Self(IdaiChol as u32),
            RawEntryKind::InaiIdaiChol => Self(InaiIdaiChol as u32 | IdaiChol as u32),
            RawEntryKind::ViliIdaiChol => Self(ViliIdaiChol as u32 | IdaiChol as u32),
        }
    }
}

impl BitOr for KindSet {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

impl BitAnd for KindSet {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }
}

pub type Section = Box<[Paragraph]>;

pub type Paragraph = Box<[Segment]>;

#[derive(Debug)]
pub struct Segment {
    kind: SegmentKind,
    start: u32,
    end: u32,
}

impl Segment {
    pub fn new(kind: SegmentKind, start: usize, end: usize) -> Option<Self> {
        if start == end {
            return None;
        }

        debug_assert!(start < end);
        debug_assert!(end <= u32::MAX as usize);

        Some(Self {
            kind,
            start: start as u32,
            end: end as u32,
        })
    }

    pub fn kind(&self) -> SegmentKind {
        self.kind
    }

    pub fn start(&self) -> usize {
        self.start as usize
    }

    pub fn end(&self) -> usize {
        self.end as usize
    }
}

#[derive(Copy, Clone, Debug)]
pub struct WordRange {
    start: u32,
    end: u32,
}

impl WordRange {
    const PAREN_MASK: u32 = 1 << 31;

    pub fn new(start: usize, end: usize, in_paren: bool) -> Self {
        debug_assert!(start < end);
        debug_assert!(end < Self::PAREN_MASK as usize);

        let mut end = end as u32;
        if in_paren {
            end |= Self::PAREN_MASK;
        }

        Self {
            start: start as u32,
            end,
        }
    }

    pub fn start(&self) -> usize {
        self.start as usize
    }

    pub fn end(&self) -> usize {
        (self.end & !Self::PAREN_MASK) as usize
    }

    pub fn in_paren(&self) -> bool {
        (self.end & Self::PAREN_MASK) != 0
    }
}
