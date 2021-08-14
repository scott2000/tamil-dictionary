use std::fs::File;

use serde::Deserialize;

use crate::intern;
use crate::tamil::{Letter, Word};

lazy_static! {
    pub static ref ENTRIES: Box<[Entry]> = {
        eprintln!("Loading dictionary...");

        let file = File::open("dictionary.json")
            .expect("missing dictionary.json");

        let mut entries: Box<[Entry]> = serde_json::from_reader(file)
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
    secs: Vec<Vec<Vec<(SegmentKind, String)>>>,
}

impl RawEntry {
    fn words(word: &str) -> impl Iterator<Item = &str> {
        word.split(&[',', ';'][..])
            .map(str::trim)
            .filter(|s| !s.is_empty())
    }

    fn parts(word: &str) -> impl Iterator<Item = &str> {
        word.split(Self::skip_char).filter(|s| !s.is_empty())
    }

    fn skip_char(ch: char) -> bool {
        ch.is_ascii() && !ch.is_ascii_alphabetic()
    }

    fn joined_subwords(word: &str) -> Vec<&'static Word> {
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
        parsed
            .into_iter()
            .map(|word| intern::word(word.into()))
            .collect()
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
    pub text: Box<str>,
    pub parsed_text: Box<[&'static Word]>,
    pub word_ranges: Box<[WordRange]>,
    pub sections: Box<[Section]>,
}

impl Entry {
    pub fn primary_word(&self) -> &str {
        RawEntry::words(&self.word).next().unwrap()
    }

    fn parsed_words<'a>(&'a self) -> impl Iterator<Item = Box<Word>> + 'a {
        RawEntry::words(&self.word).map(Word::parse)
    }
}

impl From<RawEntry> for Entry {
    fn from(raw: RawEntry) -> Self {
        let parsed_word: Box<[_]> = RawEntry::words(&raw.word)
            .flat_map(|word| RawEntry::joined_subwords(word))
            .collect();

        assert!(!parsed_word.is_empty());

        // Load all of the text of the sections, and set the segments to refer to indices
        let mut text = String::new();
        let sections: Box<[_]> = raw
            .secs
            .into_iter()
            .map(|sec| {
                sec.into_iter()
                    .map(|para| {
                        para.into_iter()
                            .map(|(kind, s)| {
                                let start = text.len();
                                text.push_str(&s);
                                let end = text.len();
                                text.push('\0');
                                Segment::new(kind, start, end)
                            })
                            .collect()
                    })
                    .collect()
            })
            .inspect(|sec: &Box<[_]>| assert!(!sec.is_empty()))
            .collect();

        assert!(!sections.is_empty());

        // Parse the words in the text, recording their start and end indices
        let mut parsed_text = Vec::new();
        let mut word_ranges = Vec::new();
        let mut paren_depth = 0;
        let mut chars = text.char_indices().peekable();
        loop {
            // Skip non-word characters
            while let Some((_, ch)) = chars.next_if(|&(_, ch)| RawEntry::skip_char(ch)) {
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
                while let Some(_) = chars.next_if(|&(_, ch)| !RawEntry::skip_char(ch)) {}

                let end = chars.peek().map(|&(i, _)| i).unwrap_or(text.len());

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

        Self {
            word: raw.word.into_boxed_str(),
            parsed_word,
            subword: raw.sub,
            text: text.into_boxed_str(),
            parsed_text: parsed_text.into_boxed_slice(),
            word_ranges: word_ranges.into_boxed_slice(),
            sections,
        }
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
    pub fn new(kind: SegmentKind, start: usize, end: usize) -> Self {
        debug_assert!(start < end);
        debug_assert!(end <= u32::MAX as usize);

        Self {
            kind,
            start: start as u32,
            end: end as u32,
        }
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
