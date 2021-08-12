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

        // Sort by the parsed word parts (no joining)
        entries.sort_by(|a, b| {
            a.parsed_parts().cmp(b.parsed_parts())
                .then_with(|| a.subword.cmp(&b.subword))
        });

        eprintln!(" => {} entries", entries.len());
        entries
    };
}

pub fn words() -> impl Iterator<Item = (&'static Word, Loc)> {
    ENTRIES.iter()
        .enumerate()
        .flat_map(|(a, entry)|
            entry.parsed_word.iter()
                .map(move |&word| (word, Loc { entry: a as u16, word: NO_WORD })))
}

pub fn definition_words() -> impl Iterator<Item = (&'static Word, Loc)> {
    ENTRIES.iter()
        .enumerate()
        .flat_map(|(a, entry)|
            entry.parsed_text.iter()
                .enumerate()
                .map(move |(b, &word)| (word, Loc { entry: a as u16, word: b as u16 })))
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
    sub: Option<u8>,
    secs: Vec<Vec<Vec<(SegmentKind, String)>>>,
}

impl RawEntry {
    fn words(word: &str) -> impl Iterator<Item = &str> {
        word.split(&[',', ';'][..]).map(str::trim)
    }

    fn parts(word: &str) -> impl Iterator<Item = &str> {
        word.split(Self::skip_char)
            .filter(|s| !s.is_empty())
    }

    fn skip_char(ch: char) -> bool {
        ch.is_ascii() && !ch.is_ascii_alphabetic()
    }

    fn joined_subwords(word: &str) -> Vec<&'static Word> {
        // Split the word into parts
        let mut parts = Self::parts(word);

        // Take the first part as the base word
        let first = parts.next().unwrap_or("");
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
        parsed.into_iter()
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
    pub word_ranges: Box<[(u32, u32)]>,
    pub sections: Box<[Section]>,
}

impl Entry {
    pub fn words(&self) -> impl Iterator<Item = &str> {
        RawEntry::words(&self.word)
    }

    pub fn parsed_parts<'a>(&'a self) -> impl Iterator<Item = Vec<Box<Word>>> + 'a {
        self.words()
            .map(|word| {
                RawEntry::parts(word)
                    .map(Word::parse)
                    .collect()
            })
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

        // Parse the words in the text, recording their start and end indices
        let mut parsed_text = Vec::new();
        let mut word_ranges = Vec::new();
        let mut chars = text.char_indices().peekable();
        loop {
            // Skip non-word characters
            while let Some(_) = chars.next_if(|&(_, ch)| RawEntry::skip_char(ch)) {}

            if let Some((start, _)) = chars.next() {
                // Skip word characters
                while let Some(_) = chars.next_if(|&(_, ch)| !RawEntry::skip_char(ch)) {}

                let end = chars.next()
                    .map(|(i, _)| i)
                    .unwrap_or(text.len());

                // Push the parsed word and the indices
                parsed_text.push(intern::word(Word::parse(&text[start..end])));
                word_ranges.push((start as u32, end as u32));
            } else {
                break;
            }
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
    pub kind: SegmentKind,
    pub start: u32,
    pub end: u32,
}
