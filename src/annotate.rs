use std::collections::{BTreeSet, HashMap};
use std::fs::File;
use std::io::BufReader;

use serde::Deserialize;

use crate::dictionary::{Entry, EntryKind, ENTRIES};
use crate::intern;
use crate::tamil::{Letter, Word};

type Ves = HashMap<String, BTreeSet<&'static Word>>;

type Verbs = HashMap<(String, Option<u8>), Vec<String>>;

pub type Stems = HashMap<Box<Word>, StemData>;

lazy_static! {
    pub static ref STEMS: Stems = {
        lazy_static::initialize(&ENTRIES);
        eprintln!("Loading verbs...");

        let file = match File::open("verbs.json") {
            Ok(file) => file,
            Err(_) => return HashMap::new(),
        };

        let verb_list: Vec<RawVerbData> = serde_json::from_reader(BufReader::new(file))
            .expect("verbs parse error");

        // Vinaiyecham map for dealing with suffixes
        let mut ves: Ves = HashMap::new();

        let mut verbs: Verbs = HashMap::new();

        for mut verb in verb_list {
            for ve in verb.ve.iter_mut() {
                if ve.starts_with('-') {
                    continue;
                }

                if let Some(index) = verb.word.rfind(' ') {
                    ve.insert_str(0, &verb.word[..=index]);
                    continue;
                }

                let word = intern::word(normalize(&Word::parse(&verb.word)));
                if let Some(set) = ves.get_mut(ve) {
                    set.insert(word);
                } else {
                    let mut set = BTreeSet::new();
                    set.insert(word);
                    ves.insert(ve.clone(), set);
                }
            }

            verbs.insert((verb.word, verb.sub), verb.ve);
        }

        let mut stems: Stems = HashMap::new();
        for entry in ENTRIES.iter() {
            StemData::parse(&mut stems, &ves, &verbs, entry);
        }

        intern::done();

        stems
    };
}

pub fn normalize(mut word: &Word) -> Box<Word> {
    use Letter::*;

    lazy_static! {
        static ref NORMALIZE: HashMap<(Letter, Letter), (Letter, Letter)> = {
            let mut map = HashMap::new();

            map.insert((Ng, K), (M, K));
            map.insert((Ny, Ch), (M, Ch));
            map.insert((N, T), (M, T));

            for right in [K, Ch, P] {
                map.insert((RetroT, right), (RetroL, right));
                map.insert((AlveolarR, right), (AlveolarL, right));
            }

            map
        };
    }

    let mut letters = Vec::with_capacity(word.len());

    if word.starts_with(word![A, Y, Y]) {
        letters.extend_from_slice(&[Ai, Y]);
        word = &word[3..];
    }

    let normalize: &HashMap<_, _> = &NORMALIZE;
    let mut iter = word.iter();
    while let Some(lt) = iter.next() {
        use Letter::*;

        if let Some(next) = iter.peek() {
            if let Some(&(lt, next)) = normalize.get(&(lt, next)) {
                iter.next();
                letters.extend_from_slice(&[lt, next]);
                continue;
            }
        }

        if lt == Au {
            letters.extend_from_slice(&[A, V, U]);
            continue;
        }

        letters.push(lt);
    }

    letters.into()
}

#[derive(Deserialize)]
struct RawVerbData {
    word: String,
    sub: Option<u8>,
    ve: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct StemData {
    pub word: &'static Word,
    pub entry: &'static Entry,
    pub kind: StemKind,
}

impl StemData {
    pub fn parse(stems: &mut Stems, ves: &Ves, verbs: &Verbs, entry: &'static Entry) {
        use EntryKind::*;
        use StemKind::*;

        let words: Vec<_> = Entry::words(&entry.word).collect();

        let kind = if entry.kind_set.matches(PeyarAdai) {
            if words.len() == 2 && entry.kind_set.matches(VinaiAdai) {
                AdverbOrParticle.stem(stems, ves, verbs, entry, words[0]);
                Adjective.stem(stems, ves, verbs, entry, words[1]);

                return;
            } else {
                Adjective
            }
        } else if entry.kind_set.matches(VinaiChol) {
            Verb(VerbData::default())
        } else if entry.kind_set.matches(PeyarChol) {
            if entry.kind_set.matches(SuttuPeyar) {
                Pronoun
            } else {
                Noun
            }
        } else {
            AdverbOrParticle
        };

        for word in words {
            kind.stem(stems, ves, verbs, entry, word);
        }
    }
}

#[derive(Clone, Debug)]
pub enum StemKind {
    Noun,
    Pronoun,
    Adjective,
    AdverbOrParticle,
    Verb(VerbData),
}

impl StemKind {
    pub fn stem(
        &self,
        stems: &mut Stems,
        ves: &Ves,
        verbs: &Verbs,
        entry: &'static Entry,
        word: &str,
    ) {
        for word in Entry::joined_subwords(word) {
            for (word, stem) in self.stem_word(&word) {
                stems.insert(
                    stem.into(),
                    StemData {
                        word,
                        entry,
                        kind: self.finalize(ves, verbs, entry, word),
                    },
                );
            }
        }
    }

    pub fn stem_word(&self, word: &Word) -> Vec<(&'static Word, &'static Word)> {
        use Letter::*;

        let word = intern::word(normalize(word));

        if word.last() == Some(U) {
            return vec![(word, &word[..(word.len() - 1)])];
        }

        match self {
            Self::Noun => {
                let replace = word
                    .replace_suffix(word![A, AlveolarN], word![A, R])
                    .or_else(|| word.replace_suffix(word![A, R], word![A, AlveolarN]));

                if let Some(other) = replace {
                    let other = intern::word(other);
                    vec![(word, word), (other, other)]
                } else if word.ends_with(word![A, M]) {
                    vec![(word, &word[..(word.len() - 1)])]
                } else {
                    vec![(word, word)]
                }
            }

            Self::AdverbOrParticle => {
                let replace = word
                    .replace_suffix(word![LongA, K, A], word![LongA, Y])
                    .or_else(|| word.replace_suffix(word![LongA, Y], word![LongA, K, A]));

                if let Some(other) = replace {
                    let other = intern::word(other);
                    vec![(word, word), (other, other)]
                } else {
                    vec![(word, word)]
                }
            }

            _ => vec![(word, word)],
        }
    }

    pub fn finalize(
        &self,
        ves: &Ves,
        verbs: &Verbs,
        entry: &'static Entry,
        word: &'static Word,
    ) -> Self {
        match self {
            Self::Adjective if word.last() != Some(Letter::A) => Self::AdverbOrParticle,

            Self::Verb(data) => {
                let mut data = data.clone();
                let ve = &verbs[&(String::from(&*entry.word), entry.subword)];

                data.insert_all(word, ves, ve);
                Self::Verb(data)
            }

            _ => self.clone(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct VerbData {
    pub adv: BTreeSet<&'static Word>,
    pub inf: BTreeSet<&'static Word>,
}

impl VerbData {
    pub fn insert_all(&mut self, word: &'static Word, ves: &Ves, ve: &[String]) {
        for ve in ve {
            let mut parsed = Word::parse(ve);

            if let Some(stripped) = ve.strip_prefix('-') {
                let mut success = false;
                for full in &ves[stripped] {
                    if let Some(stripped) = word.strip_suffix(full) {
                        parsed = stripped + &parsed;
                        success = true;
                        break;
                    }
                }

                if !success {
                    panic!("could not find ending for {}!", ve);
                }
            }

            self.insert(intern::word(normalize(&parsed)));
        }
    }

    pub fn insert(&mut self, ve: &'static Word) {
        use Letter::*;

        match ve.last() {
            Some(U | I | Y) => {
                self.adv.insert(ve);
            }

            Some(A) => {
                self.inf.insert(ve);
            }

            _ => panic!("invalid vinaiyecham: {}", ve),
        }
    }
}
