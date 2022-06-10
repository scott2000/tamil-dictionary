use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs::File;
use std::io::BufReader;

use serde::Deserialize;

use crate::dictionary::{Entry, EntryIndex, EntryKind, ENTRIES};
use crate::intern;
use crate::tamil::{Letter, LetterSet, Word};

type Ves = HashMap<String, BTreeSet<&'static Word>>;

type Verbs = HashMap<(String, Option<u8>), Vec<String>>;

pub type Stems = HashMap<Box<Word>, Vec<StemData>>;

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
        let mut ves = Ves::new();

        let mut verbs = Verbs::new();

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

        let mut stems = Stems::new();
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
                let stem = stem.into();
                let data = StemData {
                    word,
                    entry,
                    kind: self.finalize(ves, verbs, entry, word),
                };

                if let Some(datas) = stems.get_mut(&stem) {
                    datas.push(data);
                } else {
                    stems.insert(stem, vec![data]);
                }
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

// TODO: remove pub for all of these

type Choices = Vec<EntryIndex>;

type ChoiceMap = BTreeMap<usize, Vec<Choices>>;

pub fn all_choices(full_word: &Word) -> Vec<Choices> {
    let map = ChoiceMap::new();
    map.insert(0, vec![Vec::new()]);

    // TODO: use pop_first when stabilized! (nightly: map_first_last)
    while let Some((&end, _)) = map.iter().next() {
        let after = choices_after(full_word, end);

        for choices in map.remove(&end).unwrap() {
            for choice in after.iter() {
                let mut choices = choices.clone();
                choices.push(choice.entry);

                if let Some(vec) = map.get_mut(&choice.end) {
                    vec.push(choices);
                } else {
                    map.insert(choice.end, vec![choices]);
                }
            }
        }
    }

    Vec::new()
}

pub fn choices_after(full_word: &Word, start: usize) -> Vec<Choice> {
    let mut choices = Vec::new();

    for mut end in (start + 1)..=full_word.len() {
        let ex = Expand::new(full_word, start);
        let stem = &full_word[start..end];
        ex.evaluate(&mut choices);
        todo!();
    }

    choices
}

pub struct Choice {
    pub end: usize,
    pub entry: EntryIndex,
}

#[derive(Clone)]
pub struct Expand<'a> {
    pub full_word: &'a Word,
    pub start: usize,
    pub choices: Vec<ExpandChoice>,
}

impl<'a> Expand<'a> {
    pub fn new(full_word: &'a Word, start: usize) -> Self {
        Self {
            full_word,
            start,
            choices: Vec::new(),
        }
    }

    pub fn push(&mut self, choice: ExpandChoice) {
        self.choices.push(choice);
    }

    pub fn evaluate(&mut self, results: &mut Vec<Choice>) {
        while let Some(choice) = self.choices.pop() {
            if let Some(choice) = choice.step(self) {
                results.push(choice);
            }
        }
    }
}

#[derive(Copy, Clone)]
pub enum ExpandState {
    AdverbStem,
    InfinitiveStem,
    Negative,
    Infinitive,
    TenseStem,
    FutureStem,
    SpecialA,
    SpecialB,
    SpecialC,
    GeneralStem,
    GeneralStemA,
    GeneralStemI,
    GeneralStemE,
    GeneralStemO,
    GeneralStemNgal,
    GeneralStemPlural,
    Plural,
    Oblique,
    Case,
    Irundhu,
    MaybeAdjective,
    AdjectiveStem,
    AdjectiveStemVa,
    AdjectiveStemAdhu,
    Em,
    Part,
    Done,
}

#[derive(Copy, Clone)]
pub struct ExpandChoice {
    pub end: usize,
    pub entry: EntryIndex,
    pub is_start: bool,
    pub has_implicit_u: bool,
    pub state: ExpandState,
}

impl ExpandChoice {
    pub fn matched<'a>(&self, ex: &Expand<'a>) -> &'a Word {
        &ex.full_word[ex.start..self.end]
    }

    pub fn last(&self, ex: &Expand) -> Option<Letter> {
        if self.has_implicit_u {
            return Some(Letter::U);
        }

        self.matched(ex).last()
    }

    pub fn end_matches(&self, ex: &Expand, suffix: LetterSet) -> bool {
        if self.has_implicit_u {
            return suffix.matches(Letter::U);
        }

        self.matched(ex).end_matches(suffix)
    }

    pub fn ends_with(&self, ex: &Expand, mut suffix: &Word) -> bool {
        if self.has_implicit_u {
            if let Some(new_suffix) = suffix.strip_suffix(word![U]) {
                suffix = new_suffix;
            } else {
                return false;
            }
        }

        self.matched(ex).ends_with(suffix)
    }

    pub fn goto(&self, ex: &mut Expand, states: &[ExpandState]) {
        for &state in states {
            ex.push(Self {
                is_start: false,
                state,
                ..*self
            });
        }
    }

    pub fn add_goto(&self, ex: &mut Expand, suffix: &Word, states: &[ExpandState]) {
        use Letter::*;

        if suffix.is_empty() {
            self.goto(ex, states);
            return;
        }

        // Remove trailing U from suffix
        self.has_implicit_u = if let Some(new_suffix) = suffix.strip_suffix(word![U]) {
            suffix = new_suffix;
            true
        } else {
            false
        };

        // Match all other letters in suffix
        let word = ex.full_word;
        for lt in suffix.iter() {
            if word.get(self.end) != Some(lt) {
                return;
            }

            self.end += 1;
        }

        // Add shortest match (without final U, glide insertion, or doubling)
        self.goto(ex, states);

        // Add match with final U
        if self.has_implicit_u {
            if word.get(self.end) != Some(U) {
                return;
            }

            self.end += 1;
            self.has_implicit_u = false;
            self.goto(ex, states);
        }

        // The remaining cases all require this to be true
        if !word.matches(self.end + 1, LetterSet::vowel()) {
            return;
        }

        let prev = word[self.end - 1];

        if LetterSet::vowel().matches(prev) {
            // Handle insertion of Y and V glides
            let glide = if letterset![I, LongI, E, LongE, Ai].matches(prev) {
                Y
            } else {
                V
            };

            if word.get(self.end) == Some(glide) {
                self.end += 1;
                self.goto(ex, states);
            }
        } else if self.is_start && word.get(self.end) == Some(prev) {
            // Handle doubling of final consonant
            self.end += 1;
            self.goto(ex, states);
        }
    }

    pub fn step(&self, ex: &mut Expand) -> Option<Choice> {
        use ExpandState::*;
        use Letter::*;

        match self.state {
            AdverbStem => match self.last(ex) {
                Some(U) => self.goto(ex, &[TenseStem, SpecialA]),

                Some(I) => {
                    self.add_goto(ex, word![AlveolarN], &[TenseStem, SpecialB]);
                    self.add_goto(ex, word![Y, A], &[AdjectiveStem, Done]);
                    self.add_goto(ex, word![AlveolarR, AlveolarR, U], &[Done]);
                }

                _ => {
                    self.add_goto(ex, word![AlveolarN], &[TenseStem, SpecialB]);
                    self.add_goto(ex, word![Y, I, AlveolarR, AlveolarR, U], &[Done]);
                    self.add_goto(ex, word![Y, I, AlveolarN], &[TenseStem, SpecialB]);
                }
            },

            InfinitiveStem => {
                if self.ends_with(ex, word![K, U]) {
                    self.add_goto(ex, word![I, AlveolarR, U], &[TenseStem]);
                    self.add_goto(
                        ex,
                        word![I, AlveolarN, AlveolarR, U],
                        &[TenseStem, SpecialA],
                    );
                } else {
                    self.add_goto(ex, word![K, I, AlveolarR, U], &[TenseStem]);
                    self.add_goto(
                        ex,
                        word![K, I, AlveolarN, AlveolarR, U],
                        &[TenseStem, SpecialA],
                    );
                }

                if self.end_matches(ex, letterset![R, Zh]) {
                    self.add_goto(ex, word![U, K, I, AlveolarR, U], &[TenseStem]);
                    self.add_goto(
                        ex,
                        word![U, K, I, AlveolarN, AlveolarR, U],
                        &[TenseStem, SpecialA],
                    );
                }

                self.add_goto(ex, word![Ai], &[Oblique]);
                self.add_goto(ex, word![LongA], &[Negative]);
                self.add_goto(ex, word![U, M], &[Part]);
                self.add_goto(ex, word![A], &[Infinitive]);
            }

            Negative => {
                self.goto(ex, &[Done]);

                self.add_goto(ex, word![M, Ai], &[Oblique]);
                self.add_goto(ex, word![M, A, AlveolarL], &[Oblique]);
                self.add_goto(ex, word![T, A], &[AdjectiveStem, Done]);
                self.add_goto(ex, word![T, U], &[Em]);
            }

            Infinitive => {
                self.goto(ex, &[Em]);

                self.add_goto(ex, word![AlveolarL], &[Oblique]);
                self.add_goto(ex, word![RetroT, RetroT, U, M], &[Em]);
                self.add_goto(ex, word![AlveolarL, LongA, M], &[Em]);
            }

            TenseStem => {
                self.goto(ex, &[GeneralStem]);
                self.add_goto(ex, word![A], &[AdjectiveStem, Done]);
            }

            FutureStem => {
                self.goto(ex, &[GeneralStem, SpecialA, SpecialB]);
                self.add_goto(ex, word![A], &[AdjectiveStem]);
            }

            SpecialA => {
                self.add_goto(ex, word![A, AlveolarN, A], &[SpecialC]);
            }

            SpecialB => {
                self.add_goto(ex, word![A], &[SpecialC]);
            }

            SpecialC => {
                self.goto(ex, &[Part]);

                self.add_goto(ex, word![RetroL], &[Part]);
                self.add_goto(ex, word![AlveolarN], &[Part]);
                self.add_goto(ex, word![R], &[Part]);
            }

            GeneralStem => {
                self.add_goto(ex, word![LongO], &[GeneralStemO]);
                self.add_goto(ex, word![LongE], &[GeneralStemE]);
                self.add_goto(ex, word![LongI], &[GeneralStemI, GeneralStemNgal]);
                self.add_goto(ex, word![LongA], &[GeneralStemA, GeneralStemNgal]);

                self.add_goto(ex, word![A, T, U], &[Part, GeneralStemNgal]);
            }

            GeneralStemA => {
                self.add_goto(ex, word![Y], &[Part]);
                self.add_goto(ex, word![AlveolarN], &[Part]);
                self.add_goto(ex, word![AlveolarL], &[Part]);
                self.add_goto(ex, word![R], &[Part, GeneralStemPlural]);
            }

            GeneralStemI => {
                self.add_goto(ex, word![R], &[Part, GeneralStemPlural]);
            }

            GeneralStemE => {
                self.add_goto(ex, word![M], &[Part]);
                self.add_goto(ex, word![AlveolarN], &[Part]);
            }

            GeneralStemO => {
                self.add_goto(ex, word![RetroL], &[Oblique]);
                self.add_goto(ex, word![AlveolarN], &[Oblique]);
                self.add_goto(ex, word![R], &[Oblique]);
                self.add_goto(ex, word![M], &[Part]);
            }

            GeneralStemNgal => {
                self.goto(ex, &[GeneralStemPlural]);
                self.add_goto(ex, word![Ng], &[GeneralStemPlural]);
            }

            GeneralStemPlural => {
                self.add_goto(ex, word![K, A, RetroL], &[Part]);
            }

            Plural => {
                self.goto(ex, &[Oblique]);
                self.add_goto(ex, word![K, A, RetroL], &[Oblique]);
            }

            Oblique => {
                self.goto(ex, &[Case]);
                self.add_goto(ex, word![I, AlveolarN], &[Case]);
            }

            Case => {
                self.goto(ex, &[Em]);

                self.add_goto(ex, word![Ai], &[Em]);

                self.add_goto(ex, word![K, LongU, RetroT, A], &[Em]);
                self.add_goto(ex, word![U, RetroT, AlveolarN], &[Em]);
                self.add_goto(ex, word![LongO, RetroT, U], &[Em]);
                self.add_goto(ex, word![LongA, AlveolarL], &[Em]);

                self.add_goto(ex, word![K, K, U], &[Em]);
                if !self.end_matches(ex, letterset![I, LongI, Ai]) {
                    self.add_goto(ex, word![U, K, K, U], &[Em]);
                }

                self.add_goto(ex, word![I, AlveolarL], &[Irundhu]);
                self.add_goto(ex, word![I, RetroT, A, M], &[Irundhu]);
                self.add_goto(ex, word![K, I, RetroT, RetroT, LongE], &[Irundhu]);
            }

            Irundhu => {
                self.goto(ex, &[Em]);
                self.add_goto(ex, word![I, R, U, N, T, U], &[MaybeAdjective]);
            }

            MaybeAdjective => {
                self.goto(ex, &[Em]);
                self.add_goto(ex, word![A], &[AdjectiveStem, Done]);
            }

            AdjectiveStem => {
                if !self.ends_with(ex, word![V, A]) {
                    self.add_goto(ex, word![V, A], &[AdjectiveStemVa]);
                }

                self.add_goto(ex, word![T, U], &[AdjectiveStemAdhu]);
            }

            AdjectiveStemVa => {
                self.add_goto(ex, word![Ai], &[Em]);
                self.add_goto(ex, word![Ai, K, A, RetroL], &[Oblique]);
                self.add_goto(ex, word![AlveolarR, AlveolarR, U], &[Oblique]);

                self.add_goto(ex, word![RetroL], &[Oblique]);
                self.add_goto(ex, word![AlveolarN], &[Oblique]);
                self.add_goto(ex, word![R], &[Plural]);
            }

            AdjectiveStemAdhu => {
                self.goto(ex, &[Case]);

                self.add_goto(ex, word![A, AlveolarN], &[Case]);
                self.add_goto(ex, word![A, AlveolarR, K, U], &[Em]);
            }

            Em => {
                self.goto(ex, &[Part]);
                self.add_goto(ex, word![U, M], &[Part]);
            }

            Part => {
                self.goto(ex, &[Done]);

                self.add_goto(ex, word![LongA], &[Part]);
                self.add_goto(ex, word![LongE], &[Part]);
                self.add_goto(ex, word![LongO], &[Part]);
            }

            Done => {
                return Some(Choice {
                    end: self.end,
                    entry: self.entry,
                });
            }
        }

        None
    }
}
