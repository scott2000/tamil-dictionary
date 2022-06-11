use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs::File;
use std::io::BufReader;

use serde::Deserialize;

use crate::dictionary::{Entry, EntryIndex, EntryKind, KindSet, ENTRIES};
use crate::intern;
use crate::tamil::{Letter, LetterSet, Word};

use ExpandState::*;

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

struct SpecialWord {
    if_matches: KindSet,
    then_insert: Vec<(&'static Word, &'static [ExpandState])>,
}

type Specials = HashMap<&'static Word, SpecialWord>;

fn get_specials() -> Specials {
    use EntryKind::*;
    use ExpandState::*;

    let mut map = Specials::new();

    let pronouns: &[&Word] = &[
        word![Y, LongA, AlveolarN],
        word![N, LongA, AlveolarN],
        word![Y, LongA, M],
        word![N, LongA, M],
        word![N, LongA, Ng, K, A, RetroL],
        word![N, LongI],
        word![N, LongI, R],
        word![N, LongI, Ng, K, A, RetroL],
        word![T, LongA, AlveolarN],
        word![T, LongA, M],
        word![T, LongA, Ng, K, A, RetroL],
    ];

    for &pronoun in pronouns {
        map.insert(
            pronoun,
            SpecialWord {
                if_matches: KindSet::single(SuttuPeyar),
                then_insert: vec![(pronoun, &[Emphasis])],
            },
        );
    }

    let special_pronoun_bases: &[&Word] = &[
        word![E, AlveolarN],
        word![E, M],
        word![N, A, M],
        word![U, AlveolarN],
        word![U, M],
        word![T, A, AlveolarN],
        word![T, A, M],
    ];

    for &base in special_pronoun_bases {
        map.insert(
            base,
            SpecialWord {
                if_matches: KindSet::single(PeyarChol),
                then_insert: vec![
                    (base, &[CaseNoKu]),
                    (intern::word(base + word![A, K, K, U]), &[Emphasis]),
                ],
            },
        );
    }

    let avai_pronouns: &[&Word] = &[
        word![A, V, Ai],
        word![I, V, Ai],
        word![E, V, Ai],
        word![Y, LongA, V, Ai],
    ];

    for &avai in avai_pronouns {
        let without_vai = &avai[..(avai.len() - 2)];

        map.insert(
            avai,
            SpecialWord {
                if_matches: KindSet::single(SuttuPeyar) | KindSet::single(VinaaPeyar),
                then_insert: vec![(without_vai, &[AdjectiveStemAvai])],
            },
        );
    }

    let vowel_adjectives: &[&Word] = &[word![A], word![I], word![E]];

    for &vowel in vowel_adjectives {
        map.insert(
            vowel,
            SpecialWord {
                if_matches: KindSet::single(SuttuPeyar) | KindSet::single(VinaaPeyar),
                then_insert: vec![(vowel, &[VowelAdjective])],
            },
        );
    }

    let aana = word![LongA, N, A];

    map.insert(
        aana,
        SpecialWord {
            if_matches: KindSet::single(IdaiChol),
            then_insert: vec![(aana, &[AdjectiveStem, Done])],
        },
    );

    let ulla = word![U, RetroL, RetroL, A];
    let ullu = word![U, RetroL, RetroL, U];

    map.insert(
        ulla,
        SpecialWord {
            if_matches: KindSet::any(),
            then_insert: vec![(ullu, &[TenseStem, SpecialA, SpecialB])],
        },
    );

    let ellaam = word![E, AlveolarL, AlveolarL, LongA, M];
    let ellaa = word![E, AlveolarL, AlveolarL, LongA];
    let ellaavatru = word![E, AlveolarL, AlveolarL, LongA, V, A, AlveolarR, AlveolarR, U];

    map.insert(
        ellaam,
        SpecialWord {
            if_matches: KindSet::single(PeyarChol),
            then_insert: vec![
                (ellaam, &[Emphasis]),
                (ellaa, &[Done]),
                (ellaavatru, &[Oblique]),
            ],
        },
    );

    let ellaarum = word![E, AlveolarL, AlveolarL, LongA, R, U, M];
    let ellaar = word![E, AlveolarL, AlveolarL, LongA, R];

    map.insert(
        ellaarum,
        SpecialWord {
            if_matches: KindSet::single(PeyarChol),
            then_insert: vec![(ellaar, &[Oblique])],
        },
    );

    let ellorum = word![E, AlveolarL, AlveolarL, LongO, R, U, M];
    let ellor = word![E, AlveolarL, AlveolarL, LongO, R];

    map.insert(
        ellorum,
        SpecialWord {
            if_matches: KindSet::single(PeyarChol),
            then_insert: vec![(ellor, &[Oblique])],
        },
    );

    let maattu = word![M, LongA, RetroT, RetroT, U];
    let maattaa = word![M, LongA, RetroT, RetroT, LongA];

    map.insert(
        maattu,
        SpecialWord {
            if_matches: KindSet::single(ThunaiVinai),
            then_insert: vec![(maattu, &[GeneralStem, SpecialA]), (maattaa, &[Negative])],
        },
    );

    map
}

#[derive(Deserialize)]
struct RawVerbData {
    word: String,
    sub: Option<u8>,
    ve: Vec<String>,
}

type Ves = HashMap<String, BTreeSet<&'static Word>>;

type Verbs = HashMap<(String, Option<u8>), Vec<String>>;

type Stems = HashMap<&'static Word, Vec<StemData>>;

lazy_static! {
    static ref STEMS: Stems = {
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

                let word = intern::word(Word::parse(&verb.word));
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

        let specials = get_specials();

        let mut stems = Stems::new();
        for entry in ENTRIES.iter() {
            StemData::parse(&mut StemState {
                stems: &mut stems,
                ves: &ves,
                verbs: &verbs,
                specials: &specials,
                entry,
            });
        }

        intern::done();

        stems
    };
}

struct StemState<'a> {
    stems: &'a mut Stems,
    ves: &'a Ves,
    verbs: &'a Verbs,
    specials: &'a Specials,
    entry: &'static Entry,
}

#[derive(Copy, Clone, Debug)]
pub struct StemData {
    pub root: &'static Word,
    pub entry: EntryIndex,
    pub state: ExpandState,
}

impl StemData {
    pub fn new(entry: &'static Entry, root: &'static Word, state: ExpandState) -> Self {
        Self {
            entry: entry.index,
            root,
            state,
        }
    }

    fn parse(state: &mut StemState) {
        use EntryKind::*;

        let entry = state.entry;

        // Check for special words
        if !entry.word.contains(' ') {
            let word: &Word = &Word::parse(&entry.word);
            if let Some(special) = state.specials.get(word) {
                if entry.kind_set.matches_any(special.if_matches) {
                    for (word, insert) in special.then_insert.iter() {
                        Self::insert(state, word, insert);
                    }

                    return;
                }
            }
        }

        let words: Vec<_> = Entry::words(&entry.word).collect();

        let callback = if entry.kind_set.matches(PeyarAdai) {
            match words.as_slice() {
                [adv, adj] if entry.kind_set.matches(VinaiAdai) => {
                    Self::stem_subwords(state, adv, Self::stem_adverb);
                    Self::stem_subwords(state, adj, Self::stem_adjective);

                    return;
                }
                _ => Self::stem_adjective,
            }
        } else if entry.kind_set.matches(VinaiAdai) {
            Self::stem_adverb
        } else if entry.kind_set.matches(VinaiChol) {
            Self::stem_verb
        } else if entry.kind_set.matches(PeyarChol) {
            Self::stem_noun
        } else {
            Self::stem_other
        };

        for word in words {
            Self::stem_subwords(state, word, callback);
        }
    }

    fn stem_subwords(state: &mut StemState, word: &str, callback: fn(&mut StemState, &Word)) {
        for word in Entry::joined_subwords(word) {
            callback(state, &word);
        }
    }

    fn stem_verb(state: &mut StemState, word: &Word) {
        use Letter::*;

        let entry = state.entry;
        let key = (String::from(&*entry.word), entry.subword);
        let ve = &state.verbs[&key];
        let parsed: Vec<_> = ve.iter().map(|ve| Word::parse(ve)).collect();

        let strong = parsed.iter().any(|word| word.ends_with(word![K, A]))
            && !parsed.iter().any(|word| word.end_matches(letterset![I, Y]));

        for (ve, mut parsed) in ve.iter().zip(parsed) {
            if let Some(stripped) = ve.strip_prefix('-') {
                let mut success = false;
                for full in &state.ves[stripped] {
                    if let Some(stripped) = word.strip_suffix(full) {
                        parsed = stripped + &parsed;
                        success = true;
                        break;
                    }
                }

                if !success {
                    eprintln!("could not find ending for {} in {}!", ve, word);
                    continue;
                }
            }

            match parsed.last().expect("empty vinaiyecham!") {
                A => {
                    Self::stem_verb_infinitive(state, &parsed, strong);

                    // Special case for words with doubling last letter, joining letters
                    if !strong && parsed.starts_with(word) {
                        Self::insert(state, word, &[InfinitiveStem]);
                        Self::insert(state, &(word + word![V, U]), &[FutureStem]);
                        Self::insert(state, &(word + word![P, U]), &[FutureStem]);
                    }
                }
                U | I | Y => Self::stem_verb_adverb(state, &parsed),
                _ => panic!("not a valid vinaiyecham: {}", parsed),
            }
        }

        if !strong {
            Self::insert(state, word, &[WeakVerb]);
            return;
        }

        Self::insert(state, word, &[StrongVerb]);

        if let Some(word) =
            word.replace_suffix(word![AlveolarL], word![AlveolarR, AlveolarR, A, AlveolarL])
        {
            Self::insert(state, &word, &[Oblique]);
        }

        if let Some(word) = word.replace_suffix(word![RetroT], word![RetroT, RetroT, A, AlveolarL])
        {
            Self::insert(state, &word, &[Oblique]);
        }
    }

    fn stem_verb_infinitive(state: &mut StemState, word: &Word, strong: bool) {
        Self::insert(state, word, &[Infinitive]);

        if !strong {
            let word = word.strip_suffix(word![A]).unwrap();
            Self::insert(state, &(word + word![U]), &[InfinitiveStem]);
            Self::insert(state, &(word + word![U, V, U]), &[FutureStem]);
            Self::insert(state, &(word + word![U, P, U]), &[FutureStem]);
            return;
        }

        if let Some(word) = word.strip_suffix(word![K, K, A]) {
            Self::insert(state, &(word + word![K, K, U]), &[InfinitiveStem]);
            Self::insert(state, &(word + word![P, P, U]), &[FutureStem]);
            return;
        }

        let word = word.strip_suffix(word![K, A]).unwrap();
        Self::insert(state, &(word + word![K, U]), &[InfinitiveStem]);
        Self::insert(state, &(word + word![P, U]), &[FutureStem]);
    }

    fn stem_verb_adverb(state: &mut StemState, word: &Word) {
        Self::insert(state, word, &[Emphasis]);

        if let Some(word) = word.strip_suffix(word![Y]) {
            Self::insert(state, word, &[AdverbStem]);
        } else {
            Self::insert(state, word, &[AdverbStem]);
        }

        if let Some(word) =
            word.replace_suffix(word![AlveolarL, AlveolarL, I], word![AlveolarN, AlveolarN])
        {
            Self::insert(state, &word, &[TenseStem, SpecialA, SpecialB]);
        }
    }

    fn stem_noun(state: &mut StemState, word: &Word) {
        if word.ends_with(word![A, M]) {
            let base = &word[..(word.len() - 1)];
            Self::insert(state, base, &[Done]);
            Self::insert(state, &(base + word![T, T, U]), &[Oblique]);
            Self::insert(state, &(base + word![Ng, K, A, RetroL]), &[Oblique]);
            Self::insert(state, word, &[Emphasis]);
            return;
        }

        Self::insert(state, word, &[Plural]);

        if let Some(word) = word.replace_suffix(word![A, AlveolarN], word![A, R]) {
            Self::insert(state, &word, &[Plural]);
            return;
        }

        if let Some(word) = word.replace_suffix(word![A, R], word![A, AlveolarN]) {
            Self::insert(state, &word, &[Oblique]);
            return;
        }

        let replace = word
            .replace_suffix(word![RetroT, U], word![RetroT, RetroT, U])
            .or_else(|| word.replace_suffix(word![AlveolarR, U], word![AlveolarR, AlveolarR, U]));

        if let Some(word) = replace {
            Self::insert(state, &word, &[Oblique]);
        }
    }

    fn stem_adjective(state: &mut StemState, word: &Word) {
        if word.ends_with(word![A]) {
            Self::insert(state, word, &[AdjectiveStem, Done]);
        } else {
            Self::insert(state, word, &[Emphasis]);
        }
    }

    fn stem_adverb(state: &mut StemState, word: &Word) {
        let replace = word
            .replace_suffix(word![LongA, K, A], word![LongA, Y])
            .or_else(|| word.replace_suffix(word![LongA, Y], word![LongA, K, A]));

        if let Some(word) = replace {
            Self::insert(state, &word, &[Emphasis]);
        }

        Self::insert(state, word, &[Emphasis]);
    }

    fn stem_other(state: &mut StemState, word: &Word) {
        Self::insert(state, word, &[Emphasis]);
    }

    fn insert(state: &mut StemState, word: &Word, states: &[ExpandState]) {
        let word = intern::word(normalize(word));

        let mut stem = word;
        if let Some(new_stem) = stem.strip_suffix(word![U]) {
            stem = new_stem;
        }

        let entry = state.entry;
        if let Some(vec) = state.stems.get_mut(&stem) {
            for &state in states {
                vec.push(Self::new(entry, word, state));
            }
        } else {
            let mut vec = Vec::new();
            for &state in states {
                vec.push(Self::new(entry, word, state));
            }
            state.stems.insert(stem, vec);
        }
    }
}

pub type Choices = Vec<BTreeSet<EntryIndex>>;

#[derive(Clone)]
struct ShortestOnly {
    shortest: usize,
    choices: Vec<Choices>,
}

impl ShortestOnly {
    fn new() -> Self {
        Self {
            shortest: 0,
            choices: vec![Vec::new()],
        }
    }

    fn single(elem: Choices) -> Self {
        Self {
            shortest: elem.len(),
            choices: vec![elem],
        }
    }

    fn push(&mut self, elem: Choices) {
        let len = elem.len();
        if len > self.shortest {
            return;
        }

        if len < self.shortest {
            self.shortest = len;
            self.choices = vec![elem];
            return;
        }

        self.choices.push(elem);
    }

    fn into_vec(self) -> Vec<Choices> {
        self.choices
    }
}

pub fn best_choices(full_word: &Word) -> Vec<Choices> {
    let full_word = &normalize(full_word);

    let mut map: BTreeMap<usize, ShortestOnly> = BTreeMap::new();
    map.insert(0, ShortestOnly::new());

    // TODO: use pop_first when stabilized (nightly: map_first_last)
    while let Some((&end, _)) = map.iter().next() {
        let after = choices_after(full_word, end);

        let choices = map.remove(&end).unwrap();
        if end == full_word.len() {
            return choices.into_vec();
        }

        for choices in choices.into_vec() {
            for (end, choice) in after.iter() {
                let mut choices = choices.clone();
                choices.push(choice.clone());

                if let Some(vec) = map.get_mut(end) {
                    vec.push(choices);
                } else {
                    map.insert(*end, ShortestOnly::single(choices));
                }
            }
        }
    }

    Vec::new()
}

fn choices_after(full_word: &Word, start: usize) -> BTreeMap<usize, BTreeSet<EntryIndex>> {
    let stems: &Stems = &STEMS;

    let mut results = BTreeMap::new();

    for end in ((start + 1)..=full_word.len()).rev() {
        let stem = &full_word[start..end];
        if let Some(datas) = stems.get(stem) {
            let mut ex = Expand::new(full_word, start);
            for data in datas {
                ExpandChoice::insert_new(&mut ex, data);
            }
            ex.evaluate(&mut results);
        }
    }

    results
}

#[derive(Clone, Debug)]
pub struct Choice {
    pub end: usize,
    pub entry: EntryIndex,
}

#[derive(Clone, Debug)]
struct Expand<'a> {
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

    pub fn evaluate(&mut self, results: &mut BTreeMap<usize, BTreeSet<EntryIndex>>) {
        while let Some(choice) = self.choices.pop() {
            choice.step(self, results);
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ExpandState {
    WeakVerb,
    StrongVerb,
    VerbStem,
    AdverbStem,
    InfinitiveStem,
    Negative,
    Infinitive,
    PastStem,
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
    CaseNoKu,
    Irundhu,
    MaybeAdjective,
    AdjectiveStem,
    AdjectiveStemVa,
    AdjectiveStemAvai,
    Emphasis,
    Particle,
    VowelAdjective,
    Done,
}

#[derive(Copy, Clone, Debug)]
struct ExpandChoice {
    end: usize,
    entry: EntryIndex,
    is_start: bool,
    has_implicit_u: bool,
    state: ExpandState,
}

impl ExpandChoice {
    pub fn insert_new(ex: &mut Expand, data: &StemData) {
        let choice = Self {
            end: ex.start,
            entry: data.entry,
            is_start: true,
            has_implicit_u: false,
            state: Done,
        };

        choice.add_goto(ex, data.root, &[data.state]);
    }

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

    pub fn add_goto(mut self, ex: &mut Expand, mut suffix: &Word, states: &[ExpandState]) {
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
        if !self.has_implicit_u || word.matches(self.end, LetterSet::vowel()) {
            self.goto(ex, states);
        }

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

    pub fn result(&self, results: &mut BTreeMap<usize, BTreeSet<EntryIndex>>, inc: usize) {
        let end = self.end + inc;

        if let Some(set) = results.get_mut(&end) {
            set.insert(self.entry);
        } else {
            let mut set = BTreeSet::new();
            set.insert(self.entry);
            results.insert(end, set);
        }
    }

    pub fn step(&self, ex: &mut Expand, results: &mut BTreeMap<usize, BTreeSet<EntryIndex>>) {
        use Letter::*;

        match self.state {
            WeakVerb => {
                self.goto(ex, &[VerbStem]);

                if !self.end_matches(ex, LetterSet::vowel()) {
                    self.add_goto(ex, word![U, T, A, AlveolarL], &[Oblique]);
                }

                self.add_goto(ex, word![T, A, AlveolarL], &[Oblique]);
            }

            StrongVerb => {
                self.goto(ex, &[VerbStem]);

                self.add_goto(ex, word![T, T, A, AlveolarL], &[Oblique]);
            }

            VerbStem => {
                self.goto(ex, &[Emphasis]);

                self.add_goto(ex, word![K, A], &[Done]);
                self.add_goto(ex, word![M, K, A, RetroL], &[Particle]);
                self.add_goto(ex, word![U, M, K, A, RetroL], &[Particle]);
            }

            AdverbStem => match self.last(ex) {
                Some(U) => self.goto(ex, &[PastStem, SpecialA]),

                Some(I) => {
                    self.add_goto(ex, word![AlveolarN], &[PastStem, SpecialB]);
                    self.add_goto(ex, word![Y, A], &[AdjectiveStem, Done]);
                    self.add_goto(ex, word![AlveolarR, AlveolarR, U], &[Done]);
                }

                _ => {
                    self.add_goto(ex, word![AlveolarN], &[PastStem, SpecialB]);
                    self.add_goto(ex, word![Y, I, AlveolarR, AlveolarR, U], &[Done]);
                    self.add_goto(ex, word![Y, I, AlveolarN], &[PastStem, SpecialB]);
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
                self.add_goto(ex, word![U, M], &[Particle]);
            }

            Negative => {
                self.goto(ex, &[Done]);

                self.add_goto(ex, word![M, Ai], &[Oblique]);
                self.add_goto(ex, word![M, A, AlveolarL], &[Oblique]);
                self.add_goto(ex, word![T, A], &[AdjectiveStem, Done]);
                self.add_goto(ex, word![T, U], &[Emphasis]);
            }

            Infinitive => {
                self.goto(ex, &[Emphasis]);

                self.add_goto(ex, word![AlveolarL], &[Oblique]);
                self.add_goto(ex, word![RetroT, RetroT, U, M], &[Emphasis]);
                self.add_goto(ex, word![AlveolarL, LongA, M], &[Emphasis]);
            }

            PastStem => {
                self.goto(ex, &[TenseStem]);
                self.add_goto(ex, word![LongA, AlveolarL], &[Emphasis]);
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
                self.goto(ex, &[Particle]);

                self.add_goto(ex, word![RetroL], &[Particle]);
                self.add_goto(ex, word![AlveolarN], &[Particle]);
                self.add_goto(ex, word![R], &[Particle]);
            }

            GeneralStem => {
                self.add_goto(ex, word![LongO], &[GeneralStemO]);
                self.add_goto(ex, word![LongE], &[GeneralStemE]);
                self.add_goto(ex, word![LongI], &[GeneralStemI, GeneralStemNgal]);
                self.add_goto(ex, word![LongA], &[GeneralStemA, GeneralStemNgal]);

                self.add_goto(ex, word![A, T, U], &[Particle, GeneralStemNgal]);
            }

            GeneralStemA => {
                self.add_goto(ex, word![Y], &[Particle]);
                self.add_goto(ex, word![AlveolarN], &[Particle]);
                self.add_goto(ex, word![AlveolarL], &[Particle]);
                self.add_goto(ex, word![R], &[Particle, GeneralStemPlural]);
            }

            GeneralStemI => {
                self.add_goto(ex, word![R], &[Particle, GeneralStemPlural]);
            }

            GeneralStemE => {
                self.add_goto(ex, word![M], &[Particle]);
                self.add_goto(ex, word![AlveolarN], &[Particle]);
            }

            GeneralStemO => {
                self.add_goto(ex, word![RetroL], &[Oblique]);
                self.add_goto(ex, word![AlveolarN], &[Oblique]);
                self.add_goto(ex, word![R], &[Oblique]);
                self.add_goto(ex, word![M], &[Particle]);
            }

            GeneralStemNgal => {
                self.goto(ex, &[GeneralStemPlural]);
                self.add_goto(ex, word![M], &[GeneralStemPlural]);
            }

            GeneralStemPlural => {
                self.add_goto(ex, word![K, A, RetroL], &[Particle]);
            }

            Plural => {
                self.goto(ex, &[Oblique]);

                self.add_goto(ex, word![K, A, RetroL], &[Oblique]);
                self.add_goto(ex, word![K, K, A, RetroL], &[Oblique]);
            }

            Oblique => {
                self.goto(ex, &[Case]);

                if self.ends_with(ex, word![T, U])
                    && (self.ends_with(ex, word![A, T, U])
                        || self.ends_with(ex, word![I, T, U])
                        || self.ends_with(ex, word![U, T, U])
                        || self.ends_with(ex, word![E, T, U]))
                {
                    self.add_goto(ex, word![A, AlveolarN], &[Case]);
                    self.add_goto(ex, word![A, AlveolarL, K, U], &[Emphasis]);
                }

                self.add_goto(ex, word![I, AlveolarN], &[Case]);
                self.add_goto(ex, word![I, AlveolarL, K, U], &[Emphasis]);
            }

            Case => {
                self.goto(ex, &[CaseNoKu]);

                self.add_goto(ex, word![K, K, U], &[Emphasis]);
                if !self.end_matches(ex, letterset![I, LongI, Ai]) {
                    self.add_goto(ex, word![U, K, K, U], &[Emphasis]);
                }
            }

            CaseNoKu => {
                self.goto(ex, &[Emphasis]);

                self.add_goto(ex, word![Ai], &[Emphasis]);

                self.add_goto(ex, word![A, T, U], &[Oblique]);
                self.add_goto(ex, word![U, RetroT, Ai, Y, A], &[AdjectiveStem, Done]);

                self.add_goto(ex, word![U, RetroT, AlveolarN], &[Emphasis]);
                self.add_goto(ex, word![LongO, RetroT, U], &[Emphasis]);
                self.add_goto(ex, word![LongA, AlveolarL], &[Emphasis]);

                self.add_goto(ex, word![I, AlveolarL], &[Irundhu]);
                self.add_goto(ex, word![I, RetroT, A, M], &[Irundhu]);
            }

            Irundhu => {
                self.goto(ex, &[Emphasis]);
                self.add_goto(ex, word![I, R, U, M, T, U], &[MaybeAdjective]);
            }

            MaybeAdjective => {
                self.goto(ex, &[Emphasis]);
                self.add_goto(ex, word![A], &[AdjectiveStem, Done]);
            }

            AdjectiveStem => {
                if !self.ends_with(ex, word![V, A]) {
                    self.goto(ex, &[AdjectiveStemAvai]);

                    self.add_goto(ex, word![V, A], &[AdjectiveStemVa]);
                }

                self.add_goto(ex, word![T, U], &[Oblique]);
            }

            AdjectiveStemVa => {
                self.add_goto(ex, word![RetroL], &[Oblique]);
                self.add_goto(ex, word![AlveolarN], &[Oblique]);
                self.add_goto(ex, word![R], &[Plural]);
            }

            AdjectiveStemAvai => {
                self.add_goto(ex, word![V, Ai], &[Emphasis]);
                self.add_goto(ex, word![V, Ai, K, A, RetroL], &[Oblique]);
                self.add_goto(ex, word![V, A, AlveolarR, AlveolarR, U], &[Oblique]);
            }

            Emphasis => {
                self.goto(ex, &[Particle]);
                self.add_goto(ex, word![U, M], &[Particle]);
            }

            Particle => {
                self.goto(ex, &[Done]);
                self.add_goto(ex, word![LongA], &[Done]);
            }

            VowelAdjective => {
                if self.end + 3 >= ex.full_word.len() {
                    return;
                }

                let next = ex.full_word[self.end];
                let after = ex.full_word[self.end + 1];
                if after == Y {
                    if next == V {
                        // Remove only first V
                        self.result(results, 1);
                    }
                } else if next == after {
                    if next == V {
                        // Remove both V's in case it was a vowel
                        self.result(results, 2);
                    }

                    // Remove only first of doubled letters
                    self.result(results, 1);
                }
            }

            Done => {
                self.result(results, 0);

                if let Some(next) = ex.full_word.get(self.end) {
                    if !LetterSet::vallinam().matches(next) {
                        return;
                    }

                    if ex
                        .full_word
                        .get(self.end + 1)
                        .map(|lt| lt != next)
                        .unwrap_or(false)
                    {
                        return;
                    }

                    self.result(results, 1);
                }
            }
        }
    }
}
