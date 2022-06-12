use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs::File;
use std::io::BufReader;
use std::rc::Rc;

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

        match lt {
            Au => letters.extend_from_slice(&[A, V, U]),

            Aaydham => {}

            K if iter.peek() == Some(Sh) => {
                iter.next();

                if iter.peek_matches(LetterSet::vowel()) {
                    letters.extend_from_slice(&[RetroT, Ch]);
                } else {
                    letters.extend_from_slice(&[RetroT, Ch, U]);
                }
            }

            J => letters.push(Ch),

            Sh => {
                if iter.peek_matches(LetterSet::vowel()) {
                    letters.push(RetroT);
                } else {
                    letters.extend_from_slice(&[RetroT, U]);
                }
            }

            S => {
                if iter.peek_matches(LetterSet::vowel()) {
                    letters.push(Ch);
                } else {
                    letters.extend_from_slice(&[Ch, U]);
                }
            }

            _ => letters.push(lt),
        }
    }

    letters.into()
}

pub fn normalize_final(word: &mut Word) {
    use Letter::*;

    if let Some(lt) = word.last_mut() {
        match lt {
            Ng | Ny | N => *lt = M,
            RetroT => *lt = RetroL,
            AlveolarR => *lt = AlveolarL,
            _ => {}
        }
    }
}

const PRONOUN: KindSet =
    KindSet::single(EntryKind::SuttuPeyar).union(KindSet::single(EntryKind::VinaaPeyar));

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
                if_matches: PRONOUN,
                then_insert: vec![(without_vai, &[AdjectiveStemAvai])],
            },
        );
    }

    let vowel_adjectives: &[&Word] = &[word![A], word![I], word![E]];

    for &vowel in vowel_adjectives {
        map.insert(
            vowel,
            SpecialWord {
                if_matches: PRONOUN,
                then_insert: vec![(vowel, &[VowelAdjective])],
            },
        );
    }

    let aana = word![LongA, N, A];

    map.insert(
        aana,
        SpecialWord {
            if_matches: KindSet::single(IdaiChol),
            then_insert: vec![(aana, &[Adjective])],
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

// Vinaiyecham map for resolving incomplete information
type Ves = HashMap<String, BTreeSet<&'static Word>>;

// Verb data indexed by word and subword
type Verbs = HashMap<(String, Option<u8>), Vec<String>>;

// Map from stems to stem data
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

        eprintln!(" => {} verbs", verb_list.len());
        eprintln!("Building stems...");

        // Process the raw verb list by creating maps (Ves and Verbs)
        let mut ves = Ves::new();
        let mut verbs = Verbs::new();
        for mut verb in verb_list {
            for ve in verb.ve.iter_mut() {
                // Vinaiyechams starting with a hyphen are incomplete
                if ve.starts_with('-') {
                    continue;
                }

                // For compount words, only the last word is conjugated
                if let Some(index) = verb.word.rfind(' ') {
                    // Make sure that there aren't multiple parts
                    if verb.word.contains(&[',', ';']) {
                        panic!("compound verb cannot be conjugated: {}", verb.word);
                    }

                    ve.insert_str(0, &verb.word[..=index]);
                    continue;
                }

                // Record this vinaiyecham and the word it came from (in Ves)
                let word = intern::word(Word::parse(&verb.word));
                if let Some(set) = ves.get_mut(ve) {
                    set.insert(word);
                } else {
                    let mut set = BTreeSet::new();
                    set.insert(word);
                    ves.insert(ve.clone(), set);
                }
            }

            // Remember the vinaiyecham for this word and subword
            verbs.insert((verb.word, verb.sub), verb.ve);
        }

        // Get a map of special words which are expanded differently
        let specials = get_specials();

        // Create a map of stems using all these data structures
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

        // Clear the interning metadata since it won't be used anymore
        intern::done();

        eprintln!(" => {} stems", stems.len());
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

        // Splt on commas to find individual words
        let words: Vec<_> = Entry::words(&entry.word).collect();

        // Figure out which function to use for stemming
        let callback = if entry.kind_set.matches(PeyarAdai) {
            // Handle -aaga/-aana and -endru/-endra pairs
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
        } else if entry.kind_set.matches_any(PRONOUN) {
            Self::stem_pronoun
        } else if entry.kind_set.matches(PeyarChol) {
            Self::stem_noun
        } else {
            Self::stem_other
        };

        // Stem each of the words individually
        for word in words {
            Self::stem_subwords(state, word, callback);
        }
    }

    fn stem_subwords(state: &mut StemState, word: &str, callback: fn(&mut StemState, &Word)) {
        // For every possible way to join the words, add a stem
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

        // Usually strong iff infinitive is -ka and adverb is not -i or -y
        let strong = parsed.iter().any(|word| word.ends_with(word![K, A]))
            && !parsed.iter().any(|word| word.end_matches(letterset![I, Y]));

        // Stem each of the vinaiyechams
        for (ve, mut parsed) in ve.iter().zip(parsed) {
            // If incomplete, use Ves to try to match it with another verb
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

            // Use the appropriate stemming method depending on the ending
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

        // If weak, mark as a weak verb and return early
        if !strong {
            Self::insert(state, word, &[WeakVerb]);
            return;
        }

        // Mark as a strong verb
        Self::insert(state, word, &[StrongVerb]);

        // Handle nil => nitral, etc.
        if let Some(word) =
            word.replace_suffix(word![AlveolarL], word![AlveolarR, AlveolarR, A, AlveolarL])
        {
            Self::insert(state, &word, &[RareVerbalNoun]);
        }

        // Handle kel => kettal, etc.
        if let Some(word) = word.replace_suffix(word![RetroL], word![RetroT, RetroT, A, AlveolarL])
        {
            Self::insert(state, &word, &[RareVerbalNoun]);
        }
    }

    fn stem_verb_infinitive(state: &mut StemState, word: &Word, strong: bool) {
        Self::insert(state, word, &[Infinitive]);

        // Handle weak verbs by replacing final -a with -u
        if !strong {
            let word = word.strip_suffix(word![A]).unwrap();
            Self::insert(state, &(word + word![U]), &[InfinitiveStem]);
            Self::insert(state, &(word + word![U, V, U]), &[FutureStem]);
            Self::insert(state, &(word + word![U, P, U]), &[FutureStem]);
            return;
        }

        // Handle strong -kka verbs
        if let Some(word) = word.strip_suffix(word![K, K, A]) {
            Self::insert(state, &(word + word![K, K, U]), &[InfinitiveStem]);
            Self::insert(state, &(word + word![P, P, U]), &[FutureStem]);
            return;
        }

        // Handle other strong verbs
        let word = word.strip_suffix(word![K, A]).unwrap();
        Self::insert(state, &(word + word![K, U]), &[InfinitiveStem]);
        Self::insert(state, &(word + word![P, U]), &[FutureStem]);
    }

    fn stem_verb_adverb(state: &mut StemState, word: &Word) {
        Self::insert(state, word, &[Emphasis]);

        // If ends in -y, remove it
        if let Some(word) = word.strip_suffix(word![Y]) {
            Self::insert(state, word, &[AdverbStem]);
        } else {
            Self::insert(state, word, &[AdverbStem]);
        }

        // Handle solli => sonn-
        if let Some(word) =
            word.replace_suffix(word![AlveolarL, AlveolarL, I], word![AlveolarN, AlveolarN])
        {
            Self::insert(state, &word, &[TenseStem, SpecialA, SpecialB]);
        }
    }

    fn stem_noun(state: &mut StemState, word: &Word) {
        // Handle nouns ending in -am
        if word.ends_with(word![A, M]) {
            Self::insert(state, &word[..(word.len() - 1)], &[NounWithAm]);
            return;
        }

        Self::insert(state, word, &[Plural]);

        // Handle nouns ending in -an
        if let Some(word) = word.replace_suffix(word![A, AlveolarN], word![A, R]) {
            Self::insert(state, &word, &[Plural]);
            return;
        }

        // Handle nouns ending in -ar
        if let Some(word) = word.replace_suffix(word![A, R], word![A, AlveolarN]) {
            Self::insert(state, &word, &[Oblique]);
            return;
        }

        // Handle nouns which double a hard consonant
        let replace = word
            .replace_suffix(word![RetroT, U], word![RetroT, RetroT, U])
            .or_else(|| word.replace_suffix(word![AlveolarR, U], word![AlveolarR, AlveolarR, U]));

        if let Some(word) = replace {
            Self::insert(state, &word, &[Oblique]);
        }
    }

    fn stem_pronoun(state: &mut StemState, word: &Word) {
        Self::insert(state, word, &[Oblique]);
    }

    fn stem_adjective(state: &mut StemState, word: &Word) {
        if word.ends_with(word![A]) {
            Self::insert(state, word, &[Adjective]);
        } else {
            // If doesn't end with -a, treat as unknown
            Self::insert(state, word, &[Emphasis]);
        }
    }

    fn stem_adverb(state: &mut StemState, word: &Word) {
        Self::insert(state, word, &[Emphasis]);

        if word.len() <= 3 {
            return;
        }

        // Handle adverbs ending in -aaga
        if let Some(word) = word.replace_suffix(word![LongA, K, A], word![LongA, Y]) {
            Self::insert(state, &word, &[Emphasis]);
        }

        if word.len() <= 4 {
            return;
        }

        // Handle adverbs ending in -endru
        if let Some(word) =
            word.replace_suffix(word![E, AlveolarN, AlveolarR, U], word![E, AlveolarN, A])
        {
            Self::insert(state, &word, &[Emphasis]);
        }
    }

    fn stem_other(state: &mut StemState, word: &Word) {
        Self::insert(state, word, &[Emphasis]);
    }

    fn insert(state: &mut StemState, word: &Word, states: &[ExpandState]) {
        let word = intern::word(normalize(word));

        // Strip final -u since it may disappear
        let mut stem = word;
        if let Some(new_stem) = stem.strip_suffix(word![U]) {
            stem = new_stem;
        }

        // Make sure the stem is valid
        if !is_possible_stem_start(stem) {
            panic!("invalid stem: {}", stem);
        }

        // Insert stem into Stems map
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

pub fn supported() -> bool {
    !STEMS.is_empty()
}

pub type Choices = Vec<Rc<Choice>>;

#[derive(Clone, Debug)]
struct ShortestOnly {
    shortest: usize,
    unlikely: usize,
    choices: Vec<Choices>,
}

impl ShortestOnly {
    fn new() -> Self {
        Self {
            shortest: 0,
            unlikely: 0,
            choices: vec![Vec::new()],
        }
    }

    fn count_unlikely(elem: &Choices) -> usize {
        elem.iter().filter(|choice| choice.unlikely).count()
    }

    fn single(elem: Choices) -> Self {
        Self {
            shortest: elem.len(),
            unlikely: Self::count_unlikely(&elem),
            choices: vec![elem],
        }
    }

    fn push(&mut self, elem: Choices) {
        let len = elem.len();

        if len > self.shortest {
            return;
        }

        // Must be at least as short here

        let unlikely = Self::count_unlikely(&elem);

        if len == self.shortest {
            match unlikely.cmp(&self.unlikely) {
                Ordering::Less => {}

                Ordering::Equal => {
                    self.choices.push(elem);
                    return;
                }

                Ordering::Greater => {
                    return;
                }
            }
        }

        // Must either be shorter, or same length but less unlikely

        self.shortest = len;
        self.unlikely = unlikely;
        self.choices.clear();
        self.choices.push(elem);
    }

    fn into_vec(self) -> Vec<Choices> {
        self.choices
    }
}

pub fn best_choices(full_word: &Word) -> Vec<Choices> {
    // Words should not have more than 100 letters
    const MAX_LEN: usize = 100;

    // Words should not have more than 10 segments
    const MAX_SEG_COUNT: usize = 10;

    // Normalize the input word
    let mut full_word = normalize(full_word);
    normalize_final(&mut full_word);
    let full_word = &full_word;
    if full_word.len() > MAX_LEN {
        return Vec::new();
    }

    // Create an empty map
    let mut map: BTreeMap<usize, ShortestOnly> = BTreeMap::new();
    map.insert(0, ShortestOnly::new());

    // Repeatedly expand shortest suffix until matches full word
    // TODO: use pop_first when stabilized (nightly: map_first_last)
    while let Some((&end, _)) = map.iter().next() {
        // Check for matching full word
        let choices = map.remove(&end).unwrap();
        if end == full_word.len() {
            return choices.into_vec();
        }

        // Compute the maximum length that may still be useful
        let max_len = map
            .get(&full_word.len())
            .map(|s| s.shortest)
            .unwrap_or(MAX_SEG_COUNT);

        // If the shortest choice is over the maximum length, all are
        if choices.shortest >= max_len {
            continue;
        }

        // Discard any choices which are over the maximum length
        let mut choices = choices.into_vec();
        choices.retain(|choice| choice.len() < max_len);
        if choices.is_empty() {
            continue;
        }

        // Add new choices to end of existing choices
        let after = choices_after(full_word, end);
        for choices in choices {
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

    // Could not match full word
    Vec::new()
}

fn choices_after(full_word: &Word, start: usize) -> Vec<(usize, Rc<Choice>)> {
    // If the stem starts with an invalid combination, don't search at all
    if !is_possible_stem_start(&full_word[start..]) {
        return Vec::new();
    }

    let stems: &Stems = &STEMS;

    let mut results = BTreeMap::new();

    // Iterate through all prefixes, starting with longest
    for end in ((start + 1)..=full_word.len()).rev() {
        let stem = &full_word[start..end];

        // Stems never end with U since it is removed
        if stem.ends_with(word![U]) {
            continue;
        }

        // No need to look up a single consonant
        if stem.len() == 1 && stem.start_matches(LetterSet::consonant()) {
            continue;
        }

        // If the prefix matches a stem, try to expand that stem
        if let Some(datas) = stems.get(stem) {
            let mut ex = Expand::new(full_word, start);
            for data in datas {
                ExpandChoice::insert_new(&mut ex, data);
            }
            ex.evaluate(&mut results);
        }
    }

    // Put every choice into an Rc to reduce copying
    results
        .into_iter()
        .map(|(end, choice)| (end, Rc::new(choice)))
        .collect()
}

fn is_possible_stem_start(stem: &Word) -> bool {
    // Stems starting with a vowel are valid
    if stem.start_matches(LetterSet::vowel()) {
        return true;
    }

    // Stems starting with these consonants are invalid
    if stem.start_matches(letterset![RetroN, Zh, RetroL, AlveolarR, AlveolarN]) {
        return false;
    }

    // Stems with a vowel in their second position are also valid
    if stem.matches(1, LetterSet::vowel()) {
        return true;
    }

    // Stems starting with tch- are actually ksh-, which is valid
    if stem.starts_with(word![RetroT, Ch]) {
        return true;
    }

    // Double consonant at start is not valid
    false
}

#[derive(Debug)]
pub struct Choice {
    pub unlikely: bool,
    pub entries: BTreeSet<EntryIndex>,
}

impl Choice {
    pub fn new(entry: EntryIndex, unlikely: bool) -> Self {
        let mut entries = BTreeSet::new();
        entries.insert(entry);

        Self { unlikely, entries }
    }

    pub fn push(&mut self, entry: EntryIndex, unlikely: bool) {
        match (self.unlikely, unlikely) {
            (true, true) | (false, false) => {
                self.entries.insert(entry);
            }

            (true, false) => {
                self.unlikely = false;
                self.entries.clear();
                self.entries.insert(entry);
            }

            (false, true) => {}
        }
    }
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

    pub fn evaluate(&mut self, results: &mut BTreeMap<usize, Choice>) {
        while let Some(choice) = self.choices.pop() {
            choice.step(self, results);
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ExpandState {
    WeakVerb,
    StrongVerb,
    RareVerbalNoun,
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
    NounWithAm,
    Plural,
    Oblique,
    Case,
    CaseNoKu,
    Irundhu,
    MaybeAdjective,
    Adjective,
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
    unlikely: bool,
    state: ExpandState,
}

impl ExpandChoice {
    pub fn insert_new(ex: &mut Expand, data: &StemData) {
        let choice = Self {
            end: ex.start,
            entry: data.entry,
            is_start: true,
            has_implicit_u: false,
            unlikely: false,
            state: Done,
        };

        choice.add_goto(ex, data.root, &[data.state]);
    }

    fn matched<'a>(&self, ex: &Expand<'a>) -> &'a Word {
        &ex.full_word[ex.start..self.end]
    }

    fn last(&self, ex: &Expand) -> Option<Letter> {
        if self.has_implicit_u {
            return Some(Letter::U);
        }

        self.matched(ex).last()
    }

    fn end_matches(&self, ex: &Expand, suffix: LetterSet) -> bool {
        if self.has_implicit_u {
            return suffix.matches(Letter::U);
        }

        self.matched(ex).end_matches(suffix)
    }

    fn ends_with(&self, ex: &Expand, mut suffix: &Word) -> bool {
        if self.has_implicit_u {
            if let Some(new_suffix) = suffix.strip_suffix(word![U]) {
                suffix = new_suffix;
            } else {
                return false;
            }
        }

        self.matched(ex).ends_with(suffix)
    }

    fn goto(&self, ex: &mut Expand, states: &[ExpandState]) {
        for &state in states {
            ex.push(Self {
                is_start: false,
                state,
                ..*self
            });
        }
    }

    fn add_goto(mut self, ex: &mut Expand, mut suffix: &Word, states: &[ExpandState]) {
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

            // If not at start, don't permit a joining V
            if !self.is_start {
                return;
            }
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

    fn unlikely(mut self) -> Self {
        self.unlikely = true;
        self
    }

    fn result(&self, results: &mut BTreeMap<usize, Choice>, inc: usize) {
        let end = self.end + inc;

        if let Some(set) = results.get_mut(&end) {
            set.push(self.entry, self.unlikely);
        } else {
            results.insert(end, Choice::new(self.entry, self.unlikely));
        }
    }

    pub fn step(&self, ex: &mut Expand, results: &mut BTreeMap<usize, Choice>) {
        use Letter::*;

        match self.state {
            WeakVerb => {
                self.goto(ex, &[VerbStem]);

                if !self.end_matches(ex, LetterSet::vowel()) {
                    self.add_goto(ex, word![U, T, A, AlveolarL], &[RareVerbalNoun]);
                }

                self.add_goto(ex, word![T, A, AlveolarL], &[RareVerbalNoun]);
            }

            StrongVerb => {
                self.goto(ex, &[VerbStem]);

                self.add_goto(ex, word![T, T, A, AlveolarL], &[RareVerbalNoun]);
            }

            VerbStem => {
                self.goto(ex, &[Emphasis]);

                self.unlikely().add_goto(ex, word![K, A], &[Done]);

                self.add_goto(ex, word![M, K, A, RetroL], &[Particle]);
                self.add_goto(ex, word![U, M, K, A, RetroL], &[Particle]);
            }

            AdverbStem => match self.last(ex) {
                Some(U) => self.goto(ex, &[PastStem, SpecialA]),

                Some(I) => {
                    self.add_goto(ex, word![AlveolarR, AlveolarR, U], &[Done]);
                    self.add_goto(ex, word![Y, A], &[Adjective]);
                    self.add_goto(ex, word![AlveolarN], &[PastStem, SpecialB]);
                }

                _ => {
                    self.unlikely()
                        .add_goto(ex, word![Y, I, AlveolarN], &[PastStem, SpecialB]);

                    self.add_goto(ex, word![Y, I, AlveolarR, AlveolarR, U], &[Done]);
                    self.add_goto(ex, word![AlveolarN], &[PastStem, SpecialB]);
                }
            },

            InfinitiveStem => {
                if self.ends_with(ex, word![K, U]) {
                    self.add_goto(ex, word![I, AlveolarR, U], &[TenseStem]);
                    self.unlikely().add_goto(
                        ex,
                        word![I, AlveolarN, AlveolarR, U],
                        &[TenseStem, SpecialA],
                    );
                } else {
                    self.add_goto(ex, word![K, I, AlveolarR, U], &[TenseStem]);
                    self.unlikely().add_goto(
                        ex,
                        word![K, I, AlveolarN, AlveolarR, U],
                        &[TenseStem, SpecialA],
                    );
                }

                if self.end_matches(ex, letterset![R, Zh]) {
                    self.add_goto(ex, word![U, K, I, AlveolarR, U], &[TenseStem]);
                    self.unlikely().add_goto(
                        ex,
                        word![U, K, I, AlveolarN, AlveolarR, U],
                        &[TenseStem, SpecialA],
                    );
                }

                self.add_goto(ex, word![Ai], &[RareVerbalNoun]);

                self.add_goto(ex, word![LongA], &[Negative]);
                self.add_goto(ex, word![U, M], &[Particle]);
            }

            Negative => {
                self.unlikely().goto(ex, &[Done]);

                self.add_goto(
                    ex,
                    word![V, I, RetroT, RetroT, LongA, AlveolarL],
                    &[Emphasis],
                );

                self.add_goto(ex, word![M, Ai], &[RareVerbalNoun]);

                self.add_goto(ex, word![M, A, AlveolarL], &[Oblique]);
                self.add_goto(ex, word![T, A], &[Adjective]);
                self.add_goto(ex, word![T, U], &[Emphasis]);
            }

            Infinitive => {
                self.goto(ex, &[Emphasis]);

                self.add_goto(ex, word![AlveolarL], &[RareVerbalNoun]);

                self.add_goto(ex, word![RetroT, RetroT, U, M], &[Emphasis]);
                self.add_goto(ex, word![AlveolarL, LongA, M], &[Emphasis]);
            }

            PastStem => {
                self.goto(ex, &[TenseStem]);
                self.add_goto(ex, word![LongA, AlveolarL], &[Emphasis]);
            }

            TenseStem => {
                self.goto(ex, &[GeneralStem]);
                self.add_goto(ex, word![A], &[Adjective]);
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

                self.unlikely().add_goto(ex, word![RetroL], &[Particle]);
                self.unlikely().add_goto(ex, word![AlveolarN], &[Particle]);

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
                self.unlikely().add_goto(ex, word![M], &[Particle]);

                self.add_goto(ex, word![AlveolarN], &[Particle]);
            }

            GeneralStemO => {
                self.unlikely().add_goto(ex, word![RetroL], &[Oblique]);
                self.unlikely().add_goto(ex, word![AlveolarN], &[Oblique]);

                self.add_goto(ex, word![R], &[Oblique]);
                self.add_goto(ex, word![M], &[Particle]);
            }

            GeneralStemNgal => {
                self.unlikely().goto(ex, &[GeneralStemPlural]);
                self.unlikely().add_goto(ex, word![M], &[GeneralStemPlural]);
            }

            GeneralStemPlural => {
                self.add_goto(ex, word![K, A, RetroL], &[Particle]);
            }

            RareVerbalNoun => {
                self.unlikely().goto(ex, &[Oblique]);
            }

            NounWithAm => {
                self.goto(ex, &[Done]);

                self.add_goto(ex, word![M], &[Emphasis]);
                self.add_goto(ex, word![M, K, A, RetroL], &[Oblique]);
                self.add_goto(ex, word![T, T, U], &[Oblique]);
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

                self.unlikely().add_goto(ex, word![A, T, U], &[Oblique]);
                self.add_goto(ex, word![U, RetroT, Ai, Y, A], &[Adjective]);

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
                self.add_goto(ex, word![A], &[Adjective]);
            }

            Adjective => {
                self.goto(ex, &[AdjectiveStem, Done]);
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

                // Mark as unlikely to allow fixed words to get a chance
                self.unlikely().add_goto(ex, word![LongA], &[Done]);
                self.unlikely().add_goto(ex, word![LongE], &[Done]);
                self.unlikely().add_goto(ex, word![LongO], &[Done]);
            }

            VowelAdjective => {
                if self.end + 1 >= ex.full_word.len() {
                    self.goto(ex, &[Done]);
                    return;
                } else if self.end + 3 >= ex.full_word.len() {
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
