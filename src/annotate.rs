use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;
use std::fs::File;
use std::io::BufReader;
use std::rc::Rc;

use serde::Deserialize;

use crate::dictionary::{Entry, EntryIndex, EntryKind, KindSet, ENTRIES};
use crate::tamil::{Letter, LetterSet, Word};
use crate::{intern, HashMap};

use ExpandState::*;

#[derive(Debug)]
pub enum TextSegment<'a, T> {
    NonTamil(&'a str),
    Tamil(Box<Word>, T),
}

impl<'a> TextSegment<'a, ()> {
    pub fn parse(text: &'a str) -> impl Iterator<Item = TextSegment<'a, ()>> {
        let mut chars = text.char_indices().peekable();

        // Create an iterator using a closure to process the characters
        std::iter::from_fn(move || {
            let (start, ch) = chars.next()?;
            let is_tamil = Letter::is_tamil(ch);

            // Get a chunk of similar text
            let mut end = text.len();
            while let Some(&(index, ch)) = chars.peek() {
                if Letter::is_tamil(ch) != is_tamil {
                    end = index;
                    break;
                }

                chars.next();
            }

            let word = &text[start..end];

            if is_tamil {
                Some(Self::Tamil(Word::parse(word), ()))
            } else {
                Some(Self::NonTamil(word))
            }
        })
    }

    pub fn group(self) -> Vec<TextSegment<'a, Option<Rc<Choice>>>> {
        match self {
            Self::NonTamil(word) => vec![TextSegment::NonTamil(word)],

            Self::Tamil(original_word, ()) => {
                // Normalize the word
                let word = Normalized::new(&original_word, true);

                // Split the words into groups
                let groups = joined_groups(&word.normalized);

                // Only take the first group, if there is one
                if let Some(choices) = groups.into_iter().next() {
                    choices
                        .into_iter()
                        .scan(0, |index, choice| {
                            // Find the start and end of this segment
                            let start = *index;
                            let end = choice.letter_end;
                            *index = end;

                            // Return just that segment, along with the corresponding choice
                            let word = word.slice_original(start, end);
                            Some(TextSegment::Tamil(word.into(), Some(choice)))
                        })
                        .collect()
                } else {
                    vec![TextSegment::Tamil(original_word, None)]
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Normalized<'a> {
    pub original: &'a Word,
    pub normalized: Box<Word>,
    // Pairs of (normalized offset, original offset)
    offsets: Vec<(usize, usize)>,
}

impl<'a> Normalized<'a> {
    pub fn new(word: &'a Word, fix_last: bool) -> Self {
        use Letter::*;

        #[inline]
        fn normalize_map(left: Letter, right: Letter) -> Option<(Letter, Letter)> {
            match (left, right) {
                (Ng, K) => Some((M, K)),
                (Ny, Ch) => Some((M, Ch)),
                (N, T) => Some((M, T)),
                (RetroT, K | Ch | P) => Some((RetroL, right)),
                (AlveolarR, K | Ch | P) => Some((AlveolarL, right)),
                _ => None,
            }
        }

        let mut letters = Vec::with_capacity(word.len());
        let mut offsets = Vec::new();
        let mut current_difference: isize = 0;

        let mut add_offset = |letters: &Vec<Letter>, diff| {
            current_difference += diff;
            let norm_offset = letters.len();
            let original_offset = (norm_offset as isize) - current_difference;
            offsets.push((norm_offset, original_offset as usize));
        };

        let mut iter = word.iter();
        if word.starts_with(word![A, Y, Y]) {
            letters.push(Ai);
            add_offset(&letters, -1);
            letters.push(Y);
            for _ in 0..3 {
                iter.adv();
            }
        }

        while let Some(lt) = iter.next() {
            use Letter::*;

            if let Some(next) = iter.peek() {
                if let Some((lt, next)) = normalize_map(lt, next) {
                    iter.adv();
                    letters.extend_from_slice(&[lt, next]);
                    continue;
                }
            }

            match lt {
                Au => {
                    letters.push(A);
                    add_offset(&letters, 1);
                    letters.push(V);
                    add_offset(&letters, 1);
                    letters.push(U);
                }

                Aaydham => {}

                K if iter.peek() == Some(Sh) => {
                    iter.adv();

                    letters.extend_from_slice(&[RetroL, Ch]);
                    if !iter.peek_matches(LetterSet::vowel()) {
                        add_offset(&letters, 1);
                        letters.push(U);
                    }
                }

                N if !iter.is_end() => {
                    letters.push(AlveolarN);
                }

                J => letters.push(Ch),

                Sh => {
                    letters.push(RetroT);
                    if !iter.peek_matches(LetterSet::vowel()) {
                        add_offset(&letters, 1);
                        letters.push(U);
                    }
                }

                S => {
                    letters.push(Ch);
                    if !iter.peek_matches(LetterSet::vowel()) {
                        add_offset(&letters, 1);
                        if iter.peek() == Some(Y) {
                            letters.push(I);
                        } else {
                            letters.push(U);
                        }
                    }
                }

                H => letters.push(K),

                _ => letters.push(lt),
            }
        }

        if fix_last {
            if let Some(lt) = letters.last_mut() {
                match lt {
                    Ng | Ny | N => *lt = M,
                    RetroT => *lt = RetroL,
                    AlveolarR => *lt = AlveolarL,
                    _ => {}
                }
            }
        }

        Self {
            original: word,
            normalized: letters.into(),
            offsets,
        }
    }

    pub fn to_original(&self, normalized_index: usize) -> usize {
        for &(i, orig) in self.offsets.iter().rev() {
            if let Some(diff) = normalized_index.checked_sub(i) {
                return diff + orig;
            }
        }

        normalized_index
    }

    pub fn slice_original(&self, start: usize, end: usize) -> &'a Word {
        &self.original[self.to_original(start)..self.to_original(end)]
    }
}

const ADVERB: KindSet =
    KindSet::single(EntryKind::VinaiAdai).union(KindSet::single(EntryKind::InaiIdaiChol));

const PRONOUN: KindSet =
    KindSet::single(EntryKind::SuttuPeyar).union(KindSet::single(EntryKind::VinaaPeyar));

struct SpecialWord {
    if_matches: KindSet,
    likelihood: StemLikelihood,
    then_insert: Vec<(&'static Word, &'static [ExpandState])>,
}

type Specials = HashMap<&'static Word, SpecialWord>;

fn get_specials() -> Specials {
    use EntryKind::*;
    use ExpandState::*;
    use StemLikelihood::*;

    let mut map = Specials::default();

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
                likelihood: Prefix,
                then_insert: vec![(pronoun, &[Emphasis])],
            },
        );
    }

    let regular_pronoun_bases: &[&Word] = &[
        word![E, Ng, K, A, RetroL],
        word![U, Ng, K, A, RetroL],
        word![T, A, Ng, K, A, RetroL],
    ];

    for &base in regular_pronoun_bases {
        map.insert(
            base,
            SpecialWord {
                if_matches: KindSet::single(PeyarChol),
                likelihood: Prefix,
                then_insert: vec![(base, &[Oblique])],
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
                likelihood: Prefix,
                then_insert: vec![
                    (base, &[CaseNoKu]),
                    (
                        intern::word(base + word![A, K, K, U]),
                        &[Emphasis, AagaOrAayOrAana],
                    ),
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
                likelihood: Prefix,
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
                likelihood: Prefix,
                then_insert: vec![(vowel, &[VowelAdjective])],
            },
        );
    }

    let aana = word![LongA, AlveolarN, A];

    map.insert(
        aana,
        SpecialWord {
            if_matches: KindSet::single(IdaiChol),
            likelihood: Suffix,
            then_insert: vec![(aana, &[Adjective])],
        },
    );

    let ulla = word![U, RetroL, RetroL, A];
    let ullu = word![U, RetroL, RetroL, U];

    map.insert(
        ulla,
        SpecialWord {
            if_matches: KindSet::any(),
            likelihood: Regular,
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
            likelihood: Regular,
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
            likelihood: Regular,
            then_insert: vec![(ellaar, &[Oblique])],
        },
    );

    let ellorum = word![E, AlveolarL, AlveolarL, LongO, R, U, M];
    let ellor = word![E, AlveolarL, AlveolarL, LongO, R];

    map.insert(
        ellorum,
        SpecialWord {
            if_matches: KindSet::single(PeyarChol),
            likelihood: Regular,
            then_insert: vec![(ellor, &[Oblique])],
        },
    );

    let maattu = word![M, LongA, RetroT, RetroT, U];
    let maattaa = word![M, LongA, RetroT, RetroT, LongA];

    map.insert(
        maattu,
        SpecialWord {
            if_matches: KindSet::single(ThunaiVinai),
            likelihood: Regular,
            then_insert: vec![(maattu, &[GeneralStem, SpecialA]), (maattaa, &[Negative])],
        },
    );

    // Don't tread kondiru as a single word, despite it often being written as such
    map.insert(
        word![K, O, RetroN, RetroT, I, R, U],
        SpecialWord {
            if_matches: KindSet::single(VinaiChol),
            likelihood: Regular,
            then_insert: vec![],
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
            Err(_) => return HashMap::default(),
        };

        let verb_list: Vec<RawVerbData> = serde_json::from_reader(BufReader::new(file))
            .expect("verbs parse error");

        eprintln!(" => {} verbs", verb_list.len());
        eprintln!("Building stems...");

        // Process the raw verb list by creating maps (Ves and Verbs)
        let mut ves = Ves::default();
        let mut verbs = Verbs::default();
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
        let mut stems = Stems::default();
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
enum StemLikelihood {
    Regular,
    Prefix,
    Suffix,
    Unlikely,
}

#[derive(Copy, Clone, Debug)]
struct StemData {
    root: &'static Word,
    entry: EntryIndex,
    likelihood: StemLikelihood,
    state: ExpandState,
}

impl StemData {
    fn new(
        entry: &'static Entry,
        root: &'static Word,
        state: ExpandState,
        likelihood: StemLikelihood,
    ) -> Self {
        Self {
            root,
            entry: entry.index,
            likelihood,
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
                        Self::insert_with(state, word, insert, special.likelihood);
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
        } else if entry.kind_set.matches_any(ADVERB) {
            Self::stem_adverb
        } else if entry.kind_set.matches(VinaiChol) {
            Self::stem_verb
        } else if entry.kind_set.matches_any(PRONOUN) {
            Self::stem_pronoun
        } else if entry.kind_set.matches(PeyarChol) {
            Self::stem_noun
        } else if entry.kind_set.matches(IdaiChol) {
            Self::stem_particle
        } else {
            Self::stem_other
        };

        // Stem each of the words individually
        for word in words {
            Self::stem_subwords(state, word, callback);
        }
    }

    fn convert_auv_to_avv(word: &Word) -> Option<Box<Word>> {
        use Letter::*;

        if !word.contains_word(word![Au, V]) {
            return None;
        }

        let mut letters = Vec::new();
        let mut iter = word.iter();
        while let Some(lt) = iter.next() {
            if lt == Au && iter.peek() == Some(V) {
                letters.extend_from_slice(&[A, V]);
            } else {
                letters.push(lt);
            }
        }

        Some(letters.into())
    }

    fn stem_subwords(state: &mut StemState, word: &str, callback: fn(&mut StemState, &Word)) {
        // For every possible way to join the words, add a stem
        for word in Entry::joined_subwords(word) {
            // Convert any -auv- to -avv- (as in vauvaal = vavvaal)
            if let Some(word) = Self::convert_auv_to_avv(&word) {
                callback(state, &word);
            }

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
                    eprintln!("could not find ending for {ve} in {word}!");
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

                _ => panic!("not a valid vinaiyecham: {parsed}"),
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

    fn stem_particle(state: &mut StemState, word: &Word) {
        if word.ends_with(word![K, A]) {
            Self::stem_adverb(state, word);
        } else if word.ends_with(word![A]) {
            Self::stem_adverb(state, word);
            Self::stem_adjective(state, word);
        } else {
            Self::stem_noun_with(state, word, StemLikelihood::Unlikely);
            Self::stem_other(state, word);
        }
    }

    fn stem_noun(state: &mut StemState, word: &Word) {
        Self::stem_noun_with(state, word, StemLikelihood::Regular);
    }

    fn stem_noun_with(state: &mut StemState, word: &Word, likelihood: StemLikelihood) {
        // Handle nouns ending in -am
        if word.ends_with(word![A, M]) {
            Self::insert_with(state, &word[..(word.len() - 1)], &[NounWithAm], likelihood);
            return;
        }

        Self::insert_with(state, word, &[Plural], likelihood);

        // Don't allow magan to be magar
        if word.ends_with(word![M, A, K, A, AlveolarN]) {
            return;
        }

        // Handle nouns ending in -an
        if let Some(word) = word.replace_suffix(word![A, AlveolarN], word![A, R]) {
            Self::insert_with(state, &word, &[Plural], likelihood);
            return;
        }

        // Handle nouns ending in -ar
        if let Some(word) = word.replace_suffix(word![A, R], word![A, AlveolarN]) {
            Self::insert_with(state, &word, &[Oblique], likelihood);
            return;
        }

        // Handle nouns which double a hard consonant
        let replace = word
            .replace_suffix(word![RetroT, U], word![RetroT, RetroT, U])
            .or_else(|| word.replace_suffix(word![AlveolarR, U], word![AlveolarR, AlveolarR, U]));

        if let Some(word) = replace {
            Self::insert_with(state, &word, &[Oblique], likelihood);
        }
    }

    fn stem_pronoun(state: &mut StemState, word: &Word) {
        Self::insert(state, word, &[Oblique]);
    }

    fn stem_adjective(state: &mut StemState, word: &Word) {
        // Handle adjectives ending in -endra
        if let Some(word) = word.strip_suffix(word![E, AlveolarN, AlveolarR, A]) {
            Self::insert(state, &(word + word![E, AlveolarN, P, A]), &[AdjectiveStem]);

            Self::insert(
                state,
                &(word + word![E, AlveolarN, AlveolarN, U, M]),
                &[Done],
            );
        }

        // Handle adjectives ending in -aadha
        if let Some(word) = word.strip_suffix(word![LongA, T, A]) {
            Self::insert_with(
                state,
                &(word + word![LongA]),
                &[Done],
                StemLikelihood::Unlikely,
            );
        }

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
        Self::insert_with(state, word, states, StemLikelihood::Regular);
    }

    fn insert_with(
        state: &mut StemState,
        word: &Word,
        states: &[ExpandState],
        mut likelihood: StemLikelihood,
    ) {
        use EntryKind::*;
        use StemLikelihood::*;

        if let Regular = likelihood {
            let entry = state.entry;

            if entry.word.starts_with('-') {
                likelihood = Suffix;
            } else if entry.word.ends_with('-')
                || entry.kind_set.matches_any(PRONOUN)
                || (entry.kind_set.matches(VinaiChol) && !entry.kind_set.matches(ThunaiVinai))
            {
                // Mark as prefix if one of these conditions is met:
                // 1. The word ends with a hyphen
                // 2. The word is a pronoun
                // 3. The word is a non-helper verb
                likelihood = Prefix;
            }
        }

        let word = intern::word(Normalized::new(word, false).normalized);

        // Strip final -u since it may disappear
        let mut stem = word;
        if let Some(new_stem) = stem.strip_suffix(word![U]) {
            stem = new_stem;
        }

        // Make sure the stem is valid
        if !is_possible_stem_start(stem) {
            panic!("invalid stem: {stem}");
        }

        // Insert stem into Stems map
        let entry = state.entry;
        if let Some(vec) = state.stems.get_mut(&stem) {
            for &state in states {
                vec.push(Self::new(entry, word, state, likelihood));
            }
        } else {
            let mut vec = Vec::new();
            for &state in states {
                vec.push(Self::new(entry, word, state, likelihood));
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

fn joined_groups(full_word: &Word) -> Vec<Choices> {
    let groups = best_groups(full_word);

    // If only one grouping is found, just return it
    if groups.len() <= 1 {
        return groups;
    }

    // Merge any groupings with the same letter boundaries
    let mut map: BTreeMap<Vec<usize>, Choices> = BTreeMap::new();
    for choices in groups {
        let ends = choices.iter().map(|choice| choice.letter_end).collect();

        if let Some(existing) = map.get_mut(&ends) {
            for (a, b) in existing.iter_mut().zip(choices) {
                *a = a.union(&b);
            }
        } else {
            map.insert(ends, choices);
        }
    }

    // Prefer longer words first
    map.into_iter().rev().map(|(_, choices)| choices).collect()
}

fn best_groups(full_word: &Word) -> Vec<Choices> {
    // Words should not have more than 100 letters
    const MAX_LEN: usize = 100;

    // Words should not have more than 10 segments
    const MAX_SEG_COUNT: usize = 10;

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

    // Stems starting with these consonants and English letters are invalid
    const INVALID_START: LetterSet =
        LetterSet::latin().union(letterset![RetroN, Zh, RetroL, AlveolarR]);

    if stem.start_matches(INVALID_START) {
        // Stems starting with lch- are actually ksh-, which is valid
        if stem.starts_with(word![RetroL, Ch]) {
            return true;
        }

        return false;
    }

    // Stems with a vowel in their second position are also valid
    if stem.matches(1, LetterSet::vowel()) {
        return true;
    }

    // Double consonant at start is not valid
    false
}

#[derive(Debug)]
pub struct Choice {
    pub letter_end: usize,
    pub unlikely: bool,
    pub entries: BTreeSet<EntryIndex>,
}

impl Choice {
    pub fn new(entry: EntryIndex, letter_end: usize, unlikely: bool) -> Self {
        let mut entries = BTreeSet::new();
        entries.insert(entry);

        Self {
            letter_end,
            unlikely,
            entries,
        }
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

    pub fn union(&self, other: &Self) -> Rc<Self> {
        debug_assert!(self.letter_end == other.letter_end);

        Rc::new(Self {
            letter_end: self.letter_end,
            unlikely: self.unlikely && other.unlikely,
            entries: &self.entries | &other.entries,
        })
    }

    pub fn ids(&self) -> String {
        let mut buffer = String::new();
        for id in self.entries.iter() {
            if !buffer.is_empty() {
                buffer.push(',');
            }

            write!(&mut buffer, "{}", id).unwrap();
        }

        buffer
    }
}

#[derive(Clone, Debug)]
struct Expand<'a> {
    full_word: &'a Word,
    start: usize,
    choices: Vec<ExpandChoice>,
}

impl<'a> Expand<'a> {
    fn new(full_word: &'a Word, start: usize) -> Self {
        Self {
            full_word,
            start,
            choices: Vec::new(),
        }
    }

    fn push(&mut self, choice: ExpandChoice) {
        self.choices.push(choice);
    }

    fn evaluate(&mut self, results: &mut BTreeMap<usize, Choice>) {
        while let Some(choice) = self.choices.pop() {
            choice.step(self, results);
        }
    }
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
enum ExpandState {
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
    LocativeE,
    Irundhu,
    MaybeAdjective,
    Adjective,
    AdjectiveStem,
    AdjectiveStemVa,
    AdjectiveStemAvai,
    AagaOrAayOrAana,
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
    likely_end: bool,
    state: ExpandState,
}

impl ExpandChoice {
    fn insert_new(ex: &mut Expand, data: &StemData) {
        use StemLikelihood::*;

        let unlikely = match data.likelihood {
            Regular => false,
            Prefix => ex.start != 0,
            Suffix => ex.start == 0,
            Unlikely => true,
        };

        let choice = Self {
            end: ex.start,
            entry: data.entry,
            is_start: true,
            has_implicit_u: false,
            unlikely,
            likely_end: false,
            state: Done,
        };

        choice.add_goto(ex, data.root, &[data.state]);
    }

    #[inline]
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

    #[inline]
    fn unlikely(mut self) -> Self {
        self.unlikely = true;
        self
    }

    fn result(&self, ex: &Expand, results: &mut BTreeMap<usize, Choice>, inc: usize) {
        let end = self.end + inc;

        if let Some(set) = results.get_mut(&end) {
            set.push(self.entry, self.unlikely);
        } else {
            let word = ex.full_word;
            let mut letter_end = end;
            if word.matches(end - 1, LetterSet::consonant())
                && word.matches(end, LetterSet::vowel())
            {
                letter_end -= 1;
            }

            results.insert(end, Choice::new(self.entry, letter_end, self.unlikely));
        }
    }

    fn step(mut self, ex: &mut Expand, results: &mut BTreeMap<usize, Choice>) {
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
                self.likely_end = true;
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
                    // Old verbal noun (as in nadakkaiyil)
                    self.add_goto(ex, word![Ai], &[RareVerbalNoun]);

                    self.add_goto(ex, word![I, AlveolarR, U], &[TenseStem]);
                    self.unlikely().add_goto(
                        ex,
                        word![I, AlveolarN, AlveolarR, U],
                        &[TenseStem, SpecialA],
                    );
                } else {
                    // Old verbal noun (as in seygaiyil)
                    self.add_goto(ex, word![K, Ai], &[RareVerbalNoun]);

                    self.add_goto(ex, word![K, I, AlveolarR, U], &[TenseStem]);
                    self.unlikely().add_goto(
                        ex,
                        word![K, I, AlveolarN, AlveolarR, U],
                        &[TenseStem, SpecialA],
                    );
                }

                // Allow extra U (as in utkaarugiren)
                if self.end_matches(ex, letterset![R, Zh]) {
                    // Old verbal noun (as in thinnugai)
                    self.add_goto(ex, word![U, K, Ai], &[RareVerbalNoun]);

                    self.add_goto(ex, word![U, K, I, AlveolarR, U], &[TenseStem]);
                    self.unlikely().add_goto(
                        ex,
                        word![U, K, I, AlveolarN, AlveolarR, U],
                        &[TenseStem, SpecialA],
                    );
                }

                // Old conditional forms
                self.unlikely()
                    .add_goto(ex, word![I, AlveolarL], &[Emphasis]);
                self.unlikely()
                    .add_goto(ex, word![I, AlveolarN], &[Emphasis]);

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

                // Old negative verbal noun
                self.add_goto(ex, word![M, Ai], &[RareVerbalNoun]);

                self.add_goto(ex, word![T, LongI], &[GeneralStemI]);
                self.add_goto(ex, word![M, A, AlveolarL], &[Oblique]);
                self.add_goto(ex, word![T, A], &[Adjective]);
                self.add_goto(ex, word![T, U], &[Emphasis]);
            }

            Infinitive => {
                self.goto(ex, &[Emphasis]);

                // Old verbal noun (as in nadakkal)
                self.add_goto(ex, word![AlveolarL], &[RareVerbalNoun]);

                self.likely_end = true;
                self.add_goto(ex, word![I, AlveolarL, AlveolarL, Ai], &[Particle]);
                self.add_goto(ex, word![RetroT, RetroT, U, M], &[Particle]);
                self.add_goto(ex, word![AlveolarL, LongA, M], &[Particle]);
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
                self.likely_end = true;
                self.goto(ex, &[Particle]);

                self.unlikely().add_goto(ex, word![RetroL], &[Particle]);
                self.unlikely().add_goto(ex, word![AlveolarN], &[Particle]);

                self.add_goto(ex, word![R], &[Particle]);
            }

            GeneralStem => {
                self.likely_end = true;
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
                // Old -em person suffix (as in vandhem)
                self.unlikely().add_goto(ex, word![M], &[Particle]);

                self.add_goto(ex, word![AlveolarN], &[Particle]);
            }

            GeneralStemO => {
                // Old person suffixes to form nouns (as in vandhor)
                self.unlikely().add_goto(ex, word![RetroL], &[Oblique]);
                self.unlikely().add_goto(ex, word![AlveolarN], &[Oblique]);
                self.add_goto(ex, word![R], &[Oblique]);

                self.add_goto(ex, word![M], &[Particle]);
            }

            GeneralStemNgal => {
                self.unlikely = true;
                self.goto(ex, &[GeneralStemPlural]);
                self.add_goto(ex, word![M], &[GeneralStemPlural]);
            }

            GeneralStemPlural => {
                self.add_goto(ex, word![K, A, RetroL], &[Particle]);
            }

            RareVerbalNoun => {
                self.unlikely = true;
                self.goto(ex, &[Oblique]);
            }

            NounWithAm => {
                self.goto(ex, &[Done]);

                self.add_goto(ex, word![M], &[AagaOrAayOrAana, Emphasis]);
                self.add_goto(ex, word![M, K, A, RetroL], &[Oblique]);
                self.add_goto(ex, word![T, T, U], &[Oblique]);
            }

            Plural => {
                self.goto(ex, &[Oblique]);

                self.add_goto(ex, word![K, A, RetroL], &[Oblique]);
                self.add_goto(ex, word![K, K, A, RetroL], &[Oblique]);
            }

            Oblique => {
                self.goto(ex, &[Case, AagaOrAayOrAana]);

                if !self.ends_with(ex, word![K, A, RetroL]) {
                    // -am sariyai (as in aatrangarai)
                    self.unlikely().add_goto(ex, word![A, M], &[Done]);
                }

                if self.ends_with(ex, word![T, U])
                    && (self.ends_with(ex, word![A, T, U])
                        || self.ends_with(ex, word![I, T, U])
                        || self.ends_with(ex, word![U, T, U])
                        || self.ends_with(ex, word![E, T, U]))
                {
                    // -an sariyai (as in adhan or adharku)
                    self.add_goto(ex, word![A, AlveolarN], &[Case]);
                    self.add_goto(ex, word![A, AlveolarL, K, U], &[Emphasis, AagaOrAayOrAana]);
                }

                self.add_goto(ex, word![I, AlveolarN], &[Case]);
                self.add_goto(ex, word![I, AlveolarL, K, U], &[Emphasis, AagaOrAayOrAana]);
            }

            Case => {
                self.goto(ex, &[CaseNoKu]);

                self.add_goto(ex, word![K, K, U], &[Emphasis, AagaOrAayOrAana]);
                if !self.end_matches(ex, letterset![I, LongI, Ai]) {
                    self.add_goto(ex, word![U, K, K, U], &[Emphasis, AagaOrAayOrAana]);
                }
            }

            CaseNoKu => {
                self.goto(ex, &[Emphasis]);

                self.add_goto(ex, word![Ai], &[Emphasis]);

                // Old possesive suffix (as in avanadhu kai)
                self.unlikely().add_goto(ex, word![A, T, U], &[Oblique]);

                self.add_goto(ex, word![U, RetroT, Ai, Y, A], &[Adjective]);

                self.add_goto(ex, word![U, RetroT, AlveolarN], &[Emphasis]);
                self.add_goto(ex, word![LongO, RetroT, U], &[Emphasis]);
                self.add_goto(ex, word![LongA, AlveolarL], &[Emphasis]);

                self.add_goto(ex, word![I, AlveolarL], &[LocativeE]);
                self.add_goto(ex, word![I, RetroT, A, M], &[Irundhu]);
            }

            LocativeE => {
                self.goto(ex, &[Irundhu]);

                // Allow extra emphatic -e after locative case
                self.add_goto(ex, word![LongE], &[Irundhu]);
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

            AagaOrAayOrAana => {
                self.unlikely = true;
                self.add_goto(ex, word![LongA, Y], &[Emphasis]);
                self.add_goto(ex, word![LongA, K, A], &[Emphasis]);
                self.add_goto(ex, word![LongA, AlveolarN, A], &[Emphasis]);
            }

            Emphasis => {
                self.goto(ex, &[Particle]);
                self.add_goto(ex, word![U, M], &[Particle]);
            }

            Particle => {
                self.goto(ex, &[Done]);

                // Mark as unlikely to allow fixed words to get a chance
                self.unlikely = true;
                self.add_goto(ex, word![LongA], &[Done]);
                self.add_goto(ex, word![LongE], &[Done]);
                self.add_goto(ex, word![LongO], &[Done]);
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
                        self.result(ex, results, 1);
                    }
                } else if next == after {
                    if next == V {
                        // Remove both V's in case it was a vowel
                        self.result(ex, results, 2);
                    }

                    // Remove only first of doubled letters
                    self.result(ex, results, 1);
                }
            }

            Done => {
                // Mark as unlikely if one of these conditions is met:
                // 1. Single letter word (e.g. ai)
                // 2. Two letter word starting with consonant (e.g. mu)
                // 3. Marked as likely end, but not at end of word
                let matched = self.matched(ex);
                if matched.len() == 1
                    || (matched.len() == 2 && matched.matches(0, LetterSet::consonant()))
                    || (self.likely_end && self.end != ex.full_word.len())
                {
                    self.unlikely = true;
                }

                self.result(ex, results, 0);

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

                    self.result(ex, results, 1);
                }
            }
        }
    }
}
