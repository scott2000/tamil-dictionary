use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;
use std::fs::File;
use std::io::BufReader;
use std::rc::Rc;

use once_cell::sync::OnceCell;

use serde::Deserialize;

use crate::dictionary::{self, Entry, EntryIndex, EntryKind, KindSet};
use crate::tamil::{Letter, LetterCombination, LetterSet, Word};
use crate::{intern, HashMap, HashSet};

use ExpandState::*;

pub type AnnotatedTextSegment<'a> = TextSegment<'a, Option<Rc<Choice>>>;

#[derive(Debug)]
pub enum TextSegment<'a, T = ()> {
    NonTamil(&'a str),
    Tamil(Box<Word>, T),
}

impl<'a> TextSegment<'a> {
    pub fn parse(text: &'a str) -> impl Iterator<Item = TextSegment<'a>> {
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

    pub fn annotate(text: &'a str) -> impl Iterator<Item = AnnotatedTextSegment<'a>> {
        let (exclude, text) = Self::strip_exclude(text);
        Self::parse(text).flat_map(move |seg| seg.group_excluding(&exclude))
    }

    pub fn group(self) -> Vec<AnnotatedTextSegment<'a>> {
        self.group_excluding(&ExcludeSet::default())
    }

    pub fn strip_exclude(text: &'a str) -> (ExcludeSet, &'a str) {
        let text = text.trim();

        let Some(without_exclude) = text.strip_prefix("[exclude:") else {
            return (ExcludeSet::default(), text);
        };

        let Some((excluded, new_text)) = without_exclude.split_once(']') else {
            return (ExcludeSet::default(), text);
        };

        let text = new_text.trim_start();

        let exclude = excluded.split(',').map(str::trim).map(Word::parse).fold(
            ExcludeSet::default(),
            |mut exclude, word| {
                exclude.insert(&word);
                exclude
            },
        );

        (exclude, text)
    }

    pub fn group_excluding(self, exclude: &ExcludeSet) -> Vec<AnnotatedTextSegment<'a>> {
        match self {
            Self::NonTamil(word) => vec![TextSegment::NonTamil(word)],

            Self::Tamil(original_word, ()) => {
                // Normalize the word
                let word = Normalized::new(&original_word, true);

                // Split the words into groups
                let groups = joined_groups(&word, exclude);

                // Only take the first group, if there is one
                let Some(choices) = groups.into_iter().next() else {
                    return vec![TextSegment::Tamil(original_word, None)];
                };

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
            }
        }
    }
}

fn excluded_words() -> &'static HashSet<&'static str> {
    static INSTANCE: OnceCell<HashSet<&'static str>> = OnceCell::new();

    INSTANCE.get_or_init(|| {
        let mut set = HashSet::default();

        let excluded: &[&Word] = &[
            word![E, AlveolarN],
            word![E, M],
            word![E, Ng, K, A, RetroL],
            word![N, A, M],
            word![U, AlveolarN],
            word![U, M],
            word![U, Ng, K, A, RetroL],
            word![T, A, AlveolarN],
            word![T, A, M],
            word![T, A, Ng, K, A, RetroL],
            word![M, U, RetroT, I],
            word![U, RetroL, RetroL, A],
            word![I, AlveolarL, AlveolarL, LongA, T, A],
            word![I, AlveolarL, AlveolarL, LongA, M, A, AlveolarL],
            word![LongO, R],
            word![O, R, U],
            word![O, R, U, V, A, RetroL],
            word![O, R, U, V, A, AlveolarN],
            word![O, R, U, V, A, R],
        ];

        for &word in excluded {
            set.insert(&*Box::leak(word.to_string().into_boxed_str()));
        }

        set
    })
}

const ADVERB: KindSet =
    KindSet::single(EntryKind::VinaiAdai).union(KindSet::single(EntryKind::InaiIdaiChol));

const PRONOUN: KindSet =
    KindSet::single(EntryKind::SuttuPeyar).union(KindSet::single(EntryKind::VinaaPeyar));

const EXCLUDE: KindSet = PRONOUN
    .union(KindSet::single(EntryKind::IdaiChol))
    .union(KindSet::single(EntryKind::ThunaiVinai))
    .union(KindSet::single(EntryKind::VinaiMutru));

#[derive(Clone, Default, Debug)]
pub struct WordCount {
    counts: HashMap<&'static str, usize>,
}

impl WordCount {
    pub fn insert(&mut self, choice: &Choice) {
        let entries = dictionary::entries();
        let excluded = excluded_words();

        let mut has_vinai = false;
        for &index in choice.entries.iter() {
            let entry = &entries[index as usize];
            let kinds = entry.kind_set;

            // If any word is excluded, then all words are excluded to be safe
            if kinds.matches_any(EXCLUDE) || excluded.contains(&*entry.word) {
                return;
            }

            // Record whether any verbs are seen
            if kinds.matches(EntryKind::VinaiChol) {
                has_vinai = true;
            }
        }

        let mut seen = HashSet::default();
        for &index in choice.entries.iter() {
            let entry = &entries[index as usize];

            // If there is a verb present, exclude any derived adverbs
            if has_vinai && entry.kind_set.matches(EntryKind::VinaiAdai) {
                continue;
            }

            // If the word was already seen, don't count it again
            let word = entry.primary_word();
            if !seen.insert(word) {
                continue;
            }

            // Increment the corresponding count in the map
            if let Some(count) = self.counts.get_mut(word) {
                *count += 1;
            } else {
                self.counts.insert(word, 1);
            }
        }
    }

    pub fn insert_segment(&mut self, segment: &AnnotatedTextSegment) {
        if let TextSegment::Tamil(_, Some(choice)) = segment {
            self.insert(choice);
        }
    }

    pub fn into_vec(self, min: usize) -> Vec<(&'static str, usize)> {
        let mut vec: Vec<_> = self
            .counts
            .into_iter()
            .filter(|&(_, count)| count >= min)
            .collect();

        // Sort first in reverse by count, then in order by parsed word
        vec.sort_by(|(a_str, a_count), (b_str, b_count)| {
            b_count
                .cmp(a_count)
                .then_with(|| Word::parse(a_str).cmp(&Word::parse(b_str)))
        });

        vec
    }
}

#[derive(Debug)]
pub struct Normalized<'a> {
    // Normalized word (used for annotation)
    pub normalized: Box<Word>,
    // Original word (used for splitting)
    pub original: &'a Word,
    // Pairs of (normalized offset, original offset)
    original_offsets: Vec<(usize, usize)>,
    // Indices of N which were converted to AlveolarN
    n_offsets: Vec<usize>,
}

impl<'a> Normalized<'a> {
    pub fn new(word: &'a Word, fix_last: bool) -> Self {
        use Letter::*;

        #[inline]
        fn normalize_map(left: Letter, right: Letter) -> Option<(Letter, Letter)> {
            match (left, right) {
                // Mellinam-vallinam transformations
                (Ng, K) => Some((M, K)),
                (Ny, Ch) => Some((M, Ch)),
                (N, T) => Some((M, T)),

                // Mellinam-mellinam transformations
                (RetroN, RetroN) => Some((RetroN, N)),
                (AlveolarN, AlveolarN) => Some((AlveolarN, N)),

                // Vallinam-vallinam transformations
                (RetroT, K | Ch | P) => Some((RetroL, right)),
                (AlveolarR, K | Ch | P) => Some((AlveolarL, right)),

                _ => None,
            }
        }

        let mut letters = Vec::with_capacity(word.len());
        let mut original_offsets = Vec::new();
        let mut current_difference: isize = 0;

        let mut add_offset = |letters: &Vec<Letter>, diff, offset| {
            current_difference += diff;
            let norm_offset = letters.len() + offset;
            let original_offset = (norm_offset as isize) - current_difference;
            original_offsets.push((norm_offset, original_offset as usize));
        };

        let mut iter = word.iter();
        if word.starts_with(word![A, Y, Y]) {
            letters.push(Ai);
            add_offset(&letters, -1, 0);
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
                    add_offset(&letters, 1, 0);
                    letters.push(V);
                    add_offset(&letters, 1, 0);
                    letters.push(U);
                }

                Aaydham => {
                    if iter.at_end() {
                        add_offset(&letters, -1, 0);
                    } else {
                        add_offset(&letters, -1, 1);
                    }
                }

                K if iter.peek() == Some(Sh) => {
                    iter.adv();

                    letters.extend_from_slice(&[RetroL, Ch]);
                    if !iter.peek_matches(LetterSet::vowel()) {
                        add_offset(&letters, 1, 0);
                        letters.push(U);
                    }
                }

                J => letters.push(Ch),

                Sh => {
                    letters.push(RetroT);
                    if !iter.peek_matches(LetterSet::vowel()) {
                        add_offset(&letters, 1, 0);
                        letters.push(U);
                    }
                }

                S => {
                    letters.push(Ch);
                    if !iter.peek_matches(LetterSet::vowel()) {
                        add_offset(&letters, 1, 0);
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

        // Convert N to AlveolarN and record offsets
        let mut n_offsets = Vec::new();
        for (i, lt) in letters.iter_mut().enumerate() {
            if *lt == N {
                *lt = AlveolarN;
                n_offsets.push(i);
            }
        }

        Self {
            original: word,
            normalized: letters.into(),
            original_offsets,
            n_offsets,
        }
    }

    pub fn is_n_dental(&self, index: usize) -> bool {
        self.n_offsets.contains(&index)
    }

    pub fn slice_original(&self, start: usize, end: usize) -> &'a Word {
        &self.original[self.to_original(start)..self.to_original(end)]
    }

    fn to_original(&self, normalized_index: usize) -> usize {
        for &(i, orig) in self.original_offsets.iter().rev() {
            if let Some(diff) = normalized_index.checked_sub(i) {
                return diff + orig;
            }
        }

        normalized_index
    }
}

#[derive(Default, Debug)]
pub struct ExcludeSet {
    set: HashSet<Box<Word>>,
}

impl ExcludeSet {
    pub fn insert(&mut self, word: &Word) {
        // Normalize the word for efficient matching
        let mut word = Normalized::new(word, true).normalized;

        // Remove any final -u
        if let Some(new_word) = word.strip_suffix(word![U]) {
            word = new_word.into();
        }

        self.set.insert(word);
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    pub fn len(&self) -> usize {
        self.set.len()
    }

    fn contains(&self, word: &Word) -> bool {
        self.set.contains(word)
    }
}

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

    map.insert(
        ellaam,
        SpecialWord {
            if_matches: KindSet::single(PeyarChol),
            likelihood: Regular,
            then_insert: vec![(ellaam, &[Emphasis]), (ellaa, &[Adjective])],
        },
    );

    let yaavum = word![Y, LongA, V, U, M];
    let yaa = word![Y, LongA];

    map.insert(
        yaavum,
        SpecialWord {
            if_matches: KindSet::single(PeyarChol),
            likelihood: Regular,
            then_insert: vec![(yaavum, &[Emphasis]), (yaa, &[Adjective])],
        },
    );

    let muzhu = word![M, U, Zh, U];

    map.insert(
        muzhu,
        SpecialWord {
            if_matches: KindSet::single(PeyarAdai),
            likelihood: Regular,
            then_insert: vec![(muzhu, &[AdjectiveStem, Emphasis])],
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

    // Require Ai to be a prefix to prevent it being used wrongly
    let ai = word![Ai];

    map.insert(
        ai,
        SpecialWord {
            if_matches: KindSet::single(PeyarAdai),
            likelihood: Prefix,
            then_insert: vec![(ai, &[Done])],
        },
    );

    // The adjective "in" is often confused for other forms
    let r#in = word![I, AlveolarN];

    map.insert(
        r#in,
        SpecialWord {
            if_matches: KindSet::single(PeyarAdai),
            likelihood: Unlikely,
            then_insert: vec![(r#in, &[Done])],
        },
    );

    // Don't allow "onpathu" or "onpathi"
    let onpadhu = word![O, AlveolarN, P, A, T, U];

    map.insert(
        onpadhu,
        SpecialWord {
            if_matches: KindSet::single(PeyarChol),
            likelihood: Regular,
            then_insert: vec![(onpadhu, &[Plural])],
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

fn stems() -> &'static Stems {
    static INSTANCE: OnceCell<Stems> = OnceCell::new();

    INSTANCE.get_or_init(|| {
        let _ = dictionary::entries();

        eprintln!("Loading verbs...");

        let file = match File::open("verbs.json") {
            Ok(file) => file,
            Err(_) => return HashMap::default(),
        };

        let verb_list: Vec<RawVerbData> =
            serde_json::from_reader(BufReader::new(file)).expect("verbs parse error");

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
                    if verb.word.contains([',', ';']) {
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
        for entry in dictionary::entries() {
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
    })
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
        let parsed: Vec<_> = ve
            .iter()
            .flat_map(|ve| Entry::joined_subwords(ve))
            .collect();

        // Usually strong iff infinitive is -ka and adverb is not -i or -y
        let strong = parsed.iter().any(|word| word.ends_with(word![K, A]))
            && !parsed.iter().any(|word| word.end_matches(letterset![I, Y]));

        // Stem each of the vinaiyechams
        for (ve, mut parsed) in ve.iter().zip(parsed) {
            // If incomplete, use Ves to try to match it with another verb
            if let Some(stripped) = ve.strip_prefix('-') {
                let mut failed_n = false;
                let mut success = false;
                for full in &state.ves[stripped] {
                    if let Some(stripped) = word.strip_suffix(full) {
                        parsed = stripped + &parsed;
                        success = true;
                        break;
                    }

                    // Check if it only failed because of convert_n()
                    if word.get(word.len() - full.len()) == Some(AlveolarN)
                        && full.first() == Some(N)
                    {
                        failed_n = true;
                    }
                }

                if !success {
                    // If it only failed because of convert_n(), skip this stem
                    if failed_n {
                        return;
                    }

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
                        Self::insert(state, word, &[WeakInfinitiveStem]);
                        Self::insert(state, &(word + word![V, U]), &[FutureStem]);
                        Self::insert(state, &(word + word![P, U]), &[FutureStem]);
                    }

                    // Allow -aa instead of -aagu
                    if let Some(word) = parsed.strip_suffix(word![LongA, K, A]) {
                        Self::insert(state, &(word + word![LongA]), &[WeakInfinitiveStem]);
                        Self::insert(state, &(word + word![LongA, V, U]), &[FutureStem]);
                    }
                }

                U | I | Y => {
                    Self::stem_verb_adverb(state, &parsed);

                    // Allow -aay instead of -aagi
                    if let Some(parsed) = parsed.replace_suffix(word![LongA, K, I], word![LongA, Y])
                    {
                        Self::stem_verb_adverb(state, &parsed);
                    }
                }

                _ => panic!("not a valid vinaiyecham: {parsed}"),
            }
        }

        // Allow -aa instead of -aagu
        if let Some(word) = word.strip_suffix(word![LongA, K, U]) {
            Self::insert(state, &(word + word![LongA]), &[WeakVerb]);
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
            Self::insert(state, &(word + word![U]), &[WeakInfinitiveStem]);
            Self::insert(state, &(word + word![U, V, U]), &[FutureStem]);
            Self::insert(state, &(word + word![U, P, U]), &[FutureStem]);
            return;
        }

        // Handle strong -kka verbs
        if let Some(word) = word.strip_suffix(word![K, K, A]) {
            Self::insert(state, &(word + word![K, K, U]), &[StrongInfinitiveStem]);
            Self::insert(state, &(word + word![P, P, U]), &[FutureStem]);
            return;
        }

        // Handle other strong verbs
        let word = word.strip_suffix(word![K, A]).unwrap();
        Self::insert(state, &(word + word![K, U]), &[StrongInfinitiveStem]);
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
            Self::insert_with(state, word.remove_end(1), &[NounWithAm], likelihood);
            return;
        }

        Self::insert_with(state, word, &[Plural], likelihood);

        // Don't allow magan to be magar or vice versa
        if word.ends_with(word![M, A, K, A, AlveolarN])
            || word.ends_with(word![M, A, K, A, R])
            || word.ends_with(word![M, A, H, A, AlveolarN])
            || word.ends_with(word![M, A, H, A, R])
        {
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

        // Handle nouns ending in -um
        if let Some(word) = word.strip_suffix(word![U, M]) {
            if word.end_matches(LetterSet::tamil_final().complement()) {
                // Include the U if ending in invalid ending (as in muzhuvadhum => muzhuvadhu)
                Self::insert_with(state, &(word + word![U]), &[Oblique], likelihood);
            } else if !word.is_empty() {
                // Otherwise don't include the U (as in ellaarum => ellaar)
                Self::insert_with(state, word, &[Oblique], likelihood);
            }

            return;
        }

        // Handle nouns ending in -padhu
        if let Some(word) = word.strip_suffix(word![P, A, T, U]) {
            Self::insert_with(state, &(word + word![P, A, T, T, U]), &[Case], likelihood);
            Self::insert_with(state, &(word + word![P, A, T, T, I]), &[Done], likelihood);
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

        // Most adjectives end in -a, and are considered "normal"
        if word.ends_with(word![A]) {
            Self::insert(state, word, &[Adjective]);
            return;
        }

        // Check for special adjectives
        let _ = Self::stem_special_adjective(state, word);

        // Adjectives ending in -aa or -u can also act fairly "normal"
        if word.end_matches(letterset![LongA, U, LongU]) {
            Self::insert(state, word, &[Adjective]);
            return;
        }

        // If doesn't end with -a, -aa, or -u, treat as unknown
        Self::insert(state, word, &[Emphasis]);
    }

    fn stem_special_adjective(state: &mut StemState, word: &Word) -> Option<()> {
        let mut iter = word.iter();

        // The first letter must be short
        let first = LetterCombination::take(&mut iter)?;
        if !first.is_short() {
            return None;
        }

        // The second letter must end in U
        let second = LetterCombination::take(&mut iter)?;
        if second.combining != Some(Letter::U) {
            return None;
        }

        // The second letter must have a consonant
        let second_base = second.base_consonant()?;

        // A single M with no vowel
        const SINGLE_M: LetterCombination = LetterCombination::single(Letter::M);

        match LetterCombination::take(&mut iter) {
            // Check for doubling vallinam (as in pudhu => puth-)
            None if LetterSet::vallinam().matches(second_base) => {
                let doubled = word.remove_end(1) + word![second_base];
                Self::insert_with(state, &doubled, &[Done], StemLikelihood::Prefix);
            }

            // Check for -um (as in nedum => nedu)
            Some(SINGLE_M) if iter.at_end() => {
                Self::insert_with(
                    state,
                    word.remove_end(1),
                    &[DroppedM],
                    StemLikelihood::Prefix,
                );
            }

            _ => return None,
        }

        Some(())
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

        // Insert both normalization variations if necessary
        let normalized = Normalized::new(word, false);
        let fixed = Normalized::new(word, true);

        if fixed.normalized != normalized.normalized {
            Self::insert_normalized(state, fixed, states, likelihood);
        }

        Self::insert_normalized(state, normalized, states, likelihood);
    }

    fn insert_normalized(
        state: &mut StemState,
        word: Normalized,
        states: &[ExpandState],
        likelihood: StemLikelihood,
    ) {
        // Make sure the stem is valid
        if !is_possible_stem_start(&word, 0) {
            panic!("invalid word: {}", word.original);
        }

        // Intern the word since it will live for duration of program
        let word = intern::word(word.normalized);

        // Strip final -u since it may disappear
        let mut stem = word;
        if let Some(new_stem) = stem.strip_suffix(word![U]) {
            stem = new_stem;
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
    !stems().is_empty()
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

fn joined_groups(word: &Normalized, exclude: &ExcludeSet) -> Vec<Choices> {
    let groups = best_groups(word, exclude);

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

fn best_groups(word: &Normalized, exclude: &ExcludeSet) -> Vec<Choices> {
    // Words should not have more than 100 letters
    const MAX_LEN: usize = 100;

    // Words should not have more than 10 segments
    const MAX_SEG_COUNT: usize = 10;

    let len = word.normalized.len();
    if len > MAX_LEN {
        return Vec::new();
    }

    // Create an empty map
    let mut map: BTreeMap<usize, ShortestOnly> = BTreeMap::new();
    map.insert(0, ShortestOnly::new());

    // Repeatedly expand shortest suffix until matches full word
    while let Some((end, choices)) = map.pop_first() {
        // Check for matching full word
        if end == len {
            return choices.into_vec();
        }

        // Compute the maximum length that may still be useful
        let max_len = map.get(&len).map(|s| s.shortest).unwrap_or(MAX_SEG_COUNT);

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
        let after = choices_after(word, exclude, end);
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

fn choices_after(
    word: &Normalized,
    exclude: &ExcludeSet,
    start: usize,
) -> Vec<(usize, Rc<Choice>)> {
    // If the stem starts with an invalid combination, don't search at all
    if !is_possible_stem_start(word, start) {
        return Vec::new();
    }

    let full_word = &word.normalized;
    let stems = stems();

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

        // If the stem is marked as excluded, then stop searching
        if exclude.contains(stem) {
            break;
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

fn is_possible_stem_start(word: &Normalized, start: usize) -> bool {
    let stem = &word.normalized[start..];

    // Stems starting with a vowel are valid
    if stem.start_matches(LetterSet::vowel()) {
        return true;
    }

    // Stems starting with these consonants and English letters are invalid
    const INVALID_START: LetterSet =
        LetterSet::latin().union(letterset![RetroN, Zh, RetroL, AlveolarR, AlveolarN]);

    if stem.start_matches(INVALID_START) {
        // Stems starting with lch- are actually ksh-, which is valid
        if stem.starts_with(word![RetroL, Ch]) {
            return true;
        }

        // Stems starting with n- are only valid if the N is dental
        if stem.first() == Some(Letter::AlveolarN) {
            return word.is_n_dental(start);
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
    WeakInfinitiveStem,
    StrongInfinitiveStem,
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
    DroppedM,
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

        // Handle insertion of Y and V glides
        if LetterSet::vowel().matches(prev) {
            let glide = if letterset![I, LongI, E, LongE, Ai].matches(prev) {
                Y
            } else {
                V
            };

            if word.get(self.end) == Some(glide) {
                self.end += 1;
                self.goto(ex, states);
            }

            return;
        }

        // Doubling can only occur at the start
        if !self.is_start {
            return;
        }

        let next = word.get(self.end);

        // Doubling requires two of the same letter in a row
        if next != Some(prev) {
            // RetroN-RetroN becomes RetroN-AlveolarN, so it actually is valid
            if !matches!((prev, next), (RetroN, Some(AlveolarN))) {
                return;
            }
        }

        // Handle doubling of final consonant
        self.end += 1;
        self.goto(ex, states);
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
                    self.add_goto(ex, word![Y, A, AlveolarN], &[PastStem, SpecialB]);
                    self.add_goto(ex, word![AlveolarN], &[PastStem, SpecialB]);
                }

                _ => {
                    self.unlikely()
                        .add_goto(ex, word![Y, I, AlveolarN], &[PastStem, SpecialB]);

                    self.add_goto(ex, word![Y, I, AlveolarR, AlveolarR, U], &[Done]);
                    self.add_goto(ex, word![AlveolarN], &[PastStem, SpecialB]);
                }
            },

            WeakInfinitiveStem => {
                self.goto(ex, &[InfinitiveStem]);

                // Old verbal noun (as in seygaiyil)
                self.add_goto(ex, word![K, Ai], &[RareVerbalNoun]);

                self.add_goto(ex, word![K, I, AlveolarR, U], &[TenseStem]);
                self.unlikely().add_goto(
                    ex,
                    word![K, I, AlveolarN, AlveolarR, U],
                    &[TenseStem, SpecialA],
                );
            }

            StrongInfinitiveStem => {
                self.goto(ex, &[InfinitiveStem]);

                // Old verbal noun (as in nadakkaiyil)
                self.add_goto(ex, word![Ai], &[RareVerbalNoun]);

                self.add_goto(ex, word![I, AlveolarR, U], &[TenseStem]);
                self.unlikely().add_goto(
                    ex,
                    word![I, AlveolarN, AlveolarR, U],
                    &[TenseStem, SpecialA],
                );
            }

            InfinitiveStem => {
                // Allow extra U for present (as in utkaarugiren)
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
                self.add_goto(ex, word![RetroL], &[Particle]);
                self.add_goto(ex, word![AlveolarN], &[Particle]);
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

                self.add_goto(ex, word![U, RetroT, A, AlveolarN], &[Emphasis]);
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
                self.add_goto(ex, word![LongA, AlveolarN, A], &[Adjective]);
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
                // Ignore automatically inserted glides
                if !self.end_matches(ex, LetterSet::vowel()) {
                    return;
                }

                // Allow space between (as in ap-padam)
                if self.end + 1 == ex.full_word.len() {
                    if ex.full_word.matches(self.end, LetterSet::tamil_initial()) {
                        self.result(ex, results, 1);
                    }
                    return;
                }

                // If it's short, it's probably something else
                if self.end + 3 >= ex.full_word.len() {
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

            DroppedM => {
                // The next letter must be a valid initial consonant, but not vallinam
                const INVALID_NEXT: LetterSet = LetterSet::tamil_initial()
                    .intersect(LetterSet::consonant())
                    .difference(LetterSet::vallinam())
                    .complement();

                let word = &ex.full_word;
                let last = self.matched(ex).last();

                // Only add to results if ending in U and next letter is valid
                if last == Some(Letter::U) && !word.matches(self.end, INVALID_NEXT) {
                    self.result(ex, results, 0);
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
