use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Display};
use std::ops::*;

use num_enum::{IntoPrimitive, TryFromPrimitive, UnsafeFromPrimitive};

pub const PULLI: char = '\u{bcd}';
pub const COMBINING_LA: char = '\u{bd7}';
pub const OM: char = '\u{bd0}';

#[rustfmt::skip]
const TAMIL_VOWELS: &'static [char] = &[
    'அ', 'ஆ',
    'இ', 'ஈ',
    'உ', 'ஊ',
    'எ', 'ஏ', 'ஐ',
    'ஒ', 'ஓ', 'ஔ',
];

#[rustfmt::skip]
const TAMIL_VOWEL_SIGNS: &'static [char] = &[
    '\u{bbe}',
    '\u{bbf}', '\u{bc0}',
    '\u{bc1}', '\u{bc2}',
    '\u{bc6}', '\u{bc7}', '\u{bc8}',
    '\u{bca}', '\u{bcb}', '\u{bcc}',
];

const AAYDHAM: char = 'ஃ';
const GRANTHA_SSH: char = 'ஶ';

#[rustfmt::skip]
const TAMIL_CONSONANTS: &'static [char] = &[
    'க', 'ங',
    'ச', 'ஞ',
    'ட', 'ண',
    'த', 'ந',
    'ப', 'ம',
    'ய', 'ர', 'ல', 'வ', 'ழ', 'ள',
    'ற', 'ன',
    'ஜ', 'ஷ', 'ஸ', 'ஹ',
];

#[rustfmt::skip]
lazy_static! {
    static ref TAMIL_VOWEL_MAP: HashMap<char, Letter> = {
        to_map(Letter::VOWEL_START, Letter::VOWEL_END, TAMIL_VOWELS)
    };

    static ref TAMIL_VOWEL_SIGN_MAP: HashMap<char, Letter> = {
        to_map(Letter::VOWEL_SIGN_START, Letter::VOWEL_SIGN_END, TAMIL_VOWEL_SIGNS)
    };

    static ref TAMIL_CONSONANT_MAP: HashMap<char, Letter> = {
        let mut map = to_map(Letter::CONSONANT_START, Letter::CONSONANT_END, TAMIL_CONSONANTS);
        map.insert(GRANTHA_SSH, Letter::S);
        map
    };

    static ref VALID_LETTERS: HashSet<char> = {
        TAMIL_VOWELS
            .iter()
            .chain(TAMIL_VOWEL_SIGNS)
            .chain(TAMIL_CONSONANTS)
            .cloned()
            .chain([AAYDHAM, GRANTHA_SSH, PULLI, COMBINING_LA, OM, '\u{200b}', '\u{200c}', '\u{200d}'])
            .chain('a'..='z')
            .chain('A'..='Z')
            .collect()
    };
}

fn to_map(start: u8, end: u8, chars: &'static [char]) -> HashMap<char, Letter> {
    debug_assert_eq!(chars.len(), end as usize + 1 - start as usize);

    chars
        .iter()
        .copied()
        .zip((start..=end).map(Letter::expect_from))
        .collect()
}

pub fn is_consonant(ch: char) -> bool {
    TAMIL_CONSONANT_MAP.contains_key(&ch)
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[derive(IntoPrimitive, TryFromPrimitive, UnsafeFromPrimitive)]
#[repr(u8)]
#[rustfmt::skip]
pub enum Letter {
    A, LongA,
    I, LongI,
    U, LongU,
    E, LongE, Ai,
    O, LongO, Au,

    Aaydham,

    K,      Ng,
    Ch,     Ny,
    RetroT, RetroN,
    T,      N,
    P,      M,

    Y, R,  AlveolarL,
    V, Zh, RetroL,

    AlveolarR, AlveolarN,

    J, Sh, S, H,

    LatinA, LatinB, LatinC, LatinD, LatinE, LatinF,
    LatinG, LatinH, LatinI, LatinJ, LatinK, LatinL,
    LatinM, LatinN, LatinO, LatinP, LatinQ, LatinR, LatinS,
    LatinT, LatinU, LatinV, LatinW, LatinX, LatinY, LatinZ,
}

impl Letter {
    pub const VOWEL_SIGN_START: u8 = Self::LongA as u8;
    pub const VOWEL_SIGN_END: u8 = Self::Au as u8;

    pub const AAYDHAM: u8 = Self::Aaydham as u8;

    pub const TAMIL_CONSONANT_START: u8 = Self::K as u8;
    pub const TAMIL_CONSONANT_END: u8 = Self::AlveolarN as u8;

    pub const GRANTHA_START: u8 = Self::J as u8;
    pub const GRANTHA_END: u8 = Self::H as u8;

    pub const LATIN_START: u8 = Self::LatinA as u8;
    pub const LATIN_END: u8 = Self::LatinZ as u8;

    pub const VOWEL_START: u8 = Self::A as u8;
    pub const VOWEL_END: u8 = Self::VOWEL_SIGN_END;

    pub const CONSONANT_START: u8 = Self::TAMIL_CONSONANT_START;
    pub const CONSONANT_END: u8 = Self::GRANTHA_END;

    pub const LETTER_START: u8 = Self::VOWEL_START;
    pub const LETTER_END: u8 = Self::LATIN_END;

    pub const LETTER_COUNT: u8 = Self::LETTER_END + 1;

    pub fn expect_from(num: u8) -> Self {
        debug_assert!(num <= Self::LETTER_END);

        unsafe { Self::from_unchecked(num.min(Self::LETTER_END)) }
    }

    pub fn offset(self, by: u8) -> Self {
        Self::expect_from(self as u8 + by)
    }

    pub fn negative_offset(self, by: u8) -> Self {
        Self::expect_from(self as u8 - by)
    }

    pub fn is_valid(ch: char) -> bool {
        VALID_LETTERS.contains(&ch)
    }

    pub fn parse(ch: char) -> Option<Self> {
        match ch {
            'a'..='z' => Some(Self::LatinA.offset(ch as u8 - b'a')),
            'A'..='Z' => Some(Self::LatinA.offset(ch as u8 - b'A')),
            AAYDHAM => Some(Self::Aaydham),
            _ => {
                // Don't include vowel signs as they can't stand on their own
                TAMIL_VOWEL_MAP
                    .get(&ch)
                    .or_else(|| TAMIL_CONSONANT_MAP.get(&ch))
                    .copied()
            }
        }
    }

    pub fn join(mut prefix: Vec<Self>, suffix: &Word) -> Vec<Vec<Self>> {
        use Category::*;

        if suffix.is_empty() {
            return vec![prefix];
        } else if prefix.is_empty() {
            return vec![Vec::from(suffix.as_ref())];
        }

        let left = *prefix.last().unwrap();
        let right = suffix.first().unwrap();
        let joins: Vec<Vec<Self>> = match (left.category(), right.category()) {
            // Joining two vowels together
            (TamilVowel, TamilVowel) => {
                match left {
                    // Kutriyal ugaram
                    Self::U => {
                        prefix.pop();
                        vec![vec![], vec![Self::U], vec![Self::U, Self::V]]
                    }

                    // Joining with "y"
                    Self::I | Self::LongI | Self::E | Self::LongE | Self::Ai => {
                        vec![vec![], vec![Self::Y]]
                    }

                    // Joining with "v"
                    _ => vec![vec![], vec![Self::V]],
                }
            }

            // Joining a consonant with a vowel (could double)
            (TamilConsonant | TamilGrantha, TamilVowel) => {
                vec![vec![], vec![left]]
            }

            // Natural joining
            _ => {
                prefix.extend_from_slice(suffix.as_ref());
                return vec![prefix];
            }
        };

        let mut result = Vec::new();
        for mut join in joins {
            let mut word = prefix.clone();
            word.append(&mut join);
            word.extend_from_slice(suffix.as_ref());
            result.push(word);
        }

        result
    }

    pub fn is_vowel(self) -> bool {
        match self as u8 {
            Self::VOWEL_START..=Self::VOWEL_END => true,
            _ => false,
        }
    }

    pub fn is_consonant(self) -> bool {
        match self as u8 {
            Self::CONSONANT_START..=Self::CONSONANT_END => true,
            _ => false,
        }
    }

    pub fn pairs_with(self, rhs: Self) -> bool {
        if !LetterSet::vallinam().matches(rhs) {
            return false;
        }

        if self == rhs {
            return true;
        }

        rhs.offset(1) == self
    }

    pub fn paired(self) -> Option<Self> {
        if LetterSet::vallinam().matches(self) {
            Some(self.offset(1))
        } else if LetterSet::mellinam().matches(self) {
            Some(self.negative_offset(1))
        } else {
            None
        }
    }

    #[rustfmt::skip]
    pub fn category(self) -> Category {
        match self as u8 {
            Self::VOWEL_START..=Self::VOWEL_END =>
                Category::TamilVowel,

            Self::AAYDHAM =>
                Category::TamilAaydham,

            Self::TAMIL_CONSONANT_START..=Self::TAMIL_CONSONANT_END =>
                Category::TamilConsonant,

            Self::GRANTHA_START..=Self::GRANTHA_END =>
                Category::TamilGrantha,

            Self::LATIN_START..=Self::LATIN_END =>
                Category::LatinAlpha,

            ch => unreachable!("invalid character: {}", ch),
        }
    }

    pub fn range(self, to: Self) -> Result<LetterSet, (Category, Category)> {
        let a = self.category();
        let b = to.category();
        if a == b {
            Ok(LetterSet::range(self as u8, to as u8))
        } else {
            Err((a, b))
        }
    }
}

impl Display for Letter {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ch = *self as u8;
        match ch {
            Self::VOWEL_START..=Self::VOWEL_END =>
                write!(f, "{}", TAMIL_VOWELS[ch as usize]),

            Self::AAYDHAM =>
                write!(f, "{}", AAYDHAM),

            Self::CONSONANT_START..=Self::CONSONANT_END =>
                write!(f, "{}{}", TAMIL_CONSONANTS[(ch - Self::CONSONANT_START) as usize], PULLI),

            Self::LATIN_START..=Self::LATIN_END =>
                write!(f, "{}", (ch - Self::LATIN_START + b'a') as char),

            _ => unreachable!("invalid character: {}", ch),
        }
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct LetterSet(pub u64);

impl LetterSet {
    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn any() -> Self {
        Self::empty().complement()
    }

    pub const fn single(lt: Letter) -> Self {
        Self(1 << lt as u8)
    }

    pub const fn is_empty(self) -> bool {
        (self.0 & ((1 << Letter::LETTER_COUNT) - 1)) == 0
    }

    pub const fn is_any(self) -> bool {
        self.complement().is_empty()
    }

    pub const fn complement(self) -> Self {
        Self(!self.0)
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn intersect(self, other: Self) -> Self {
        Self(self.0 & other.0)
    }

    const fn range(start: u8, end: u8) -> Self {
        if end < start {
            Self::empty()
        } else {
            Self(((1 << (end + 1)) - 1) & !((1 << start) - 1))
        }
    }

    pub const fn vowel() -> Self {
        Self::range(Letter::VOWEL_START, Letter::VOWEL_END)
    }

    pub const fn kuril() -> Self {
        letterset![A, I, U, E, O]
    }

    pub const fn nedil() -> Self {
        letterset![LongA, LongI, LongU, LongE, Ai, LongO, Au]
    }

    pub const fn consonant() -> Self {
        Self::range(Letter::CONSONANT_START, Letter::CONSONANT_END)
    }

    pub const fn vallinam() -> Self {
        letterset![K, Ch, RetroT, T, P, AlveolarR]
    }

    pub const fn idaiyinam() -> Self {
        Self::glide().union(Self::rhotic()).union(Self::lateral())
    }

    pub const fn mellinam() -> Self {
        letterset![Ng, Ny, RetroN, N, M, AlveolarN]
    }

    pub const fn grantha() -> Self {
        Self::range(Letter::GRANTHA_START, Letter::GRANTHA_END)
    }

    pub const fn latin() -> Self {
        Self::vowel()
            .union(Self::single(Letter::Aaydham))
            .union(Self::consonant())
            .complement()
    }

    pub const fn glide() -> Self {
        letterset![Y, V]
    }

    pub const fn rhotic() -> Self {
        letterset![R, Zh]
    }

    pub const fn lateral() -> Self {
        letterset![AlveolarL, RetroL]
    }

    pub const fn tamil_initial() -> Self {
        Self::vowel()
            .union(Self::glide())
            .union(letterset![K, Ch, Ny, T, N, P, M])
    }

    pub const fn tamil_final() -> Self {
        Self::vowel()
            .union(Self::rhotic())
            .union(Self::lateral())
            .union(letterset![RetroN, M, Y, AlveolarN])
    }

    pub const fn vowel_with_v() -> Self {
        letterset![A, LongA, U, LongU, O, LongO, Au]
    }

    pub const fn vowel_with_y() -> Self {
        letterset![I, LongI, E, LongE, Ai]
    }

    pub fn to_single(mut self) -> Option<Letter> {
        let mut n = 0;
        while (self.0 & 1) == 0 {
            self.0 >>= 1;
            n += 1;

            if n > Letter::LETTER_END {
                return None;
            }
        }

        if (self.0 ^ 1) == 0 {
            Some(Letter::expect_from(n))
        } else {
            None
        }
    }

    pub fn is_complement(self) -> bool {
        (self.0 & (1 << Letter::LETTER_COUNT)) != 0
    }

    pub fn matches(self, lt: Letter) -> bool {
        (self.0 & (1 << lt as u8)) != 0
    }

    pub fn parse_escape(ch: char) -> Option<Self> {
        match ch {
            'V' => Some(Self::vowel()),
            'C' => Some(Self::consonant()),
            'P' => Some(Self::vallinam()),
            'N' => Some(Self::mellinam()),
            'G' => Some(Self::glide()),
            'R' => Some(Self::rhotic()),
            'L' => Some(Self::lateral()),

            'k' => Some(Self::kuril()),
            'n' => Some(Self::nedil()),
            'v' => Some(Self::vallinam()),
            'i' => Some(Self::idaiyinam()),
            'm' => Some(Self::mellinam()),
            'g' => Some(Self::grantha()),
            'l' => Some(Self::latin()),

            '<' | '^' => Some(Self::tamil_initial()),
            '>' | '$' => Some(Self::tamil_final()),

            _ => None,
        }
    }

    pub fn iter(self) -> impl Iterator<Item = Letter> {
        (Letter::LETTER_START..=Letter::LETTER_END)
            .map(Letter::expect_from)
            .filter(move |&lt| self.matches(lt))
    }
}

impl Display for LetterSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut lts = *self;

        write!(f, "[")?;

        if lts.is_complement() {
            write!(f, "^")?;
            lts = lts.complement();
        }

        for lt in lts.iter() {
            write!(f, "{}", lt)?;
        }

        write!(f, "]")
    }
}

impl Debug for LetterSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Word([Letter]);

impl Word {
    pub fn new() -> &'static Self {
        word![]
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn first(&self) -> Option<Letter> {
        self.0.first().cloned()
    }

    pub fn last(&self) -> Option<Letter> {
        self.0.last().cloned()
    }

    pub fn get(&self, index: usize) -> Option<Letter> {
        self.0.get(index).copied()
    }

    pub fn iter(&self) -> WordIter {
        WordIter::new(self)
    }

    pub fn contains(&self, lts: LetterSet) -> bool {
        self.iter().any(|lt| lts.matches(lt))
    }

    pub fn take_prefix<'a, 'b>(&'a self, prefix: &'b Self) -> Option<&'a Self> {
        if prefix.len() > self.len() {
            return None;
        }

        for (a, b) in self.iter().zip(prefix) {
            if a != b {
                return None;
            }
        }

        Some(&self[0..prefix.len()])
    }

    pub fn parse(s: &str) -> Box<Word> {
        Self::parse_unboxed(s).into()
    }

    pub fn parse_unboxed(s: &str) -> Vec<Letter> {
        let mut word = Vec::new();
        for ch in s.chars() {
            match ch {
                'a'..='z' => word.push(Letter::LatinA.offset(ch as u8 - b'a')),
                'A'..='Z' => word.push(Letter::LatinA.offset(ch as u8 - b'A')),
                AAYDHAM => word.push(Letter::Aaydham),
                PULLI => {
                    match word.pop() {
                        None => {}

                        // Remove inherent 'a'
                        Some(Letter::A) => {}

                        // Convert 'aa' + pulli into 'r'
                        Some(Letter::LongA) => word.push(Letter::R),

                        // Convert short 'o' + pulli into short 'e' + 'r'
                        Some(Letter::O) => {
                            word.push(Letter::E);
                            word.push(Letter::R);
                        }

                        // Convert long 'o' + pulli into long 'e' + 'r'
                        Some(Letter::LongO) => {
                            word.push(Letter::LongE);
                            word.push(Letter::R);
                        }

                        // Convert 'au' + pulli into short 'o' + 'L' at the start of a word
                        Some(Letter::Au) if word.is_empty() => {
                            word.push(Letter::O);
                            word.push(Letter::RetroL);
                        }

                        // Convert 'au' + pulli into short 'e' + 'L' otherwise
                        Some(Letter::Au) => {
                            word.push(Letter::E);
                            word.push(Letter::RetroL);
                        }

                        Some(x) => word.push(x),
                    }
                }

                // Handle combining 'La'
                COMBINING_LA => {
                    match word.pop() {
                        None => {}

                        // Combine with previous short 'e' or 'o' to form 'au'
                        Some(Letter::E | Letter::O) => {
                            word.push(Letter::Au);
                            continue;
                        }

                        Some(x) => word.push(x),
                    }

                    // Treat as 'La'
                    word.push(Letter::RetroL);
                    word.push(Letter::A);
                }

                // Handle combined 'om'
                OM => {
                    word.push(Letter::LongO);
                    word.push(Letter::M);
                }

                _ => {
                    if let Some(&n) = TAMIL_VOWEL_MAP.get(&ch) {
                        word.push(n);
                    } else if let Some(&n) = TAMIL_CONSONANT_MAP.get(&ch) {
                        word.push(n);
                        word.push(Letter::A);
                    } else if let Some(&n) = TAMIL_VOWEL_SIGN_MAP.get(&ch) {
                        match word.pop() {
                            None => {}

                            // Remove inherent 'a' before adding vowel
                            Some(Letter::A) => {}

                            // Convert short 'e' + 'aa' into short 'o'
                            Some(Letter::E) if n == Letter::LongA => {
                                word.push(Letter::O);
                                continue;
                            }

                            // Convert long 'e' + 'aa' into long 'o'
                            Some(Letter::LongE) if n == Letter::LongA => {
                                word.push(Letter::LongO);
                                continue;
                            }

                            Some(x) => word.push(x),
                        }

                        word.push(n);
                    }
                }
            }
        }

        word
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        for lt in self {
            let ch = lt as u8;
            match ch {
                Letter::VOWEL_START..=Letter::VOWEL_END => {
                    match s.pop() {
                        None => {}

                        // Remove pulli before adding vowel
                        Some(PULLI) => {
                            if ch >= Letter::VOWEL_SIGN_START {
                                s.push(TAMIL_VOWEL_SIGNS[(ch - Letter::VOWEL_SIGN_START) as usize]);
                            }
                            continue;
                        }

                        Some(x) => s.push(x),
                    }
                    s.push(TAMIL_VOWELS[ch as usize]);
                }

                Letter::AAYDHAM => s.push(AAYDHAM),

                Letter::CONSONANT_START..=Letter::CONSONANT_END => {
                    s.push(TAMIL_CONSONANTS[(ch - Letter::CONSONANT_START) as usize]);
                    s.push(PULLI);
                }

                Letter::LATIN_START..=Letter::LATIN_END => {
                    s.push((ch - Letter::LatinA as u8 + b'a') as char);
                }

                _ => unreachable!("invalid character: {}", ch),
            }
        }

        write!(f, "{}", s)
    }
}

impl Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct WordIter<'a> {
    pub word: &'a Word,
    pub index: usize,
}

impl<'a> WordIter<'a> {
    pub fn new(word: &'a Word) -> Self {
        Self { word, index: 0 }
    }

    pub fn prev(&self) -> Option<Letter> {
        self.index.checked_sub(2).map(|i| self.word[i])
    }

    pub fn peek(&self) -> Option<Letter> {
        self.word.get(self.index)
    }

    pub fn adv(&mut self) {
        self.index += 1;
        debug_assert!(self.index <= self.word.len());
    }

    pub fn remaining(&self) -> &'a Word {
        &self.word[self.index..]
    }

    pub fn remaining_with_offset(&self, offset: isize) -> &'a Word {
        &self.word[((self.index as isize + offset) as usize)..]
    }

    pub fn remaining_count(&self) -> usize {
        self.word.len() - self.index
    }
}

impl<'a> Iterator for WordIter<'a> {
    type Item = Letter;

    fn next(&mut self) -> Option<Letter> {
        let letter = self.word.get(self.index)?;
        self.index += 1;
        Some(letter)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.remaining_count();
        (remaining, Some(remaining))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LetterBase {
    Single(Letter),
    Double(Letter, Letter),
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LetterCombination {
    pub base: LetterBase,
    pub combining: Option<Letter>,
}

impl LetterCombination {
    pub fn take(
        iter: &mut std::iter::Peekable<impl Iterator<Item = Letter>>,
    ) -> Option<LetterCombination> {
        if let Some(letter) = iter.next() {
            let mut base = LetterBase::Single(letter);

            // Check for consonant which may have combining letters
            if letter.is_consonant() {
                // Check for 'ksh' cluster
                if letter == Letter::K && iter.peek() == Some(&Letter::Sh) {
                    iter.next();
                    base = LetterBase::Double(letter, Letter::Sh);
                }

                // Check for combining vowel
                if let Some(combining) = iter.peek() {
                    if combining.is_vowel() {
                        return Some(LetterCombination {
                            base,
                            combining: iter.next(),
                        });
                    }
                }
            }

            return Some(LetterCombination {
                base,
                combining: None,
            });
        }

        None
    }
}

impl Ord for Word {
    fn cmp(&self, rhs: &Self) -> Ordering {
        let mut a = self.iter().peekable();
        let mut b = rhs.iter().peekable();
        loop {
            match (
                LetterCombination::take(&mut a),
                LetterCombination::take(&mut b),
            ) {
                (Some(a), Some(b)) => match a.cmp(&b) {
                    Ordering::Equal => {}
                    other => return other,
                },
                (Some(_), None) => return Ordering::Greater,
                (None, Some(_)) => return Ordering::Less,
                (None, None) => return Ordering::Equal,
            }
        }
    }
}

impl PartialOrd for Word {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}

impl<'a> From<&'a [Letter]> for &'a Word {
    fn from(lts: &'a [Letter]) -> Self {
        let ptr = lts as *const [Letter] as *const Word;
        unsafe { &*ptr }
    }
}

impl<'a> From<&'a mut [Letter]> for &'a mut Word {
    fn from(lts: &'a mut [Letter]) -> Self {
        let ptr = lts as *mut [Letter] as *mut Word;
        unsafe { &mut *ptr }
    }
}

impl<'a> From<&'a Word> for Box<Word> {
    fn from(lts: &'a Word) -> Self {
        let boxed: Box<[Letter]> = Box::from(&lts.0);
        Self::from(boxed)
    }
}

impl<'a> From<&'a [Letter]> for Box<Word> {
    fn from(lts: &'a [Letter]) -> Self {
        let boxed: Box<[Letter]> = Box::from(lts);
        Self::from(boxed)
    }
}

impl From<Vec<Letter>> for Box<Word> {
    fn from(lts: Vec<Letter>) -> Self {
        Self::from(lts.into_boxed_slice())
    }
}

impl From<Box<[Letter]>> for Box<Word> {
    fn from(boxed: Box<[Letter]>) -> Self {
        let ptr = Box::into_raw(boxed) as *mut Word;
        unsafe { Box::from_raw(ptr) }
    }
}

impl AsMut<[Letter]> for Word {
    fn as_mut(&mut self) -> &mut [Letter] {
        &mut self.0
    }
}

impl AsRef<[Letter]> for Word {
    fn as_ref(&self) -> &[Letter] {
        &self.0
    }
}

impl<'a> IntoIterator for &'a Word {
    type Item = Letter;
    type IntoIter = WordIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut Word {
    type Item = &'a mut Letter;
    type IntoIter = std::slice::IterMut<'a, Letter>;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

impl Index<usize> for Word {
    type Output = Letter;

    fn index(&self, index: usize) -> &Letter {
        self.0.index(index).into()
    }
}

impl IndexMut<usize> for Word {
    fn index_mut(&mut self, index: usize) -> &mut Letter {
        self.0.index_mut(index).into()
    }
}

#[doc(hiddden)]
macro_rules! index_impl {
    () => {};
    ($ty:ty; $($tt:tt)*) => {
        impl Index<$ty> for Word {
            type Output = Self;

            fn index(&self, index: $ty) -> &Self {
                self.0.index(index).into()
            }
        }

        impl IndexMut<$ty> for Word {
            fn index_mut(&mut self, index: $ty) -> &mut Self {
                self.0.index_mut(index).into()
            }
        }

        index_impl!($($tt)*);
    };
}

index_impl! {
    RangeFull;
    RangeFrom<usize>;
    RangeTo<usize>;
    RangeToInclusive<usize>;
    Range<usize>;
    RangeInclusive<usize>;
}
