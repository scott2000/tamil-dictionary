use std::fmt::{self, Display, Debug};
use std::collections::{HashMap, HashSet};

macro_rules! letters {
    ($lt:ident, $($tt:tt)*) => {
        mod num {
            letters!([NUMBER 0] $lt, $($tt)*);
        }

        impl Letter {
            letters!([LETTER 0] $lt, $($tt)*);
        }
    };
    ([$_:ident $n:expr]) => {};
    ([NUMBER   $n:expr] $lt:ident, $($tt:tt)*) => {
        pub const $lt: u8 = $n;
        letters!([NUMBER $n + 1] $($tt)*);
    };
    ([LETTER   $n:expr] $lt:ident, $($tt:tt)*) => {
        pub const $lt: Letter = Letter(num::$lt);
        letters!([LETTER $n + 1] $($tt)*);
    };
}

letters! {
    SHORT_A,         LONG_A,           SHORT_I,      LONG_I,            SHORT_U,           LONG_U,
    SHORT_E,         LONG_E,           AI,           SHORT_O,           LONG_O,            AU,
    AAYDHAM,         VELAR_PLOSIVE,    VELAR_NASAL,  PALATAL_AFFRICATE, PALATAL_NASAL,     RETROFLEX_PLOSIVE,
    RETROFLEX_NASAL, DENTAL_PLOSIVE,   DENTAL_NASAL, LABIAL_PLOSIVE,    LABIAL_NASAL,      PALATAL_GLIDE,
    ALVEOLAR_RHOTIC, ALVEOLAR_LATERAL, LABIAL_GLIDE, RETROFLEX_RHOTIC,  RETROFLEX_LATERAL, ALVEOLAR_PLOSIVE,
    ALVEOLAR_NASAL,  GRANTHA_J,        GRANTHA_S,    GRANTHA_SH,        GRANTHA_H,         GRANTHA_SSH,
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

pub const PULLI: char = '\u{bcd}';
pub const COMBINING_LA: char = '\u{bd7}';
pub const OM: char = '\u{bd0}';

const TAMIL_VOWELS: &'static [char] = &[
    'அ', 'ஆ',
    'இ', 'ஈ',
    'உ', 'ஊ',
    'எ', 'ஏ', 'ஐ',
    'ஒ', 'ஓ', 'ஔ',
    'ஃ',
];

const TAMIL_VOWEL_SIGNS: &'static [char] = &[
    '\u{bbe}',
    '\u{bbf}', '\u{bc0}',
    '\u{bc1}', '\u{bc2}',
    '\u{bc6}', '\u{bc7}', '\u{bc8}',
    '\u{bca}', '\u{bcb}', '\u{bcc}',
];

const TAMIL_CONSONANTS: &'static [char] = &[
    'க', 'ங',
    'ச', 'ஞ',
    'ட', 'ண',
    'த', 'ந',
    'ப', 'ம',
    'ய', 'ர', 'ல', 'வ', 'ழ', 'ள',
    'ற', 'ன',
    'ஜ', 'ஸ', 'ஷ', 'ஹ', 'ஶ',
];

lazy_static! {
    static ref TAMIL_VOWEL_MAP: HashMap<char, Letter> = to_map(num::SHORT_A, TAMIL_VOWELS);
    static ref TAMIL_VOWEL_SIGN_MAP: HashMap<char, Letter> = to_map(num::LONG_A, TAMIL_VOWEL_SIGNS);
    static ref TAMIL_CONSONANT_MAP: HashMap<char, Letter> = to_map(num::VELAR_PLOSIVE, TAMIL_CONSONANTS);
    static ref VALID_LETTERS: HashSet<char> = {
        let mut set = HashSet::new();
        for ch in TAMIL_VOWELS.iter()
            .chain(TAMIL_VOWEL_SIGNS)
            .chain(TAMIL_CONSONANTS)
            .cloned()
            .chain([PULLI, COMBINING_LA, OM, '\u{200b}', '\u{200c}', '\u{200d}'])
            .chain('a'..='z')
            .chain('A'..='Z')
        {
            set.insert(ch);
        }
        set
    };
}

fn to_map(mut offset: u8, chars: &'static [char]) -> HashMap<char, Letter> {
    let mut map = HashMap::new();
    for &ch in chars {
        map.insert(ch, Letter(offset));
        offset += 1;
    }
    map
}

pub type Word = [Letter];

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Letter(u8);

impl Letter {
    pub fn is_valid(ch: char) -> bool {
        VALID_LETTERS.contains(&ch)
    }

    pub fn parse(ch: char) -> Option<Self> {
        match ch {
            'a'..='z' => Some(Self(ch as u8 - b'a' + num::A)),
            'A'..='Z' => Some(Self(ch as u8 - b'A' + num::A)),
            _ => {
                // Don't include vowel signs as they can't stand on their own
                TAMIL_VOWEL_MAP.get(&ch)
                    .or_else(|| TAMIL_CONSONANT_MAP.get(&ch))
                    .copied()
            }
        }
    }

    pub fn parse_str(s: &str) -> Box<Word> {
        let mut word = Vec::new();
        for ch in s.chars() {
            match ch {
                'a'..='z' => word.push(Self(ch as u8 - b'a' + num::A)),
                'A'..='Z' => word.push(Self(ch as u8 - b'A' + num::A)),
                PULLI => {
                    match word.pop() {
                        None => {}

                        // Remove inherent 'a'
                        Some(Self::SHORT_A) => {}

                        // Convert 'aa' + pulli into 'r'
                        Some(Self::LONG_A) => word.push(Self::ALVEOLAR_RHOTIC),

                        // Convert short 'o' + pulli into short 'e' + 'r'
                        Some(Self::SHORT_O) => {
                            word.push(Self::SHORT_E);
                            word.push(Self::ALVEOLAR_RHOTIC);
                        }

                        // Convert long 'o' + pulli into long 'e' + 'r'
                        Some(Self::LONG_O) => {
                            word.push(Self::LONG_E);
                            word.push(Self::ALVEOLAR_RHOTIC);
                        }

                        // Convert 'au' + pulli into short 'o' + 'L' at the start of a word
                        Some(Self::AU) if word.is_empty() => {
                            word.push(Self::SHORT_O);
                            word.push(Self::RETROFLEX_LATERAL);
                        }

                        // Convert 'au' + pulli into short 'e' + 'L' otherwise
                        Some(Self::AU) => {
                            word.push(Self::SHORT_E);
                            word.push(Self::RETROFLEX_LATERAL);
                        }

                        Some(x) => word.push(x),
                    }
                }

                // Handle combining 'La'
                COMBINING_LA => {
                    match word.pop() {
                        None => {}

                        // Combine with previous short 'e' or 'o' to form 'au'
                        Some(Self::SHORT_E | Self::SHORT_O) => {
                            word.push(Self::AU);
                            continue;
                        }

                        Some(x) => word.push(x),
                    }

                    // Treat as 'La'
                    word.push(Self::RETROFLEX_LATERAL);
                    word.push(Self::SHORT_A);
                }

                // Handle combined 'om'
                OM => {
                    word.push(Self::LONG_O);
                    word.push(Self::LABIAL_NASAL);
                }

                _ => {
                    if let Some(&n) = TAMIL_VOWEL_MAP.get(&ch) {
                        word.push(n);
                    } else if let Some(&n) = TAMIL_CONSONANT_MAP.get(&ch) {
                        word.push(n);
                        word.push(Self::SHORT_A);
                    } else if let Some(&n) = TAMIL_VOWEL_SIGN_MAP.get(&ch) {
                        match word.pop() {
                            None => {}

                            // Remove inherent 'a' before adding vowel
                            Some(Self::SHORT_A) => {}

                            // Convert short 'e' + 'aa' into short 'o'
                            Some(Self::SHORT_E) if n == Self::LONG_A => {
                                word.push(Self::SHORT_O);
                                continue;
                            }

                            // Convert long 'e' + 'aa' into long 'o'
                            Some(Self::LONG_E) if n == Self::LONG_A => {
                                word.push(Self::LONG_O);
                                continue;
                            }

                            Some(x) => word.push(x),
                        }

                        word.push(n);
                    }
                }
            }
        }
        word.into_boxed_slice()
    }

    pub fn to_char(self) -> char {
        let ch = self.0;
        match ch {
            num::SHORT_A..=num::AAYDHAM => TAMIL_VOWELS[ch as usize],
            num::VELAR_PLOSIVE..=num::GRANTHA_SSH => TAMIL_CONSONANTS[(ch - num::VELAR_PLOSIVE) as usize],
            num::A..=num::Z => (ch - num::A + b'a') as char,
            _ => unreachable!("invalid character: {}", ch),
        }
    }

    pub fn to_str(word: &Word) -> String {
        let mut s = String::new();
        for &Letter(ch) in word {
            match ch {
                num::SHORT_A..=num::AU => {
                    match s.pop() {
                        None => {},

                        // Remove pulli before adding vowel
                        Some(PULLI) => {
                            if ch > num::SHORT_A {
                                s.push(TAMIL_VOWEL_SIGNS[(ch - num::LONG_A) as usize]);
                            }
                            continue;
                        },

                        Some(x) => s.push(x),
                    }
                    s.push(TAMIL_VOWELS[ch as usize]);
                },
                num::AAYDHAM => s.push('ஃ'),
                num::VELAR_PLOSIVE..=num::GRANTHA_SSH => {
                    s.push(TAMIL_CONSONANTS[(ch - num::VELAR_PLOSIVE) as usize]);
                    s.push(PULLI);
                },
                num::A..=num::Z => {
                    s.push((ch - num::A + b'a') as char);
                },
                _ => unreachable!("invalid character: {}", ch),
            }
        }
        s
    }

    pub fn is_consonant(self) -> bool {
        match self.0 {
            num::VELAR_PLOSIVE..=num::GRANTHA_SSH => true,
            _ => false,
        }
    }

    pub fn category(self) -> Category {
        match self.0 {
            num::SHORT_A..=num::AU => Category::TamilVowel,
            num::AAYDHAM => Category::TamilAaydham,
            num::VELAR_PLOSIVE..=num::ALVEOLAR_NASAL => Category::TamilConsonant,
            num::GRANTHA_J..=num::GRANTHA_SSH => Category::TamilGrantha,
            num::A..=num::Z => Category::LatinAlpha,
            ch => unreachable!("invalid character: {}", ch),
        }
    }

    pub fn range(self, to: Self) -> Result<impl Iterator<Item = Self>, (Category, Category)> {
        let a = self.category();
        let b = to.category();
        if a == b {
            Ok((self.0..=to.0).map(Self))
        } else {
            Err((a, b))
        }
    }
}

impl Display for Letter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_char())
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

#[derive(Copy, Clone, Debug)]
pub struct LetterSet(pub u64);

impl LetterSet {
    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn any() -> Self {
        Self::empty()
            .complement()
    }

    pub const fn single(lt: Letter) -> Self {
        Self(1 << lt.0)
    }

    pub const fn is_empty(self) -> bool {
        (self.0 & ((1 << 62) - 1)) == 0
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

    pub const fn vowel() -> Self {
        Self::kuril()
            .union(Self::nedil())
    }

    pub const fn kuril() -> Self {
        Self::single(Letter::SHORT_A)
            .union(Self::single(Letter::SHORT_I))
            .union(Self::single(Letter::SHORT_U))
            .union(Self::single(Letter::SHORT_E))
            .union(Self::single(Letter::SHORT_O))
    }

    pub const fn nedil() -> Self {
        Self::single(Letter::LONG_A)
            .union(Self::single(Letter::LONG_I))
            .union(Self::single(Letter::LONG_U))
            .union(Self::single(Letter::LONG_E))
            .union(Self::single(Letter::AI))
            .union(Self::single(Letter::LONG_O))
            .union(Self::single(Letter::AU))
    }

    pub const fn consonant() -> Self {
        Self::vallinam()
            .union(Self::idaiyinam())
            .union(Self::mellinam())
    }

    pub const fn vallinam() -> Self {
        Self::single(Letter::VELAR_PLOSIVE)
            .union(Self::single(Letter::PALATAL_AFFRICATE))
            .union(Self::single(Letter::RETROFLEX_PLOSIVE))
            .union(Self::single(Letter::DENTAL_PLOSIVE))
            .union(Self::single(Letter::LABIAL_PLOSIVE))
            .union(Self::single(Letter::ALVEOLAR_PLOSIVE))
    }

    pub const fn idaiyinam() -> Self {
        Self::glide()
            .union(Self::rhotic())
            .union(Self::lateral())
    }

    pub const fn mellinam() -> Self {
        Self::single(Letter::VELAR_NASAL)
            .union(Self::single(Letter::PALATAL_NASAL))
            .union(Self::single(Letter::RETROFLEX_NASAL))
            .union(Self::single(Letter::DENTAL_NASAL))
            .union(Self::single(Letter::LABIAL_NASAL))
            .union(Self::single(Letter::ALVEOLAR_NASAL))
    }

    pub const fn grantha() -> Self {
        Self::single(Letter::GRANTHA_J)
            .union(Self::single(Letter::GRANTHA_S))
            .union(Self::single(Letter::GRANTHA_SH))
            .union(Self::single(Letter::GRANTHA_H))
            .union(Self::single(Letter::GRANTHA_SSH))
    }

    pub const fn latin() -> Self {
        Self::vowel()
            .union(Self::single(Letter::AAYDHAM))
            .union(Self::consonant())
            .complement()
    }

    pub const fn glide() -> Self {
        Self::single(Letter::PALATAL_GLIDE)
            .union(Self::single(Letter::LABIAL_GLIDE))
    }

    pub const fn rhotic() -> Self {
        Self::single(Letter::ALVEOLAR_RHOTIC)
            .union(Self::single(Letter::RETROFLEX_RHOTIC))
    }

    pub const fn lateral() -> Self {
        Self::single(Letter::ALVEOLAR_LATERAL)
            .union(Self::single(Letter::RETROFLEX_LATERAL))
    }

    pub const fn tamil_initial() -> Self {
        Self::vowel()
            .union(Self::single(Letter::VELAR_PLOSIVE))
            .union(Self::single(Letter::PALATAL_AFFRICATE))
            .union(Self::single(Letter::PALATAL_NASAL))
            .union(Self::single(Letter::DENTAL_PLOSIVE))
            .union(Self::single(Letter::DENTAL_NASAL))
            .union(Self::single(Letter::LABIAL_PLOSIVE))
            .union(Self::single(Letter::LABIAL_NASAL))
            .union(Self::glide())
    }

    pub const fn tamil_final() -> Self {
        Self::vowel()
            .union(Self::single(Letter::RETROFLEX_NASAL))
            .union(Self::single(Letter::LABIAL_NASAL))
            .union(Self::single(Letter::ALVEOLAR_NASAL))
            .union(Self::single(Letter::PALATAL_GLIDE))
            .union(Self::rhotic())
            .union(Self::lateral())
    }

    pub fn matches(self, lt: Letter) -> bool {
        (self.0 & (1 << lt.0)) != 0
    }

    pub fn parse_escape(ch: char) -> Option<LetterSet> {
        match ch {
            'V' => Some(LetterSet::vowel()),
            'C' => Some(LetterSet::consonant()),
            'P' => Some(LetterSet::vallinam()),
            'N' => Some(LetterSet::mellinam()),
            'G' => Some(LetterSet::glide()),
            'R' => Some(LetterSet::rhotic()),
            'L' => Some(LetterSet::lateral()),

            'k' => Some(LetterSet::kuril()),
            'n' => Some(LetterSet::nedil()),
            'v' => Some(LetterSet::vallinam()),
            'i' => Some(LetterSet::idaiyinam()),
            'm' => Some(LetterSet::mellinam()),
            'g' => Some(LetterSet::grantha()),
            'l' => Some(LetterSet::latin()),

            '<' | '^' => Some(LetterSet::tamil_initial()),
            '>' | '$' => Some(LetterSet::tamil_final()),

            _ => None,
        }
    }
}

impl Display for LetterSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut lts = *self;

        write!(f, "[")?;

        let flip = (lts.0 & (1 << 63)) != 0;
        if flip {
            write!(f, "!")?;
            lts = lts.complement();
        }

        for i in 0..62 {
            if lts.matches(Letter(i)) != flip {
                write!(f, "{}", Letter(i))?;
            }
        }

        write!(f, "]")
    }
}
