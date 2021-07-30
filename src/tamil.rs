use std::fmt::{self, Display, Debug};
use std::collections::{HashMap, HashSet};

#[doc(hidden)]
macro_rules! letters_impl {
    ([$($_:tt)*]) => {};
    ([mod $n:expr] [$lt:ident] $($tt:tt)*) => {
        pub const $lt: u8 = $n;
        letters_impl!([mod $n] $($tt)*);
    };
    ([mod $n:expr] [..$lt:ident] $($tt:tt)*) => {
        pub const $lt: u8 = $n - 1;
        letters_impl!([mod $n] $($tt)*);
    };
    ([mod $n:expr] $lt:ident, $($tt:tt)*) => {
        pub const $lt: u8 = $n;
        letters_impl!([mod $n + 1] $($tt)*);
    };
    ([impl $n:expr] [$($_:tt)*] $($tt:tt)*) => {
        letters_impl!([impl $n] $($tt)*);
    };
    ([impl $n:expr] $lt:ident, $($tt:tt)*) => {
        pub const $lt: Letter = Letter(num::$lt);
        letters_impl!([impl $n + 1] $($tt)*);
    };
}

#[doc(hidden)]
macro_rules! letters {
    ($($tt:tt)*) => {
        pub mod num {
            letters_impl!([mod 0] $($tt)*);
        }

        impl Letter {
            letters_impl!([impl 0] $($tt)*);
        }
    };
}

letters! {
    [VOWEL_START]
    SHORT_A, LONG_A,
    SHORT_I, LONG_I,
    SHORT_U, LONG_U,
    SHORT_E, LONG_E, AI,
    SHORT_O, LONG_O, AU,
    [..VOWEL_END]

    AAYDHAM,

    [CONSONANT_START]
    TAMIL_K,           TAMIL_NG,
    TAMIL_CH,          TAMIL_NY,
    TAMIL_RETRO_T,     TAMIL_RETRO_N,
    TAMIL_T,           TAMIL_N,
    TAMIL_P,           TAMIL_M,

    TAMIL_Y, TAMIL_R,  TAMIL_ALVEOLAR_L,
    TAMIL_V, TAMIL_ZH, TAMIL_RETRO_L,

    TAMIL_ALVEOLAR_TR, TAMIL_ALVEOLAR_N,
    [..TAMIL_CONSONANT_END]

    [GRANTHA_START]
    GRANTHA_J, GRANTHA_S, GRANTHA_SH, GRANTHA_H, GRANTHA_SSH,
    [..GRANTHA_END]
    [..CONSONANT_END]

    LATIN_A, LATIN_B, LATIN_C, LATIN_D, LATIN_E, LATIN_F,
    LATIN_G, LATIN_H, LATIN_I, LATIN_J, LATIN_K, LATIN_L,
    LATIN_M, LATIN_N, LATIN_O, LATIN_P, LATIN_Q, LATIN_R, LATIN_S,
    LATIN_T, LATIN_U, LATIN_V, LATIN_W, LATIN_X, LATIN_Y, LATIN_Z,
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
];

const TAMIL_VOWEL_SIGNS: &'static [char] = &[
    '\u{bbe}',
    '\u{bbf}', '\u{bc0}',
    '\u{bc1}', '\u{bc2}',
    '\u{bc6}', '\u{bc7}', '\u{bc8}',
    '\u{bca}', '\u{bcb}', '\u{bcc}',
];

const AAYDHAM: char = 'ஃ';

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
    static ref TAMIL_VOWEL_MAP: HashMap<char, Letter> = to_map(num::VOWEL_START, TAMIL_VOWELS);
    static ref TAMIL_VOWEL_SIGN_MAP: HashMap<char, Letter> = to_map(num::LONG_A, TAMIL_VOWEL_SIGNS);
    static ref TAMIL_CONSONANT_MAP: HashMap<char, Letter> = to_map(num::CONSONANT_START, TAMIL_CONSONANTS);
    static ref VALID_LETTERS: HashSet<char> = {
        let mut set = HashSet::new();
        for ch in TAMIL_VOWELS.iter()
            .chain(TAMIL_VOWEL_SIGNS)
            .chain(TAMIL_CONSONANTS)
            .cloned()
            .chain([AAYDHAM, PULLI, COMBINING_LA, OM, '\u{200b}', '\u{200c}', '\u{200d}'])
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
            'a'..='z' => Some(Self(ch as u8 - b'a' + num::LATIN_A)),
            'A'..='Z' => Some(Self(ch as u8 - b'A' + num::LATIN_A)),
            AAYDHAM => Some(Self::AAYDHAM),
            _ => {
                // Don't include vowel signs as they can't stand on their own
                TAMIL_VOWEL_MAP.get(&ch)
                    .or_else(|| TAMIL_CONSONANT_MAP.get(&ch))
                    .copied()
            }
        }
    }

    pub fn parse_str_unboxed(s: &str) -> Vec<Self> {
        let mut word = Vec::new();
        for ch in s.chars() {
            match ch {
                'a'..='z' => word.push(Self(ch as u8 - b'a' + num::LATIN_A)),
                'A'..='Z' => word.push(Self(ch as u8 - b'A' + num::LATIN_A)),
                AAYDHAM => word.push(Self::AAYDHAM),
                PULLI => {
                    match word.pop() {
                        None => {}

                        // Remove inherent 'a'
                        Some(Self::SHORT_A) => {}

                        // Convert 'aa' + pulli into 'r'
                        Some(Self::LONG_A) => word.push(Self::TAMIL_R),

                        // Convert short 'o' + pulli into short 'e' + 'r'
                        Some(Self::SHORT_O) => {
                            word.push(Self::SHORT_E);
                            word.push(Self::TAMIL_R);
                        }

                        // Convert long 'o' + pulli into long 'e' + 'r'
                        Some(Self::LONG_O) => {
                            word.push(Self::LONG_E);
                            word.push(Self::TAMIL_R);
                        }

                        // Convert 'au' + pulli into short 'o' + 'L' at the start of a word
                        Some(Self::AU) if word.is_empty() => {
                            word.push(Self::SHORT_O);
                            word.push(Self::TAMIL_RETRO_L);
                        }

                        // Convert 'au' + pulli into short 'e' + 'L' otherwise
                        Some(Self::AU) => {
                            word.push(Self::SHORT_E);
                            word.push(Self::TAMIL_RETRO_L);
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
                    word.push(Self::TAMIL_RETRO_L);
                    word.push(Self::SHORT_A);
                }

                // Handle combined 'om'
                OM => {
                    word.push(Self::LONG_O);
                    word.push(Self::TAMIL_M);
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

        word
    }

    pub fn parse_str(s: &str) -> Box<Word> {
        Self::parse_str_unboxed(s).into_boxed_slice()
    }

    pub fn to_str(word: &Word) -> String {
        let mut s = String::new();
        for &Letter(ch) in word {
            match ch {
                num::VOWEL_START..=num::VOWEL_END => {
                    match s.pop() {
                        None => {},

                        // Remove pulli before adding vowel
                        Some(PULLI) => {
                            if ch >= num::LONG_A {
                                s.push(TAMIL_VOWEL_SIGNS[(ch - num::LONG_A) as usize]);
                            }
                            continue;
                        },

                        Some(x) => s.push(x),
                    }
                    s.push(TAMIL_VOWELS[ch as usize]);
                },

                num::AAYDHAM => s.push(AAYDHAM),

                num::CONSONANT_START..=num::CONSONANT_END => {
                    s.push(TAMIL_CONSONANTS[(ch - num::CONSONANT_START) as usize]);
                    s.push(PULLI);
                },

                num::LATIN_A..=num::LATIN_Z => {
                    s.push((ch - num::LATIN_A + b'a') as char);
                },

                _ => unreachable!("invalid character: {}", ch),
            }
        }
        s
    }

    pub fn join(mut prefix: Vec<Self>, mut suffix: &Word) -> Vec<Vec<Self>> {
        use Category::*;

        if suffix.is_empty() {
            return vec![prefix];
        } else if prefix.is_empty() {
            return vec![Vec::from(suffix)];
        }

        let just_extend = |mut prefix: Vec<Self>, suffix: &Word| {
            prefix.extend_from_slice(suffix);
            vec![prefix]
        };

        let left = *prefix.last().unwrap();
        let right = *suffix.first().unwrap();
        let joins: Vec<Vec<Letter>> = match (left.category(), right.category()) {
            // Joining two vowels together
            (TamilVowel, TamilVowel) => {
                match left {
                    // Kutriyal ugaram
                    Self::SHORT_U => {
                        prefix.pop();
                        vec![vec![], vec![Letter::SHORT_U], vec![Letter::SHORT_U, Letter::TAMIL_V]]
                    },

                    // Joining with "v"
                    Self::SHORT_A | Self::LONG_A | Self::LONG_U | Self::SHORT_O | Self::LONG_O | Self::AU =>
                        vec![vec![], vec![Letter::TAMIL_V]],

                    // Joining with "y"
                    _ => vec![vec![], vec![Letter::TAMIL_Y]],
                }
            },

            // Joining a consonant with a vowel (could double)
            (TamilConsonant | TamilGrantha, TamilVowel) => {
                vec![vec![], vec![left]]
            },

            // Joining two consonants
            (TamilConsonant, TamilConsonant) => {
                if LetterSet::vallinam().matches(right) {
                    match left {
                        // Retroflex assimilation
                        Self::TAMIL_RETRO_T | Self::TAMIL_RETRO_N | Self::TAMIL_RETRO_L => {
                            prefix.pop();
                            suffix = &suffix[1..];
                            vec![
                                vec![Self::TAMIL_RETRO_T, right],
                                vec![Self::TAMIL_RETRO_N, right],
                                vec![Self::TAMIL_RETRO_L, right],
                                vec![Self::TAMIL_RETRO_T, Self::TAMIL_T],
                                vec![Self::TAMIL_RETRO_N, Self::TAMIL_T],
                                vec![Self::TAMIL_RETRO_L, Self::TAMIL_T],
                            ]
                        },

                        // Alveolar assimilation
                        Self::TAMIL_ALVEOLAR_TR | Self::TAMIL_ALVEOLAR_N | Self::TAMIL_ALVEOLAR_L => {
                            prefix.pop();
                            suffix = &suffix[1..];
                            vec![
                                vec![Self::TAMIL_ALVEOLAR_TR, right],
                                vec![Self::TAMIL_ALVEOLAR_N, right],
                                vec![Self::TAMIL_ALVEOLAR_L, right],
                                vec![Self::TAMIL_ALVEOLAR_TR, Self::TAMIL_T],
                                vec![Self::TAMIL_ALVEOLAR_N, Self::TAMIL_T],
                                vec![Self::TAMIL_ALVEOLAR_L, Self::TAMIL_T],
                            ]
                        },

                        // Nasal assimilation of "m"
                        Self::TAMIL_M => {
                            prefix.pop();
                            let paired = Letter(right.0 + 1);
                            vec![vec![paired], vec![Letter::TAMIL_M]]
                        },

                        _ => {
                            // Nasal assimilation of "m" (reversed)
                            if LetterSet::mellinam().matches(left) {
                                prefix.pop();
                                vec![vec![left], vec![Letter::TAMIL_M]]
                            } else {
                                return just_extend(prefix, suffix)
                            }
                        },
                    }
                } else if LetterSet::mellinam().matches(right) {
                    prefix.pop();
                    suffix = &suffix[1..];
                    match (left, right) {
                        // Nasal assimilation of "n" or "m" with "n"
                        (Letter::TAMIL_N | Letter::TAMIL_M, Letter::TAMIL_N) => vec![
                            vec![Letter::TAMIL_N, right],
                            vec![Letter::TAMIL_M, right],
                        ],

                        // Dropping of doubled "m"
                        (Letter::TAMIL_M, Letter::TAMIL_M) => vec![
                            vec![left, right],
                            vec![left],
                        ],

                        // Nasal assimilation of "n"
                        (_, Letter::TAMIL_N) => vec![
                            vec![left, right],
                            vec![left, left],
                        ],

                        // Nasal assimilation of "m"
                        (Letter::TAMIL_M, _) => vec![
                            vec![left, right],
                            vec![right, right],
                        ],

                        // Nasal assimilation of "m" or "n" (reversed)
                        _ => vec![
                            vec![left, right],
                            vec![left, Letter::TAMIL_N],
                            vec![Letter::TAMIL_M, right],
                        ],
                    }
                } else {
                    return just_extend(prefix, suffix)
                }
            },

            // Natural joining
            _ => return just_extend(prefix, suffix),
        };

        let mut result = Vec::new();
        for mut join in joins {
            let mut word = prefix.clone();
            word.append(&mut join);
            word.extend_from_slice(suffix);
            result.push(word);
        }

        result
    }

    pub fn is_consonant(self) -> bool {
        match self.0 {
            num::CONSONANT_START..=num::CONSONANT_END => true,
            _ => false,
        }
    }

    pub fn category(self) -> Category {
        match self.0 {
            num::VOWEL_START..=num::VOWEL_END =>
                Category::TamilVowel,

            num::AAYDHAM =>
                Category::TamilAaydham,

            num::CONSONANT_START..=num::TAMIL_CONSONANT_END =>
                Category::TamilConsonant,

            num::GRANTHA_START..=num::GRANTHA_END =>
                Category::TamilGrantha,

            num::LATIN_A..=num::LATIN_Z =>
                Category::LatinAlpha,

            ch => unreachable!("invalid character: {}", ch),
        }
    }

    pub fn range(self, to: Self) -> Result<LetterSet, (Category, Category)> {
        let a = self.category();
        let b = to.category();
        if a == b {
            Ok(LetterSet::range(self.0, to.0))
        } else {
            Err((a, b))
        }
    }
}

impl Display for Letter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ch = self.0;
        match ch {
            num::VOWEL_START..=num::VOWEL_END =>
                write!(f, "{}", TAMIL_VOWELS[ch as usize]),

            num::AAYDHAM =>
                write!(f, "{}", AAYDHAM),

            num::CONSONANT_START..=num::CONSONANT_END =>
                write!(f, "{}{}", TAMIL_CONSONANTS[(ch - num::CONSONANT_START) as usize], PULLI),

            num::LATIN_A..=num::LATIN_Z =>
                write!(f, "{}", (ch - num::LATIN_A + b'a') as char),

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

    const fn range(start: u8, end: u8) -> Self {
        if end < start {
            Self::empty()
        } else {
            Self(((1 << (end + 1)) - 1) & !((1 << start) - 1))
        }
    }

    pub const fn vowel() -> Self {
        Self::kuril()
            .union(Self::nedil())
    }

    pub const fn kuril() -> Self {
        letterset![
            SHORT_A,
            SHORT_I,
            SHORT_U,
            SHORT_E,
            SHORT_O,
        ]
    }

    pub const fn nedil() -> Self {
        letterset![
            LONG_A,
            LONG_I,
            LONG_U,
            LONG_E,
            AI,
            LONG_O,
            AU,
        ]
    }

    pub const fn consonant() -> Self {
        Self::vallinam()
            .union(Self::idaiyinam())
            .union(Self::mellinam())
            .union(Self::grantha())
    }

    pub const fn vallinam() -> Self {
        letterset![
            TAMIL_K,
            TAMIL_CH,
            TAMIL_RETRO_T,
            TAMIL_T,
            TAMIL_P,
            TAMIL_ALVEOLAR_TR,
        ]
    }

    pub const fn idaiyinam() -> Self {
        Self::glide()
            .union(Self::rhotic())
            .union(Self::lateral())
    }

    pub const fn mellinam() -> Self {
        letterset![
            TAMIL_NG,
            TAMIL_NY,
            TAMIL_RETRO_N,
            TAMIL_N,
            TAMIL_M,
            TAMIL_ALVEOLAR_N,
        ]
    }

    pub const fn grantha() -> Self {
        Self::range(num::GRANTHA_START, num::GRANTHA_END)
    }

    pub const fn latin() -> Self {
        Self::vowel()
            .union(Self::single(Letter::AAYDHAM))
            .union(Self::consonant())
            .complement()
    }

    pub const fn glide() -> Self {
        letterset![
            TAMIL_Y,
            TAMIL_V,
        ]
    }

    pub const fn rhotic() -> Self {
        letterset![
            TAMIL_R,
            TAMIL_ZH,
        ]
    }

    pub const fn lateral() -> Self {
        letterset![
            TAMIL_ALVEOLAR_L,
            TAMIL_RETRO_L,
        ]
    }

    pub const fn tamil_initial() -> Self {
        Self::vowel()
            .union(Self::glide())
            .union(letterset![
                TAMIL_K,
                TAMIL_CH,
                TAMIL_NY,
                TAMIL_T,
                TAMIL_N,
                TAMIL_P,
                TAMIL_M,
            ])
    }

    pub const fn tamil_final() -> Self {
        Self::vowel()
            .union(Self::rhotic())
            .union(Self::lateral())
            .union(letterset![
                TAMIL_RETRO_N,
                TAMIL_M,
                TAMIL_Y,
                TAMIL_ALVEOLAR_N,
            ])
    }

    pub fn matches(self, lt: Letter) -> bool {
        (self.0 & (1 << lt.0)) != 0
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
        (0..62).map(Letter).filter(move |&lt| self.matches(lt))
    }
}

impl Display for LetterSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut lts = *self;

        write!(f, "[")?;

        if (lts.0 & (1 << 63)) != 0 {
            write!(f, "!")?;
            lts = lts.complement();
        }

        for i in 0..62 {
            if lts.matches(Letter(i)) {
                write!(f, "{}", Letter(i))?;
            }
        }

        write!(f, "]")
    }
}
