use std::collections::HashMap;
use std::mem;

use crate::search::Search;
use crate::tamil::{Letter, LetterSet, Word, WordIter};

#[derive(Default, Debug)]
pub struct Joins(HashMap<(Letter, Letter), Vec<(LetterSet, LetterSet)>>);

impl Joins {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, from: (Letter, Letter), to: (LetterSet, LetterSet)) {
        debug_assert!(LetterSet::single(from.0)
            .union(LetterSet::single(from.1))
            .union(to.0)
            .union(to.1)
            .intersect(LetterSet::consonant().complement())
            .is_empty());

        if let Some(vec) = self.0.get_mut(&from) {
            if !vec.contains(&to) {
                vec.push(to);
            }
        } else {
            self.0.insert(from, vec![to]);
        }
    }

    pub fn insert_pair(&mut self, a: (Letter, Letter), b: (Letter, Letter)) {
        let a_set = (LetterSet::single(a.0), LetterSet::single(a.1));
        let b_set = (LetterSet::single(b.0), LetterSet::single(b.1));

        self.insert(a, a_set);
        self.insert(a, b_set);
        self.insert(b, a_set);
        self.insert(b, b_set);
    }

    pub fn get(&self, left: Letter, right: Letter) -> Option<&[(LetterSet, LetterSet)]> {
        self.0.get(&(left, right)).map(|vec| vec.as_ref())
    }
}

lazy_static! {
    pub static ref JOINS: Joins = {
        let mut joins = Joins::new();

        for right in letterset![TAMIL_K, TAMIL_CH, TAMIL_P].iter() {
            // (TAMIL_RETRO_L, Vallinam)
            joins.insert_pair(
                (Letter::TAMIL_RETRO_L, right),
                (Letter::TAMIL_RETRO_T, right),
            );

            let options = letterset![TAMIL_ALVEOLAR_TR, TAMIL_ALVEOLAR_N, TAMIL_ALVEOLAR_L];

            // (TAMIL_ALVEOLAR_TR, Vallinam)
            joins.insert(
                (Letter::TAMIL_ALVEOLAR_TR, right),
                (options, LetterSet::single(right)),
            );

            let options = letterset![TAMIL_ALVEOLAR_TR, TAMIL_ALVEOLAR_L];

            // (TAMIL_ALVEOLAR_L, Vallinam)
            joins.insert(
                (Letter::TAMIL_ALVEOLAR_L, right),
                (options, LetterSet::single(right)),
            );

            let options = letterset![TAMIL_ALVEOLAR_TR, TAMIL_ALVEOLAR_N];

            // (TAMIL_ALVEOLAR_N, Vallinam)
            joins.insert(
                (Letter::TAMIL_ALVEOLAR_N, right),
                (options, LetterSet::single(right)),
            );
        }

        // (TAMIL_RETRO_L, TAMIL_T) - Strong
        joins.insert_pair(
            (Letter::TAMIL_RETRO_L, Letter::TAMIL_T),
            (Letter::TAMIL_RETRO_T, Letter::TAMIL_RETRO_T),
        );

        // (TAMIL_ALVEOLAR_L, TAMIL_T) - Strong
        joins.insert_pair(
            (Letter::TAMIL_ALVEOLAR_L, Letter::TAMIL_T),
            (Letter::TAMIL_ALVEOLAR_TR, Letter::TAMIL_ALVEOLAR_TR),
        );

        // (TAMIL_RETRO_L, TAMIL_T) - Weak
        joins.insert_pair(
            (Letter::TAMIL_RETRO_L, Letter::TAMIL_T),
            (Letter::TAMIL_RETRO_N, Letter::TAMIL_RETRO_T),
        );

        // (TAMIL_ALVEOLAR_L, TAMIL_T) - Weak
        joins.insert_pair(
            (Letter::TAMIL_ALVEOLAR_L, Letter::TAMIL_T),
            (Letter::TAMIL_ALVEOLAR_N, Letter::TAMIL_ALVEOLAR_TR),
        );

        let options = letterset![TAMIL_T, TAMIL_RETRO_T];

        // (TAMIL_RETRO_N, TAMIL_T)
        for right in options.iter() {
            joins.insert(
                (Letter::TAMIL_RETRO_N, right),
                (letterset![TAMIL_RETRO_N], options),
            );
        }

        let options = letterset![TAMIL_T, TAMIL_ALVEOLAR_TR];

        // (TAMIL_ALVEOLAR_N, TAMIL_T)
        for right in options.iter() {
            joins.insert(
                (Letter::TAMIL_ALVEOLAR_N, right),
                (letterset![TAMIL_ALVEOLAR_N], options),
            );
        }

        for right in letterset![TAMIL_K, TAMIL_CH, TAMIL_T].iter() {
            let options = LetterSet::single(right.paired().unwrap())
                .union(letterset![TAMIL_M]);

            // (TAMIL_M, Vallinam)
            for left in options.iter() {
                joins.insert((left, right), (options, LetterSet::single(right)));
            }
        }

        let options = letterset![TAMIL_M, TAMIL_NY];

        // (TAMIL_M, TAMIL_NY)
        for left in options.iter() {
            joins.insert(
                (left, Letter::TAMIL_NY),
                (options, letterset![TAMIL_NY]),
            );
        }

        let options = letterset![TAMIL_M, TAMIL_N];

        // (TAMIL_M, TAMIL_N)
        for right in options.iter() {
            joins.insert(
                (Letter::TAMIL_M, right),
                (letterset![TAMIL_M], options),
            );
        }

        // (TAMIL_RETRO_L, TAMIL_N);
        joins.insert_pair(
            (Letter::TAMIL_RETRO_L, Letter::TAMIL_N),
            (Letter::TAMIL_RETRO_N, Letter::TAMIL_RETRO_N),
        );

        // (TAMIL_ALVEOLAR_L, TAMIL_N);
        joins.insert_pair(
            (Letter::TAMIL_ALVEOLAR_L, Letter::TAMIL_N),
            (Letter::TAMIL_ALVEOLAR_N, Letter::TAMIL_ALVEOLAR_N),
        );

        let options = letterset![TAMIL_RETRO_N, TAMIL_N];

        // (TAMIL_RETRO_N, TAMIL_N)
        for right in options.iter() {
            joins.insert(
                (Letter::TAMIL_RETRO_N, right),
                (letterset![TAMIL_RETRO_N], options),
            );
        }

        let options = letterset![TAMIL_ALVEOLAR_N, TAMIL_N];

        // (TAMIL_ALVEOLAR_N, TAMIL_N)
        for right in options.iter() {
            joins.insert(
                (Letter::TAMIL_ALVEOLAR_N, right),
                (letterset![TAMIL_ALVEOLAR_N], options),
            );
        }

        joins
    };

    pub static ref GRANTHA_TRANSFORM: HashMap<Letter, Letter> = {
        let pairs = &[
            (Letter::GRANTHA_J, Letter::TAMIL_CH),
            (Letter::GRANTHA_SH, Letter::TAMIL_RETRO_T),
            (Letter::GRANTHA_H, Letter::TAMIL_K),
        ];

        let mut map = HashMap::new();
        for &(a, b) in pairs {
            map.insert(a, b);
            map.insert(b, a);
        }

        map
    };
}

pub fn letter_set(mut lts: LetterSet, trans: bool) -> LetterSet {
    if trans {
        // If the letter set is negated, transliterate the non-negated version
        let complement = lts.is_complement();

        if complement {
            lts = lts.complement();
        }

        lts = transliterate_letter_set(lts);

        if complement {
            lts = lts.complement();
        }
    }

    lts
}

pub fn literal_search<S: Search>(
    mut search: S,
    word: &Word,
    expand: bool,
    trans: bool,
) -> Result<S, S::Error> {
    if !expand && !trans {
        return Ok(search.literal(word));
    }

    let mut letters = word.iter();
    while let Some(lt) = letters.next() {
        if trans {
            // Check for transliteration
            if let Some(result) = transliterate(&search, expand, &mut letters, lt)? {
                search = result;
                continue;
            }
        }

        if expand {
            // Check for intervocalic grantha transformations
            if let Some(&alt) = GRANTHA_TRANSFORM.get(&lt) {
                let prev_vowel = letters
                    .index
                    .checked_sub(2)
                    .map(|index| letters.word[index].is_vowel())
                    .unwrap_or(true);

                if prev_vowel {
                    search = search
                        .literal(word![alt])
                        .asserting_next(LetterSet::vowel())
                        .marking_expanded()
                        .joining(&search.literal(word![lt]))?;

                    continue;
                }
            }

            if let Some(next) = letters.peek() {
                // Check for consonant joining transformations
                for check in [check_join, check_initial, check_final] {
                    if check(&mut search, &mut letters, lt, next)? {
                        continue;
                    }
                }
            } else if letters.index > 3 {
                // Check for final "u"
                if let Letter::SHORT_U = lt {
                    search = search
                        .literal(word![lt])
                        .joining(&search.asserting_next(LetterSet::vowel()).marking_expanded())?;

                    continue;
                }
            }
        }

        search = search.literal(word![lt]);
    }

    Ok(search)
}

fn check_join<S: Search>(
    search: &mut S,
    letters: &mut WordIter,
    lt: Letter,
    next: Letter,
) -> Result<bool, S::Error> {
    if let Some(joins) = JOINS.get(lt, next) {
        letters.adv();

        let mut iter = joins.iter().cloned();
        let (left, right) = iter.next().unwrap();

        let base = mem::replace(search, two_letter_sets(search, left, right)?);

        for (left, right) in iter {
            search.join(&two_letter_sets(&base, left, right)?)?;
        }

        Ok(true)
    } else {
        Ok(false)
    }
}

fn check_initial<S: Search>(
    search: &mut S,
    letters: &mut WordIter,
    lt: Letter,
    next: Letter,
) -> Result<bool, S::Error> {
    if letters.index != 1 || !next.is_vowel() {
        return Ok(false);
    }

    *search = match lt {
        // Initial "t"
        Letter::TAMIL_T => search.literal(word![lt]).joining(
            &search
                .asserting_prev_matching(letterset![TAMIL_RETRO_T, TAMIL_RETRO_N])?
                .literal(word![Letter::TAMIL_RETRO_T])
                .joining(
                    &search
                        .asserting_prev_matching(letterset![TAMIL_ALVEOLAR_TR, TAMIL_ALVEOLAR_N])?
                        .literal(word![Letter::TAMIL_ALVEOLAR_TR]),
                )?
                .marking_expanded(),
        )?,

        // Initial "n"
        Letter::TAMIL_N => search.literal(word![lt]).joining(
            &search
                .asserting_prev(Letter::TAMIL_RETRO_N)
                .literal(word![Letter::TAMIL_RETRO_N])
                .joining(
                    &search
                        .asserting_prev(Letter::TAMIL_ALVEOLAR_N)
                        .literal(word![Letter::TAMIL_ALVEOLAR_N]),
                )?
                .marking_expanded(),
        )?,

        _ => return Ok(false),
    };

    Ok(true)
}

fn check_final<S: Search>(
    search: &mut S,
    letters: &mut WordIter,
    lt: Letter,
    next: Letter,
) -> Result<bool, S::Error> {
    const FINAL_TRANSFORM: LetterSet =
        letterset![TAMIL_M, TAMIL_RETRO_L, TAMIL_ALVEOLAR_L, TAMIL_ALVEOLAR_N];

    if letters.index == 1
        || letters.remaining_count() != 1
        || !lt.is_vowel()
        || !FINAL_TRANSFORM.matches(next)
    {
        return Ok(false);
    }

    letters.adv();

    // Check for final "am"
    if let (Letter::SHORT_A, Letter::TAMIL_M) = (lt, next) {
        if letters.index > 5 {
            // Allow entire "am" to be removed since the word is long
            let without_am = mem::replace(search, search.literal(word![lt, next]));
            search.join(&without_am.marking_expanded())?;

            return Ok(true);
        } else if letters.index > 2 {
            // Only allow final "m" to be dropped, and only before another word
            *search = search.literal(word![lt]);

            *search = search
                .asserting_next(LetterSet::tamil_initial())
                .marking_expanded()
                .joining(&search.literal(word![next]))?;

            return Ok(true);
        }
    }

    // All patterns will ignore this vowel
    *search = search.literal(word![lt]);

    const KCP: LetterSet = letterset![TAMIL_K, TAMIL_CH, TAMIL_P];

    *search = match next {
        // Final "m"
        Letter::TAMIL_M => search
            .matching(letterset![TAMIL_M, TAMIL_NG, TAMIL_NY, TAMIL_N])?
            .joining(
                &search
                    .matching(letterset![TAMIL_RETRO_N, TAMIL_ALVEOLAR_N])?
                    .marking_expanded(),
            )?,

        // Final retroflex "l"
        Letter::TAMIL_RETRO_L => search.literal(word![next]).joining(
            &search
                .literal(word![Letter::TAMIL_RETRO_T])
                .asserting_next(KCP.union(letterset![TAMIL_RETRO_T]))
                .joining(
                    &search
                        .literal(word![Letter::TAMIL_RETRO_N])
                        .asserting_next(KCP.union(letterset![TAMIL_RETRO_N])),
                )?
                .marking_expanded(),
        )?,

        // Final alveolar "l"
        Letter::TAMIL_ALVEOLAR_L => search.literal(word![next]).joining(
            &search
                .literal(word![Letter::TAMIL_ALVEOLAR_TR])
                .asserting_next(KCP.union(letterset![TAMIL_ALVEOLAR_TR]))
                .joining(
                    &search
                        .literal(word![Letter::TAMIL_ALVEOLAR_N])
                        .asserting_next(KCP.union(letterset![TAMIL_ALVEOLAR_N])),
                )?
                .marking_expanded(),
        )?,

        // Final alveolar "n"
        Letter::TAMIL_ALVEOLAR_N => search.literal(word![next]).joining(
            &search
                .literal(word![Letter::TAMIL_ALVEOLAR_TR])
                .asserting_next(KCP)
                .marking_expanded(),
        )?,

        _ => unreachable!(),
    };

    Ok(true)
}

fn transliterate<S: Search>(
    search: &S,
    expand: bool,
    letters: &mut WordIter,
    lt: Letter,
) -> Result<Option<S>, S::Error> {
    use Letter as L;

    let search = match lt {
        L::LATIN_A => {
            match letters.peek() {
                // Check for "aa"
                Some(L::LATIN_A) => {
                    letters.adv();
                    search.literal(word![Letter::LONG_A])
                }

                // Check for "ae"
                Some(L::LATIN_E) => {
                    letters.adv();
                    search.literal(word![Letter::LONG_E])
                }

                // Check for "ai"
                Some(L::LATIN_I) => {
                    letters.adv();
                    search.literal(word![Letter::AI])
                }

                // Check for "ay"
                Some(L::LATIN_Y) => {
                    letters.adv();

                    let mut base = search
                        .literal(word![Letter::AI])
                        .joining(&search.literal(word![Letter::SHORT_A, Letter::TAMIL_Y]))?;

                    if expand {
                        base =
                            base.joining(&search.literal(word![Letter::LONG_A, Letter::TAMIL_Y]))?;
                    }

                    base.literal(word![Letter::TAMIL_Y]).joining(&base)?
                }

                // Check for "au" or "aw"
                Some(L::LATIN_U | L::LATIN_W) => {
                    letters.adv();
                    let av = search.literal(word![Letter::SHORT_A, Letter::TAMIL_V]);
                    search
                        .literal(word![Letter::AU])
                        .joining(&av)?
                        .joining(&av.literal(word![Letter::SHORT_U]))?
                }

                _ => {
                    if expand {
                        search.matching(letterset![SHORT_A, LONG_A, AI])?
                    } else {
                        search.literal(word![Letter::SHORT_A])
                    }
                }
            }
        }

        L::LATIN_E => {
            match letters.peek() {
                // Check for "ee"
                Some(L::LATIN_E) => {
                    letters.adv();
                    search.literal(word![Letter::LONG_I])
                }

                _ => {
                    if expand {
                        search.matching(letterset![SHORT_E, LONG_E, AI])?
                    } else {
                        search.matching(letterset![SHORT_E, LONG_E])?
                    }
                }
            }
        }

        L::LATIN_O => {
            match letters.peek() {
                // Check for "oo"
                Some(L::LATIN_O) => {
                    letters.adv();
                    search.literal(word![Letter::LONG_U])
                }

                _ => {
                    if expand {
                        search.matching(letterset![SHORT_O, LONG_O, AU])?
                    } else {
                        search.matching(letterset![SHORT_O, LONG_O])?
                    }
                }
            }
        }

        L::LATIN_N => {
            let n_set = letterset![TAMIL_NG, TAMIL_NY, TAMIL_N, TAMIL_ALVEOLAR_N, TAMIL_RETRO_N];
            match letters.peek() {
                // Check for "ny"
                Some(L::LATIN_Y) => {
                    letters.adv();
                    search
                        .matching(n_set)?
                        .literal(word![Letter::TAMIL_Y])
                        .joining(&optional_double(search, Letter::TAMIL_NY)?)?
                }

                // Check for "ng"
                Some(L::LATIN_G) => {
                    letters.adv();
                    search
                        .matching(n_set)?
                        .literal(word![Letter::TAMIL_K])
                        .joining(&optional_double(search, Letter::TAMIL_NG)?)?
                }

                _ => search.matching(n_set)?,
            }
        }

        // Check for "dr"
        L::LATIN_D if letters.peek() == Some(L::LATIN_R) => {
            letters.adv();
            search
                .matching(letterset![TAMIL_T, TAMIL_RETRO_T])?
                .literal(word![Letter::TAMIL_R])
                .joining(&optional_double(search, Letter::TAMIL_ALVEOLAR_TR)?)?
        }

        // If not expanding, treat these more strictly
        L::LATIN_I if !expand => search.literal(word![Letter::SHORT_I]),
        L::LATIN_L if !expand => search.matching(letterset![TAMIL_ALVEOLAR_L, TAMIL_RETRO_L])?,
        L::LATIN_U if !expand => search.literal(word![Letter::SHORT_U]),

        _ => {
            if let Some((kind, lts)) = transliterate_letter(lt) {
                let mut search = if kind.can_double {
                    optional_double_set(search, lts)?
                } else {
                    search.matching(lts.union(LetterSet::single(lt)))?
                };

                if kind.follow_by_h {
                    if let Some(L::LATIN_H) = letters.peek() {
                        letters.adv();

                        let with_h = search.matching(letterset![TAMIL_K, GRANTHA_H, AAYDHAM])?;
                        search.join(&with_h)?;
                    }
                }

                search
            } else {
                return Ok(None);
            }
        }
    };

    Ok(Some(search))
}

fn optional_double_set<S: Search>(search: &S, lts: LetterSet) -> Result<S, S::Error> {
    lts.iter()
        .try_fold(S::empty(), |a, b| a.joining(&optional_double(search, b)?))
}

fn optional_double<S: Search>(search: &S, lt: Letter) -> Result<S, S::Error> {
    let search = search.literal(word![lt]);
    search.literal(word![lt]).joining(&search)
}

fn transliterate_letter_set(lts: LetterSet) -> LetterSet {
    lts.iter()
        .filter_map(|lt| transliterate_letter(lt).map(|(_, lts)| lts))
        .fold(lts, LetterSet::union)
}

fn two_letter_sets<S: Search>(search: &S, a: LetterSet, b: LetterSet) -> Result<S, S::Error> {
    match (a.to_single(), b.to_single()) {
        (None, None) => search.matching(a)?.matching(b),
        (Some(a), None) => search.literal(word![a]).matching(b),
        (None, Some(b)) => Ok(search.matching(a)?.literal(word![b])),
        (Some(a), Some(b)) => Ok(search.literal(word![a, b])),
    }
}

const fn transliterate_letter(lt: Letter) -> Option<(TransliterationKind, LetterSet)> {
    use Letter as L;
    use TransliterationKind as T;

    let (kind, lts) = match lt {
        L::LATIN_A => (T::NONE, letterset![SHORT_A, LONG_A, AI]),
        L::LATIN_B => (T::DOUBLE_H, letterset![TAMIL_P]),
        L::LATIN_C => (T::DOUBLE_H, letterset![TAMIL_CH]),
        L::LATIN_D => (T::DOUBLE_H, letterset![TAMIL_T, TAMIL_RETRO_T]),
        L::LATIN_E => (T::NONE, letterset![SHORT_E, LONG_E, AI]),
        L::LATIN_F => (T::DOUBLE, letterset![TAMIL_P]),
        L::LATIN_G => (T::DOUBLE_H, letterset![TAMIL_K]),
        L::LATIN_H => (T::NONE, letterset![TAMIL_K, GRANTHA_H, AAYDHAM]),
        L::LATIN_I => (T::NONE, letterset![SHORT_I, LONG_I, TAMIL_Y]),
        L::LATIN_J => (T::DOUBLE_H, letterset![TAMIL_CH, GRANTHA_J]),
        L::LATIN_K => (T::DOUBLE_H, letterset![TAMIL_K, AAYDHAM]),
        L::LATIN_L => (
            T::DOUBLE,
            letterset![TAMIL_ALVEOLAR_L, TAMIL_RETRO_L, TAMIL_ZH],
        ),
        L::LATIN_M => (T::DOUBLE, letterset![TAMIL_M]),
        L::LATIN_N => (
            T::DOUBLE,
            letterset![TAMIL_NG, TAMIL_NY, TAMIL_N, TAMIL_ALVEOLAR_N, TAMIL_RETRO_N],
        ),
        L::LATIN_O => (T::NONE, letterset![SHORT_O, LONG_O, AU]),
        L::LATIN_P => (T::DOUBLE, letterset![TAMIL_P]),
        L::LATIN_Q => (T::DOUBLE, letterset![TAMIL_K]),
        L::LATIN_R => (T::DOUBLE, letterset![TAMIL_R, TAMIL_ALVEOLAR_TR]),
        L::LATIN_S => (T::DOUBLE_H, letterset![TAMIL_CH, GRANTHA_S, GRANTHA_SH]),
        L::LATIN_T => (
            T::DOUBLE_H,
            letterset![TAMIL_T, TAMIL_ALVEOLAR_TR, TAMIL_RETRO_T],
        ),
        L::LATIN_U => (T::NONE, letterset![SHORT_U, LONG_U]),
        L::LATIN_V => (T::DOUBLE, letterset![TAMIL_V]),
        L::LATIN_W => (T::DOUBLE, letterset![TAMIL_V]),
        L::LATIN_X => (T::NONE, letterset![GRANTHA_S]),
        L::LATIN_Y => (T::DOUBLE, letterset![TAMIL_Y]),
        L::LATIN_Z => (T::ALLOW_H, letterset![TAMIL_ZH]),
        _ => return None,
    };

    Some((kind, lts))
}

#[derive(Copy, Clone, Debug)]
struct TransliterationKind {
    can_double: bool,
    follow_by_h: bool,
}

impl TransliterationKind {
    const NONE: Self = Self {
        can_double: false,
        follow_by_h: false,
    };

    const DOUBLE: Self = Self {
        can_double: true,
        follow_by_h: false,
    };

    const ALLOW_H: Self = Self {
        can_double: false,
        follow_by_h: true,
    };

    const DOUBLE_H: Self = Self {
        can_double: true,
        follow_by_h: true,
    };
}
