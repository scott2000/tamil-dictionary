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
        debug_assert!(letterset![from.0, from.1]
            .union(to.0.union(to.1))
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
        let a_set = (letterset![a.0], letterset![a.1]);
        let b_set = (letterset![b.0], letterset![b.1]);

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

        for right in letterset![K, Ch, P].iter() {
            // (RetroL, Vallinam)
            joins.insert_pair(
                (Letter::RetroL, right),
                (Letter::RetroT, right),
            );

            let options = letterset![AlveolarR, AlveolarN, AlveolarL];

            // (AlveolarR, Vallinam)
            joins.insert(
                (Letter::AlveolarR, right),
                (options, letterset![right]),
            );

            let options = letterset![AlveolarR, AlveolarL];

            // (AlveolarL, Vallinam)
            joins.insert(
                (Letter::AlveolarL, right),
                (options, letterset![right]),
            );

            let options = letterset![AlveolarR, AlveolarN];

            // (AlveolarN, Vallinam)
            joins.insert(
                (Letter::AlveolarN, right),
                (options, letterset![right]),
            );
        }

        // (RetroL, T) - Strong
        joins.insert_pair(
            (Letter::RetroL, Letter::T),
            (Letter::RetroT, Letter::RetroT),
        );

        // (AlveolarL, T) - Strong
        joins.insert_pair(
            (Letter::AlveolarL, Letter::T),
            (Letter::AlveolarR, Letter::AlveolarR),
        );

        // (RetroL, T) - Weak
        joins.insert_pair(
            (Letter::RetroL, Letter::T),
            (Letter::RetroN, Letter::RetroT),
        );

        // (AlveolarL, T) - Weak
        joins.insert_pair(
            (Letter::AlveolarL, Letter::T),
            (Letter::AlveolarN, Letter::AlveolarR),
        );

        let options = letterset![T, RetroT];

        // (RetroN, T)
        for right in options.iter() {
            joins.insert(
                (Letter::RetroN, right),
                (letterset![RetroN], options),
            );
        }

        let options = letterset![T, AlveolarR];

        // (AlveolarN, T)
        for right in options.iter() {
            joins.insert(
                (Letter::AlveolarN, right),
                (letterset![AlveolarN], options),
            );
        }

        for right in letterset![K, Ch, T].iter() {
            let options = letterset![M, right.paired().unwrap()];

            // (M, Vallinam)
            for left in options.iter() {
                joins.insert((left, right), (options, letterset![right]));
            }
        }

        let options = letterset![M, Ny];

        // (M, Ny)
        for left in options.iter() {
            joins.insert(
                (left, Letter::Ny),
                (options, letterset![Ny]),
            );
        }

        let options = letterset![M, N];

        // (M, N)
        for right in options.iter() {
            joins.insert(
                (Letter::M, right),
                (letterset![M], options),
            );
        }

        // (RetroL, N);
        joins.insert_pair(
            (Letter::RetroL, Letter::N),
            (Letter::RetroN, Letter::RetroN),
        );

        // (AlveolarL, N);
        joins.insert_pair(
            (Letter::AlveolarL, Letter::N),
            (Letter::AlveolarN, Letter::AlveolarN),
        );

        let options = letterset![RetroN, N];

        // (RetroN, N)
        for right in options.iter() {
            joins.insert(
                (Letter::RetroN, right),
                (letterset![RetroN], options),
            );
        }

        let options = letterset![AlveolarN, N];

        // (AlveolarN, N)
        for right in options.iter() {
            joins.insert(
                (Letter::AlveolarN, right),
                (letterset![AlveolarN], options),
            );
        }

        joins
    };

    pub static ref GRANTHA_TRANSFORM: HashMap<Letter, Letter> = {
        let pairs = &[
            (Letter::J, Letter::Ch),
            (Letter::Sh, Letter::RetroT),
            (Letter::H, Letter::K),
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
    'outer: while let Some(lt) = letters.next() {
        // Exit early if the search is empty
        if search.is_empty() {
            break;
        }

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
                let prev_vowel = letters.prev().map(Letter::is_vowel).unwrap_or(true);

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
                // Check for various common suffixes to make optional
                if letters.index > 1 && check_suffix(&mut search, &mut letters)? {
                    break;
                }

                // Check for consonant joining transformations
                for check in [check_join, check_initial, check_final] {
                    if check(&mut search, &mut letters, lt, next)? {
                        continue 'outer;
                    }
                }
            } else if letters.index > 3 {
                // Check for final "u"
                if let Letter::U = lt {
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

const KCP: LetterSet = letterset![K, Ch, P];

#[rustfmt::skip]
fn check_suffix<S: Search>(search: &mut S, letters: &mut WordIter) -> Result<bool, S::Error> {
    match letters.remaining_with_offset(-2).as_ref() {
        // Check for "aaga" and "aana"
        [_, Letter::LongA, Letter::K | Letter::AlveolarN, Letter::A]
            if letters.index > 2 => {}

        // Check for "aaga" and "aana"  with "y" joiner
        &[lt, Letter::Y, Letter::LongA, Letter::K | Letter::AlveolarN, Letter::A]
            if LetterSet::vowel_with_y().matches(lt) => {}

        // Check for "aaga" and "aana"  with "v" joiner
        &[lt, Letter::V, Letter::LongA, Letter::K | Letter::AlveolarN, Letter::A]
            if LetterSet::vowel_with_v().matches(lt) => {}

        // Check for "endru" and "endra"
        &[
            lt,
            Letter::E,
            Letter::AlveolarN,
            Letter::AlveolarR,
            Letter::U | Letter::A,
        ] if LetterSet::tamil_final().matches(lt) => {}

        // Check for "endru" and "endra" with "y" joiner
        &[
            lt,
            Letter::Y,
            Letter::E,
            Letter::AlveolarN,
            Letter::AlveolarR,
            Letter::U | Letter::A,
        ] if LetterSet::vowel_with_y().matches(lt) => {}

        // Check for "endru" and "endra" with "v" joiner
        &[
            lt,
            Letter::V,
            Letter::E,
            Letter::AlveolarN,
            Letter::AlveolarR,
            Letter::U | Letter::A,
        ] if LetterSet::vowel_with_v().matches(lt) => {}

        // Check for "padu", "paadu", or "paduthu"
        &[prev, lt, Letter::P, Letter::A | Letter::LongA, Letter::RetroT, Letter::U]
        | &[
            prev,
            lt,
            Letter::P,
            Letter::A,
            Letter::RetroT,
            Letter::U,
            Letter::T,
            Letter::T,
            Letter::U,
        ] => {
            match lt {
                // Handle doubling of "p"
                Letter::P => {
                    if letters.index > 3 {
                        // Handle final "am"
                        if prev == Letter::A {
                            letters.adv();
                            *search = search
                                .literal(word![P])
                                .asserting_next(KCP)
                                .joining(&search.literal(word![M]))?;
                        }
                    } else {
                        return Ok(false);
                    }
                }

                // Handle final retroflex "l"
                Letter::RetroL | Letter::RetroT => {
                    letters.adv();
                    *search = search
                        .literal(word![RetroT])
                        .asserting_next(KCP)
                        .joining(&search.literal(word![RetroL]))?;
                }

                // Handle final alveolar "l"
                Letter::AlveolarL | Letter::AlveolarR => {
                    letters.adv();
                    *search = search
                        .literal(word![AlveolarR])
                        .asserting_next(KCP)
                        .joining(&search.literal(word![AlveolarL]))?;
                }

                _ => {
                    if letters.index > 2 && LetterSet::tamil_final().matches(lt) {
                        letters.adv();
                        *search = search.literal(word![lt]);
                    } else {
                        return Ok(false);
                    }
                }
            }
        }

        // Check for "thal"
        &[lt, Letter::T, Letter::A, Letter::AlveolarL]
        | &[lt, Letter::T, Letter::T, Letter::A, Letter::AlveolarL]
        | &[lt, Letter::U, Letter::T, Letter::A, Letter::AlveolarL]
        | &[lt, Letter::U, Letter::T, Letter::T, Letter::A, Letter::AlveolarL]
            if letters.index > 3 && LetterSet::tamil_final().matches(lt) => {}

        _ => return Ok(false),
    }

    let without_suffix = mem::replace(search, search.literal(letters.remaining_with_offset(-1)));
    search.join(&without_suffix.marking_expanded().freezing())?;

    Ok(true)
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
        Letter::T => search.literal(word![lt]).joining(
            &search
                .asserting_prev_matching(letterset![RetroT, RetroN])?
                .literal(word![RetroT])
                .joining(
                    &search
                        .asserting_prev_matching(letterset![AlveolarR, AlveolarN])?
                        .literal(word![AlveolarR]),
                )?
                .marking_expanded(),
        )?,

        // Initial "n"
        Letter::N => search.literal(word![lt]).joining(
            &search
                .asserting_prev(Letter::RetroN)
                .literal(word![RetroN])
                .joining(
                    &search
                        .asserting_prev(Letter::AlveolarN)
                        .literal(word![AlveolarN]),
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
    const FINAL_TRANSFORM: LetterSet = letterset![M, RetroL, AlveolarL, AlveolarN];

    if letters.remaining_count() != 1 || !lt.is_vowel() || !FINAL_TRANSFORM.matches(next) {
        return Ok(false);
    }

    letters.adv();
    let with_vowel = search.literal(word![lt]);
    let final_m = || -> Result<S, S::Error> {
        Ok(with_vowel
            .matching(letterset![Ng, Ny, N])?
            .marking_expanded()
            .joining(&with_vowel.literal(word![M]))?)
    };

    // Check for final "am"
    if let (Letter::A, Letter::M) = (lt, next) {
        if letters.index > 5 {
            // Allow entire "am" to be removed since the word is long
            *search = search
                .asserting_next(LetterSet::vowel())
                .marking_expanded()
                .freezing()
                .joining(&final_m()?)?;

            return Ok(true);
        } else if letters.index > 2 {
            // Only allow final "m" to be dropped, and only before another word
            *search = with_vowel
                .asserting_next(LetterSet::tamil_initial())
                .marking_expanded()
                .freezing()
                .joining(&final_m()?)?;

            return Ok(true);
        }
    }

    *search = match next {
        // Final "m"
        Letter::M => final_m()?,

        // Final retroflex "l"
        Letter::RetroL => with_vowel.literal(word![next]).joining(
            &with_vowel
                .literal(word![RetroT])
                .asserting_next(KCP.union(letterset![RetroT]))
                .joining(
                    &with_vowel
                        .literal(word![RetroN])
                        .asserting_next(KCP.union(letterset![RetroN])),
                )?
                .marking_expanded(),
        )?,

        // Final alveolar "l"
        Letter::AlveolarL => with_vowel.literal(word![next]).joining(
            &with_vowel
                .literal(word![AlveolarR])
                .asserting_next(KCP.union(letterset![AlveolarR]))
                .joining(
                    &with_vowel
                        .literal(word![AlveolarN])
                        .asserting_next(KCP.union(letterset![AlveolarN])),
                )?
                .marking_expanded(),
        )?,

        // Final alveolar "n"
        Letter::AlveolarN => with_vowel.literal(word![next]).joining(
            &with_vowel
                .literal(word![AlveolarR])
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
        L::LatinA => {
            match letters.peek() {
                // Check for "aa"
                Some(L::LatinA) => {
                    letters.adv();
                    search.literal(word![LongA])
                }

                // Check for "ae"
                Some(L::LatinE) => {
                    letters.adv();
                    search.literal(word![LongE])
                }

                // Check for "ai"
                Some(L::LatinI) => {
                    letters.adv();
                    search.literal(word![Ai])
                }

                // Check for "ay"
                Some(L::LatinY) => {
                    letters.adv();

                    let mut base = search
                        .literal(word![Ai])
                        .joining(&search.literal(word![A, Y]))?;

                    if expand {
                        base = base.joining(&search.literal(word![LongA, Y]))?;
                    }

                    base.literal(word![Y]).joining(&base)?
                }

                // Check for "au" or "aw"
                Some(L::LatinU | L::LatinW) => {
                    letters.adv();
                    let av = search.literal(word![A, V]);
                    search
                        .literal(word![Au])
                        .joining(&av)?
                        .joining(&av.literal(word![U]))?
                }

                _ => {
                    if expand {
                        search.matching(letterset![A, LongA, Ai])?
                    } else {
                        search.literal(word![A])
                    }
                }
            }
        }

        L::LatinE => {
            match letters.peek() {
                // Check for "ee"
                Some(L::LatinE) => {
                    letters.adv();
                    search.literal(word![LongI])
                }

                _ => {
                    if expand {
                        search.matching(letterset![E, LongE, Ai])?
                    } else {
                        search.matching(letterset![E, LongE])?
                    }
                }
            }
        }

        L::LatinO => {
            match letters.peek() {
                // Check for "oo"
                Some(L::LatinO) => {
                    letters.adv();
                    search.literal(word![LongU])
                }

                _ => {
                    if expand {
                        search.matching(letterset![O, LongO, Au])?
                    } else {
                        search.matching(letterset![O, LongO])?
                    }
                }
            }
        }

        L::LatinN => {
            let n_set = letterset![Ng, Ny, N, AlveolarN, RetroN];
            match letters.peek() {
                // Check for "ny"
                Some(L::LatinY) => {
                    letters.adv();
                    search
                        .matching(n_set)?
                        .literal(word![Y])
                        .joining(&optional_double(search, Letter::Ny)?)?
                }

                // Check for "ng"
                Some(L::LatinG) => {
                    letters.adv();
                    search
                        .matching(n_set)?
                        .literal(word![K])
                        .joining(&optional_double(search, Letter::Ng)?)?
                }

                _ => search.matching(n_set)?,
            }
        }

        // Check for "dr"
        L::LatinD if letters.peek() == Some(L::LatinR) => {
            letters.adv();
            search
                .matching(letterset![T, RetroT])?
                .literal(word![R])
                .joining(&optional_double(search, Letter::AlveolarR)?)?
        }

        // If not expanding, treat these more strictly
        L::LatinI if !expand => search.literal(word![I]),
        L::LatinL if !expand => search.matching(letterset![AlveolarL, RetroL])?,
        L::LatinU if !expand => search.literal(word![U]),

        _ => {
            if let Some((kind, lts)) = transliterate_letter(lt) {
                let mut search = if kind.can_double {
                    optional_double_set(search, lts)?
                } else {
                    search.matching(lts.union(letterset![lt]))?
                };

                if kind.follow_by_h {
                    if let Some(L::LatinH) = letters.peek() {
                        letters.adv();

                        let with_h = search.matching(letterset![K, H, Aaydham])?;
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
        L::LatinA => (T::NONE, letterset![A, LongA, Ai]),
        L::LatinB => (T::DOUBLE_H, letterset![P]),
        L::LatinC => (T::DOUBLE_H, letterset![Ch]),
        L::LatinD => (T::DOUBLE_H, letterset![T, RetroT]),
        L::LatinE => (T::NONE, letterset![E, LongE, Ai]),
        L::LatinF => (T::DOUBLE, letterset![P]),
        L::LatinG => (T::DOUBLE_H, letterset![K]),
        L::LatinH => (T::NONE, letterset![K, H, Aaydham]),
        L::LatinI => (T::NONE, letterset![I, LongI, Y]),
        L::LatinJ => (T::DOUBLE_H, letterset![Ch, J]),
        L::LatinK => (T::DOUBLE_H, letterset![K, Aaydham]),
        L::LatinL => (T::DOUBLE, letterset![AlveolarL, RetroL, Zh]),
        L::LatinM => (T::DOUBLE, letterset![M]),
        L::LatinN => (T::DOUBLE, letterset![Ng, Ny, N, AlveolarN, RetroN]),
        L::LatinO => (T::NONE, letterset![O, LongO, Au]),
        L::LatinP => (T::DOUBLE, letterset![P]),
        L::LatinQ => (T::DOUBLE, letterset![K]),
        L::LatinR => (T::DOUBLE, letterset![R, AlveolarR]),
        L::LatinS => (T::DOUBLE_H, letterset![Ch, S, Sh]),
        L::LatinT => (T::DOUBLE_H, letterset![T, AlveolarR, RetroT]),
        L::LatinU => (T::NONE, letterset![U, LongU]),
        L::LatinV => (T::DOUBLE, letterset![V]),
        L::LatinW => (T::DOUBLE, letterset![V]),
        L::LatinX => (T::NONE, letterset![S]),
        L::LatinY => (T::DOUBLE, letterset![Y]),
        L::LatinZ => (T::ALLOW_H, letterset![Zh]),
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
