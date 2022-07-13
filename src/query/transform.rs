use std::mem;

use crate::search::Search;
use crate::tamil::{Letter, LetterSet, Word, WordIter};
use crate::HashMap;

#[derive(Default, Debug)]
pub struct Joins(HashMap<(Letter, Letter), Vec<(LetterSet, LetterSet)>>);

impl Joins {
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
        let mut joins = Joins::default();

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

        // (RetroL, M);
        joins.insert_pair(
            (Letter::RetroL, Letter::M),
            (Letter::RetroN, Letter::M),
        );

        // (AlveolarL, M);
        joins.insert_pair(
            (Letter::AlveolarL, Letter::M),
            (Letter::AlveolarN, Letter::M),
        );

        joins
    };
}

#[inline]
pub fn grantha_transform(lt: Letter) -> Option<(Letter, bool)> {
    use Letter::*;

    match lt {
        J => Some((Ch, true)),
        Ch => Some((J, true)),
        Sh => Some((RetroT, false)),
        RetroT => Some((Sh, false)),
        H => Some((K, false)),
        K => Some((H, false)),
        _ => None,
    }
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
            if let Some((alt, allow_start)) = grantha_transform(lt) {
                let prev_vowel = letters.prev().map(Letter::is_vowel).unwrap_or(allow_start);

                if prev_vowel {
                    search = search
                        .literal(word![alt])
                        .asserting_next(LetterSet::vowel())
                        .marking_expanded()
                        .joining(search.literal(word![lt]))?;

                    continue;
                }
            }

            if let Some(next) = letters.peek() {
                // Check for various common suffixes to make optional
                if letters.index > 1 && check_suffix(&mut search, &mut letters)? {
                    break;
                }

                // Check for consonant joining transformations
                for check in [
                    check_join,
                    check_initial,
                    check_expanded_diphthongs,
                    check_final,
                ] {
                    if check(&mut search, &mut letters, lt, next)? {
                        continue 'outer;
                    }
                }
            } else if letters.index > 3 && lt == Letter::U {
                // Check for final "u"
                search = search
                    .asserting_prev_matching(LetterSet::tamil_final())?
                    .asserting_end()
                    .joining(search.asserting_next(LetterSet::vowel().difference(letterset![U])))?
                    .marking_expanded()
                    .joining(search.literal(word![U]))?;

                continue;
            }

            // Check for following consonant and either A or Ai
            let a_transform = || {
                letters.peek_matches(LetterSet::consonant())
                    && letters.peek_over_matches(letterset![A, Ai])
            };

            // Check for variations of vowels and "n"
            match lt {
                // Check for "a" (/ai/ -> [a])
                Letter::A => {
                    search = search
                        .literal(word![Ai])
                        .marking_expanded()
                        .joining(search.literal(word![A]))?;

                    continue;
                }

                // Check for "u" (/i/ -> [u] / #p_R)
                Letter::U if !a_transform() => {
                    search = search
                        .asserting_prev_matching(letterset![P, M])?
                        .literal(word![I])
                        .asserting_next(LetterSet::retroflex())
                        .marking_expanded()
                        .joining(search.literal(word![U]))?;

                    continue;
                }

                // Check for "e" (/i/ -> [e] / #_Ca)
                Letter::E if a_transform() => {
                    search = search
                        .literal(word![I])
                        .marking_expanded()
                        .joining(search.literal(word![E]))?;

                    continue;
                }

                // Check for "ai"
                Letter::Ai => {
                    search = search
                        .literal(word![A, Y])
                        .marking_expanded()
                        .joining(search.literal(word![Ai]))?;

                    continue;
                }

                Letter::O => {
                    if a_transform() {
                        // Check for "o" (/u/ -> [o] / #_Ca, /i/ -> [o] / #p_Ra)
                        search = search
                            .asserting_prev_matching(letterset![P, M])?
                            .literal(word![I])
                            .asserting_next(LetterSet::retroflex())
                            .joining(search.literal(word![U]))?
                            .marking_expanded()
                            .joining(search.literal(word![O]))?;
                    } else {
                        // Check for "o" (/e/ -> [o] / #p_R)
                        search = search
                            .asserting_prev_matching(letterset![P, M])?
                            .literal(word![E])
                            .asserting_next(LetterSet::retroflex())
                            .marking_expanded()
                            .joining(search.literal(word![O]))?;
                    }

                    continue;
                }

                // Check for "au"
                Letter::Au => {
                    search = search
                        .literal(word![A, V, U])
                        .marking_expanded()
                        .joining(search.literal(word![Au]))?;

                    continue;
                }

                // Check for dental "n"
                Letter::N => {
                    search = search
                        .literal(word![AlveolarN])
                        .asserting_next(LetterSet::vowel())
                        .marking_expanded()
                        .joining(search.literal(word![N]))?;

                    continue;
                }

                // Check for alveolar "n"
                Letter::AlveolarN => {
                    search = search
                        .literal(word![N])
                        .asserting_next(LetterSet::vowel())
                        .marking_expanded()
                        .joining(search.literal(word![AlveolarN]))?;

                    continue;
                }

                // Check for retroflex "l" (/z/ -> [l])
                Letter::RetroL => {
                    search = search
                        .literal(word![Zh])
                        .marking_expanded()
                        .joining(search.literal(word![RetroL]))?;

                    continue;
                }

                _ => {}
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
        // Check for double letter followed by U
        &[_, lt, next, Letter::U] if next == lt && LetterSet::tamil_final().matches(lt) => {
            let with_single = search.literal(word![lt]);
            let with_double = with_single.literal(word![lt]);

            *search = with_double
                .asserting_next(LetterSet::vowel().difference(letterset![U]))
                .joining(with_single.asserting_end())?
                .marking_expanded()
                .joining(with_double.literal(word![U]))?;

            return Ok(true);
        }

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
                                .joining(search.literal(word![M]))?;
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
                        .joining(search.literal(word![RetroL]))?;
                }

                // Handle final alveolar "l"
                Letter::AlveolarL | Letter::AlveolarR => {
                    letters.adv();
                    *search = search
                        .literal(word![AlveolarR])
                        .asserting_next(KCP)
                        .joining(search.literal(word![AlveolarL]))?;
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
    search.join(&without_suffix.freezing())?;

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

        let mut iter = joins.iter().copied();
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
            search
                .asserting_prev_matching(letterset![RetroT, RetroN])?
                .literal(word![RetroT])
                .joining(
                    search
                        .asserting_prev_matching(letterset![AlveolarR, AlveolarN])?
                        .literal(word![AlveolarR]),
                )?
                .marking_expanded(),
        )?,

        // Initial "n"
        Letter::N => search.literal(word![lt]).joining(
            search
                .asserting_prev(Letter::RetroN)
                .literal(word![RetroN])
                .joining(
                    search
                        .asserting_prev(Letter::AlveolarN)
                        .literal(word![AlveolarN]),
                )?
                .marking_expanded(),
        )?,

        _ => return Ok(false),
    };

    Ok(true)
}

fn check_expanded_diphthongs<S: Search>(
    search: &mut S,
    letters: &mut WordIter,
    lt: Letter,
    next: Letter,
) -> Result<bool, S::Error> {
    *search = match (lt, next, letters.peek_over()) {
        // Check for "ay"
        (Letter::A, Letter::Y, _) => search
            .literal(word![Ai])
            .marking_expanded()
            .joining(search.literal(word![A, Y]))?,

        // Check for "avu"
        (Letter::A, Letter::V, Some(Letter::U)) => {
            letters.adv();

            search
                .literal(word![Au])
                .marking_expanded()
                .joining(search.literal(word![A, V, U]))?
        }

        // Check for "avu"
        (Letter::A, Letter::V, Some(Letter::V)) => search
            .literal(word![Au])
            .marking_expanded()
            .joining(search.literal(word![A, V]))?,

        _ => return Ok(false),
    };

    letters.adv();

    Ok(true)
}

fn check_final<S: Search>(
    search: &mut S,
    letters: &mut WordIter,
    lt: Letter,
    next: Letter,
) -> Result<bool, S::Error> {
    const FINAL_TRANSFORM: LetterSet = letterset![M, RetroL, AlveolarL, AlveolarN, R];

    if letters.remaining_count() != 1 || !lt.is_vowel() || !FINAL_TRANSFORM.matches(next) {
        return Ok(false);
    }

    letters.adv();
    let with_vowel = search.literal(word![lt]);

    *search = match next {
        // Final "m"
        Letter::M => {
            let final_m = with_vowel
                .matching(letterset![Ng, Ny, N])?
                .marking_expanded()
                .joining(with_vowel.literal(word![M]))?;

            // Final "am"
            if lt == Letter::A {
                if letters.index > 4 {
                    // Allow entire "am" to be removed since the word is long
                    search
                        .asserting_next(LetterSet::vowel())
                        .freezing()
                        .joining(final_m)?
                } else if letters.index > 2 {
                    // Only allow final "m" to be dropped, and only before another word
                    with_vowel
                        .asserting_next(LetterSet::tamil_initial())
                        .freezing()
                        .joining(final_m)?
                } else {
                    final_m
                }
            } else {
                final_m
            }
        }

        // Final retroflex "l"
        Letter::RetroL => with_vowel.literal(word![next]).joining(
            with_vowel
                .literal(word![RetroT])
                .asserting_next(KCP.union(letterset![RetroT]))
                .joining(
                    with_vowel
                        .literal(word![RetroN])
                        .asserting_next(KCP.union(letterset![RetroN, M])),
                )?
                .marking_expanded(),
        )?,

        // Final alveolar "l"
        Letter::AlveolarL => with_vowel.literal(word![next]).joining(
            with_vowel
                .literal(word![AlveolarR])
                .asserting_next(KCP.union(letterset![AlveolarR]))
                .joining(
                    with_vowel
                        .literal(word![AlveolarN])
                        .asserting_next(KCP.union(letterset![AlveolarN, M])),
                )?
                .marking_expanded(),
        )?,

        // Final alveolar "n"
        Letter::AlveolarN => {
            let with_kcp = with_vowel.literal(word![next]).joining(
                with_vowel
                    .literal(word![AlveolarR])
                    .asserting_next(KCP)
                    .marking_expanded(),
            )?;

            // Final "an"
            if lt == Letter::A {
                with_kcp.joining(
                    with_vowel
                        .literal(word![R])
                        .asserting_end()
                        .marking_expanded(),
                )?
            } else {
                with_kcp
            }
        }

        // Final "ar"
        Letter::R if lt == Letter::A => with_vowel.literal(word![next]).joining(
            with_vowel
                .literal(word![AlveolarN])
                .asserting_end()
                .marking_expanded(),
        )?,

        _ => with_vowel.literal(word![next]),
    };

    Ok(true)
}

fn transliterate<S: Search>(
    search: &S,
    expand: bool,
    letters: &mut WordIter,
    lt: Letter,
) -> Result<Option<S>, S::Error> {
    use Letter::*;

    let search = match lt {
        LatinA => {
            match letters.peek() {
                // Check for "aa"
                Some(LatinA) => {
                    letters.adv();
                    search.literal(word![LongA])
                }

                // Check for "ae"
                Some(LatinE) => {
                    letters.adv();
                    search.literal(word![LongE])
                }

                // Check for "ai"
                Some(LatinI) => {
                    letters.adv();
                    search.literal(word![Ai])
                }

                // Check for "ay"
                Some(LatinY) => {
                    letters.adv();

                    let mut base = search
                        .literal(word![Ai])
                        .joining(search.literal(word![A, Y]))?;

                    if expand {
                        base = base.joining(search.literal(word![LongA, Y]))?;
                    }

                    base.literal(word![Y]).joining(base)?
                }

                // Check for "au"
                Some(LatinU) => {
                    letters.adv();
                    let av = search.literal(word![A, V]);
                    let search = search.literal(word![Au]).joining(av.literal(word![U]))?;

                    if expand {
                        search.joining(av.marking_expanded())?
                    } else {
                        search
                    }
                }

                // Check for "aw"
                Some(LatinW) => {
                    letters.adv();
                    let av = search.literal(word![A, V]);
                    search
                        .literal(word![Au])
                        .joining(av.literal(word![U]))?
                        .joining(av)?
                }

                _ => {
                    if expand {
                        search
                            .literal(word![A])
                            .joining(search.matching(letterset![LongA, Ai])?)?
                    } else {
                        search.literal(word![A])
                    }
                }
            }
        }

        LatinE => {
            match letters.peek() {
                // Check for "ee"
                Some(LatinE) => {
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

        LatinO => {
            match letters.peek() {
                // Check for "oo"
                Some(LatinO) => {
                    letters.adv();
                    search.literal(word![LongU])
                }

                _ => {
                    if expand {
                        search
                            .matching(letterset![O, LongO])?
                            .joining(search.literal(word![Au]))?
                    } else {
                        search.matching(letterset![O, LongO])?
                    }
                }
            }
        }

        // Check for "ng"
        LatinN if letters.peek() == Some(LatinG) => {
            letters.adv();
            search
                .matching(letterset![Ng, N, RetroN, AlveolarN])?
                .literal(word![K])
                .joining(optional_double(search, false, false, Letter::Ng)?)?
        }

        // Check for "ny"
        LatinN if letters.peek() == Some(LatinY) => {
            letters.adv();
            search
                .matching(letterset![N, RetroN, AlveolarN])?
                .literal(word![Y])
                .joining(optional_double(search, false, false, Letter::Ny)?)?
        }

        // Check for "nj"
        LatinN if letters.peek() == Some(LatinJ) => {
            letters.adv();
            search.literal(word![Ny, Ch])
        }

        // Check for "dr"
        LatinD if letters.peek() == Some(LatinR) => {
            letters.adv();
            search
                .matching(letterset![T, RetroT])?
                .literal(word![R])
                .joining(search.literal(word![AlveolarR]))?
        }

        // If not expanding, treat these more strictly
        LatinI if !expand => search.literal(word![I]),
        LatinL if !expand => search.matching(letterset![AlveolarL, RetroL])?,
        LatinU if !expand => search.literal(word![U]),

        _ => {
            if let Some(tr) = Transliteration::get(lt) {
                let mut double_kind = tr.double_kind;

                // Record whether at the start of the word or not before advancing
                let start_of_word = letters.index == 1;

                // Decide which letters are likely and unlikely
                let (mut likely, mut unlikely) = if tr.with_h.is_empty() {
                    tr.without_h()
                } else if let Some(LatinH) = letters.peek() {
                    letters.adv();
                    tr.with_h()
                } else {
                    tr.without_h()
                };

                if start_of_word {
                    // Also allow any grantha letters as initial
                    const INITIAL: LetterSet =
                        LetterSet::tamil_initial().union(LetterSet::grantha());

                    // If at the start of the word, allow unlikely valid initial characters
                    if INITIAL.intersect(likely).is_empty() {
                        likely = INITIAL.intersect(unlikely).union(likely);
                    }

                    // If at the start of the word, allow single letters always
                    double_kind.allow_single();
                }

                if expand {
                    // Remove any overlap between the likely and unlikely characters
                    unlikely = likely.complement().intersect(unlikely);
                } else {
                    // Ignore unlikely characters since expansions are disabled
                    unlikely = LetterSet::empty();
                }

                let mut result = optional_double_set(search, double_kind, likely)?;

                // Only join the unlikely set if it is non-empty
                if !unlikely.is_empty() {
                    result.join(
                        &optional_double_set(search, double_kind, unlikely)?.marking_expanded(),
                    )?;
                }

                result
            } else {
                return Ok(None);
            }
        }
    };

    Ok(Some(search))
}

fn optional_double_set<S: Search>(
    search: &S,
    kind: DoubleKind,
    lts: LetterSet,
) -> Result<S, S::Error> {
    let (avoid_single, avoid_double) = match kind {
        DoubleKind::Never => return matching(search, lts),
        DoubleKind::Avoid => (false, true),
        DoubleKind::Allow => (false, false),
        DoubleKind::Force => (true, false),
    };

    lts.iter().try_fold(search.clearing(), |a, b| {
        a.joining(optional_double(search, avoid_single, avoid_double, b)?)
    })
}

fn optional_double<S: Search>(
    search: &S,
    avoid_single: bool,
    avoid_double: bool,
    lt: Letter,
) -> Result<S, S::Error> {
    debug_assert!(!avoid_single || !avoid_double);

    let with_single = search.literal(word![lt]);
    let kcp_lt = KCP.union(letterset![lt]);

    if avoid_single {
        let tr_lt = letterset![RetroT, AlveolarR, J, Sh, S, lt];

        // Check for end of word
        let end_of_word = with_single.asserting_end();

        // Check for following hard consonant
        let double_next = with_single.asserting_next(kcp_lt);

        // Check for previous hard consonant and following not hard consonant
        let double_prev_only = search
            .asserting_prev_matching(tr_lt)?
            .literal(word![lt])
            .asserting_next(kcp_lt.complement());

        // Check for no hard consonant on either side
        let no_double = search
            .asserting_prev_matching(tr_lt.complement())?
            .literal(word![lt])
            .asserting_next(kcp_lt.complement())
            .marking_expanded();

        with_single
            .literal(word![lt])
            .joining(end_of_word)?
            .joining(double_next)?
            .joining(double_prev_only)?
            .joining(no_double)
    } else if !avoid_double {
        // Since there is no preference, the context doesn't matter
        with_single.literal(word![lt]).joining(with_single)
    } else if LetterSet::vallinam().matches(lt) {
        // Check for end of word
        let end_of_word = with_single.asserting_end().marking_expanded();

        // Check for no following hard consonant
        let vowel_next = with_single.asserting_next(kcp_lt.complement());

        // Check for following hard consonant
        let non_vowel_next = with_single.asserting_next(kcp_lt).marking_expanded();

        with_single
            .literal(word![lt])
            .marking_expanded()
            .joining(end_of_word)?
            .joining(vowel_next)?
            .joining(non_vowel_next)
    } else {
        // Since the letter isn't a hard consonant, the context doesn't matter
        with_single
            .literal(word![lt])
            .marking_expanded()
            .joining(with_single)
    }
}

fn transliterate_letter_set(lts: LetterSet) -> LetterSet {
    lts.iter()
        .filter_map(|lt| Transliteration::get(lt).map(|tr| tr.all()))
        .fold(lts, LetterSet::union)
}

fn two_letter_sets<S: Search>(search: &S, a: LetterSet, b: LetterSet) -> Result<S, S::Error> {
    matching(&matching(search, a)?, b)
}

fn matching<S: Search>(search: &S, lts: LetterSet) -> Result<S, S::Error> {
    if let Some(lt) = lts.to_single() {
        Ok(search.literal(word![lt]))
    } else {
        search.matching(lts)
    }
}

#[derive(Copy, Clone, Debug)]
struct Transliteration {
    double_kind: DoubleKind,
    without_h: LetterSet,
    unlikely: LetterSet,
    with_h: LetterSet,
}

impl Transliteration {
    #[rustfmt::skip]
    const fn get(lt: Letter) -> Option<Self> {
        use Letter::*;
        use DoubleKind::*;

        let (double_kind, without_h, unlikely, with_h) = match lt {
            LatinA => (Never, letterset![A],                    letterset![LongA, Ai], letterset![]),
            LatinB => (Avoid, letterset![P],                    letterset![],          letterset![P]),
            LatinC => (Force, letterset![Ch],                   letterset![],          letterset![Ch]),
            LatinD => (Avoid, letterset![RetroT, T],            letterset![],          letterset![T]),
            LatinE => (Never, letterset![E, LongE, Ai],         letterset![],          letterset![]),
            LatinF => (Allow, letterset![P],                    letterset![],          letterset![]),
            LatinG => (Avoid, letterset![K],                    letterset![],          letterset![K]),
            LatinH => (Avoid, letterset![K, H, Aaydham],        letterset![],          letterset![]),
            LatinI => (Never, letterset![I],                    letterset![LongI, Y],  letterset![]),
            LatinJ => (Avoid, letterset![J],                    letterset![Ch],        letterset![J]),
            LatinK => (Force, letterset![K],                    letterset![Aaydham],   letterset![K]),
            LatinL => (Avoid, letterset![AlveolarL, RetroL],    letterset![Zh],        letterset![]),
            LatinM => (Avoid, letterset![M],                    letterset![],          letterset![]),
            LatinN => (Avoid, letterset![N, AlveolarN, RetroN], letterset![Ng, Ny],    letterset![]),
            LatinO => (Never, letterset![O, LongO, Au],         letterset![],          letterset![]),
            LatinP => (Force, letterset![P],                    letterset![],          letterset![P]),
            LatinQ => (Never, letterset![],                     letterset![],          letterset![]),
            LatinR => (Avoid, letterset![R, AlveolarR],         letterset![],          letterset![]),
            LatinS => (Avoid, letterset![Ch, S],                letterset![],          letterset![Sh]),
            LatinT => (Force, letterset![RetroT, AlveolarR, T], letterset![],          letterset![T]),
            LatinU => (Never, letterset![U],                    letterset![LongU],     letterset![]),
            LatinV => (Avoid, letterset![V],                    letterset![],          letterset![]),
            LatinW => (Avoid, letterset![V],                    letterset![],          letterset![]),
            LatinX => (Never, letterset![Aaydham],              letterset![],          letterset![]),
            LatinY => (Avoid, letterset![Y],                    letterset![],          letterset![]),
            LatinZ => (Never, letterset![Zh],                   letterset![J],         letterset![Zh]),
            _ => return None,
        };

        Some(Self {
            double_kind,
            without_h,
            unlikely,
            with_h,
        })
    }

    const fn all(&self) -> LetterSet {
        self.without_h.union(self.unlikely).union(self.with_h)
    }

    const fn without_h(&self) -> (LetterSet, LetterSet) {
        (self.without_h, self.unlikely.union(self.with_h))
    }

    const fn with_h(&self) -> (LetterSet, LetterSet) {
        (self.with_h, self.unlikely.union(self.without_h))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum DoubleKind {
    Never,
    Avoid,
    Allow,
    Force,
}

impl DoubleKind {
    fn allow_single(&mut self) {
        if *self == Self::Force {
            *self = Self::Allow;
        }
    }
}
