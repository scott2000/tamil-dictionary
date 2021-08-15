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

    pub static ref GRANTHA_TRANSFORM: HashMap<Letter, (Letter, bool)> = {
        let pairs = &[
            (Letter::J, Letter::Ch, true),
            (Letter::Sh, Letter::RetroT, false),
            (Letter::H, Letter::K, false),
        ];

        let mut map = HashMap::new();
        for &(a, b, allow_start) in pairs {
            map.insert(a, (b, allow_start));
            map.insert(b, (a, allow_start));
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
            if let Some(&(alt, allow_start)) = GRANTHA_TRANSFORM.get(&lt) {
                let prev_vowel = letters.prev().map(Letter::is_vowel).unwrap_or(allow_start);

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
                        .joining(&search.literal(word![A, Y]))?;

                    if expand {
                        base = base.joining(&search.literal(word![LongA, Y]))?;
                    }

                    base.literal(word![Y]).joining(&base)?
                }

                // Check for "au" or "aw"
                Some(LatinU | LatinW) => {
                    letters.adv();
                    let av = search.literal(word![A, V]);
                    search
                        .literal(word![Au])
                        .joining(&av)?
                        .joining(&av.literal(word![U]))?
                }

                _ => {
                    if expand {
                        search
                            .literal(word![A])
                            .joining(&search.matching(letterset![LongA, Ai])?)?
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
                            .joining(&search.literal(word![Au]))?
                    } else {
                        search.matching(letterset![O, LongO])?
                    }
                }
            }
        }

        // Check for "ny"
        LatinN if letters.peek() == Some(LatinY) => {
            letters.adv();
            search
                .matching(letterset![N, RetroN, AlveolarN])?
                .literal(word![Y])
                .joining(&optional_double(search, false, Letter::Ny)?)?
        }

        // Check for "ng"
        LatinN if letters.peek() == Some(LatinG) => {
            letters.adv();
            search
                .matching(letterset![Ng, N, RetroN, AlveolarN])?
                .literal(word![K])
                .joining(&optional_double(search, false, Letter::Ng)?)?
        }

        // Check for "dr"
        LatinD if letters.peek() == Some(LatinR) => {
            letters.adv();
            search
                .matching(letterset![T, RetroT])?
                .literal(word![R])
                .marking_expanded()
                .joining(&search.literal(word![AlveolarR]))?
        }

        // If not expanding, treat these more strictly
        LatinI if !expand => search.literal(word![I]),
        LatinL if !expand => search.matching(letterset![AlveolarL, RetroL])?,
        LatinU if !expand => search.literal(word![U]),

        _ => {
            if let Some(tr) = Transliteration::get(lt) {
                // Decide which letters are likely and unlikely
                let (mut likely, mut unlikely) = if tr.with_h.is_empty() {
                    tr.without_h()
                } else if let Some(LatinH) = letters.peek() {
                    letters.adv();
                    tr.with_h()
                } else {
                    tr.without_h()
                };

                // If at the start of the word, allow unlikely valid initial characters
                if letters.index == 1 && LetterSet::tamil_initial().intersect(likely).is_empty() {
                    likely = LetterSet::tamil_initial().intersect(unlikely).union(likely);
                }

                // Remove any overlap between the likely and unlikely characters
                unlikely = likely.complement().intersect(unlikely);

                optional_double_set(search, tr.double_kind, likely)?.joining(
                    &optional_double_set(search, tr.double_kind, unlikely)?.marking_expanded(),
                )?
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
    let avoid = match kind {
        DoubleKind::NeverDouble => return matching(search, lts),
        DoubleKind::AvoidDouble => true,
        DoubleKind::AllowDouble => false,
    };

    lts.iter().try_fold(S::empty(), |a, b| {
        a.joining(&optional_double(search, avoid, b)?)
    })
}

fn optional_double<S: Search>(search: &S, avoid: bool, lt: Letter) -> Result<S, S::Error> {
    let search = search.literal(word![lt]);
    if avoid {
        search
            .literal(word![lt])
            .marking_expanded()
            .joining(&search)
    } else {
        search.literal(word![lt]).joining(&search)
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
            LatinA => (NeverDouble, letterset![A],                    letterset![LongA, Ai], letterset![]),
            LatinB => (AvoidDouble, letterset![P],                    letterset![],          letterset![P]),
            LatinC => (AllowDouble, letterset![Ch],                   letterset![],          letterset![Ch]),
            LatinD => (AvoidDouble, letterset![RetroT],               letterset![],          letterset![T]),
            LatinE => (NeverDouble, letterset![E, LongE, Ai],         letterset![],          letterset![]),
            LatinF => (AllowDouble, letterset![P],                    letterset![],          letterset![]),
            LatinG => (AvoidDouble, letterset![K],                    letterset![],          letterset![K]),
            LatinH => (NeverDouble, letterset![K, H, Aaydham],        letterset![],          letterset![]),
            LatinI => (NeverDouble, letterset![I],                    letterset![LongI, Y],  letterset![]),
            LatinJ => (AvoidDouble, letterset![Ch, J],                letterset![],          letterset![Ch, J]),
            LatinK => (AllowDouble, letterset![K],                    letterset![Aaydham],   letterset![K]),
            LatinL => (AllowDouble, letterset![AlveolarL, RetroL],    letterset![Zh],        letterset![]),
            LatinM => (AllowDouble, letterset![M],                    letterset![],          letterset![]),
            LatinN => (AllowDouble, letterset![N, AlveolarN, RetroN], letterset![Ng, Ny],    letterset![]),
            LatinO => (NeverDouble, letterset![O, LongO, Au],         letterset![],          letterset![]),
            LatinP => (AllowDouble, letterset![P],                    letterset![],          letterset![P]),
            LatinQ => (AllowDouble, letterset![K],                    letterset![],          letterset![]),
            LatinR => (AvoidDouble, letterset![R, AlveolarR],         letterset![],          letterset![]),
            LatinS => (AvoidDouble, letterset![Ch, S],                letterset![],          letterset![Sh]),
            LatinT => (AllowDouble, letterset![RetroT, AlveolarR],    letterset![],          letterset![T]),
            LatinU => (NeverDouble, letterset![U],                    letterset![LongU],     letterset![]),
            LatinV => (AllowDouble, letterset![V],                    letterset![],          letterset![]),
            LatinW => (AllowDouble, letterset![V],                    letterset![],          letterset![]),
            LatinX => (NeverDouble, letterset![S],                    letterset![],          letterset![]),
            LatinY => (AllowDouble, letterset![Y],                    letterset![],          letterset![]),
            LatinZ => (NeverDouble, letterset![Zh],                   letterset![],          letterset![Zh]),
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
    NeverDouble,
    AvoidDouble,
    AllowDouble,
}
