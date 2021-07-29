use crate::tamil::{Word, Letter, LetterSet};
use crate::search::{SearchError, Search};

type Letters<'a> = std::iter::Peekable<std::iter::Copied<std::slice::Iter<'a, Letter>>>;

pub fn letter_set(lts: LetterSet, _expand: bool, trans: bool) -> LetterSet {
    if trans {
        transliterate_letter_set(lts)
    } else {
        lts
    }
}

pub fn literal_search<S: Search>(search: &S, word: &Word, _expand: bool, trans: bool) -> Result<S, SearchError> {
    if !trans {
        return Ok(search.literal(word));
    }

    let mut search = search.clone();
    let mut letters = word.iter().copied().peekable();
    while let Some(lt) = letters.next() {
        if let Some(result) = transliterate(&search, &mut letters, lt)? {
            search = result;
            continue;
        }

        search = search.literal(&[lt]);
    }

    Ok(search)
}

fn transliterate<S: Search>(search: &S, letters: &mut Letters, lt: Letter) -> Result<Option<S>, SearchError> {
    use Letter as L;

    let search = match lt {
        L::LATIN_A => {
            match letters.peek() {
                // Check for "ai"
                Some(&L::LATIN_I) => {
                    letters.next();
                    search.literal(&[Letter::AI])
                }

                // Check for "au" or "aw"
                Some(&(L::LATIN_U | L::LATIN_W)) => {
                    letters.next();
                    let av = search.literal(&[Letter::SHORT_A, Letter::TAMIL_V]);
                    search.literal(&[Letter::AU])
                        .joining(&av)?
                        .joining(&av.literal(&[Letter::SHORT_U]))?
                }

                // Check for "aa"
                Some(&L::LATIN_A) => {
                    letters.next();
                    search.literal(&[Letter::LONG_A])
                }

                _ => search.literal(&[Letter::SHORT_A]),
            }
        }

        L::LATIN_E => {
            match letters.peek() {
                // Check for "ee"
                Some(&L::LATIN_E) => {
                    letters.next();
                    search.literal(&[Letter::LONG_I])
                }

                _ => search.matching(letterset![SHORT_E, LONG_E, AI])?,
            }
        }

        L::LATIN_O => {
            match letters.peek() {
                // Check for "oo"
                Some(&L::LATIN_O) => {
                    letters.next();
                    search.literal(&[Letter::LONG_U])
                }

                _ => search.matching(letterset![SHORT_O, LONG_O, AU])?,
            }
        }

        L::LATIN_N => {
            let n_set = letterset![TAMIL_NG, TAMIL_NY, TAMIL_N, TAMIL_ALVEOLAR_N, TAMIL_RETRO_N];
            match letters.peek() {
                // Check for "ny"
                Some(&L::LATIN_Y) => {
                    letters.next();
                    search.matching(n_set)?
                        .literal(&[Letter::TAMIL_Y])
                        .joining(&optional_double(search, Letter::TAMIL_NY)?)?
                }

                // Check for "ng"
                Some(&L::LATIN_G) => {
                    letters.next();
                    search.matching(n_set)?
                        .literal(&[Letter::TAMIL_K])
                        .joining(&optional_double(search, Letter::TAMIL_NG)?)?
                }

                _ => search.matching(n_set)?,
            }
        }

        _ => {
            if let Some((kind, lts)) = transliterate_letter(lt) {
                let mut search = if kind.can_double {
                    optional_double_set(search, lts)?
                } else {
                    search.matching(lts.union(LetterSet::single(lt)))?
                };

                if kind.follow_by_h {
                    if let Some(&L::LATIN_H) = letters.peek() {
                        letters.next();

                        let with_h = search.matching(letterset![TAMIL_K, GRANTHA_H, AAYDHAM])?;
                        search.join(&with_h)?;
                    }
                }

                search
            } else {
                return Ok(None)
            }
        }
    };

    Ok(Some(search))
}

fn optional_double_set<S: Search>(search: &S, lts: LetterSet) -> Result<S, SearchError> {
    lts.iter().try_fold(S::empty(), |a, b| a.joining(&optional_double(search, b)?))
}

fn optional_double<S: Search>(search: &S, lt: Letter) -> Result<S, SearchError> {
    let search = search.literal(&[lt]);
    search.literal(&[lt]).joining(&search)
}

fn transliterate_letter_set(lts: LetterSet) -> LetterSet {
    lts.iter()
        .filter_map(|lt| transliterate_letter(lt).map(|(_, lts)| lts))
        .fold(lts, LetterSet::union)
}

const fn transliterate_letter(lt: Letter) -> Option<(TransliterationKind, LetterSet)> {
    use Letter as L;
    use TransliterationKind as T;

    let (kind, lts) = match lt {
        L::LATIN_A => (T::NONE,     letterset![SHORT_A, LONG_A]),
        L::LATIN_B => (T::DOUBLE_H, letterset![TAMIL_P]),
        L::LATIN_C => (T::DOUBLE_H, letterset![TAMIL_CH]),
        L::LATIN_D => (T::DOUBLE_H, letterset![TAMIL_T, TAMIL_RETRO_T]),
        L::LATIN_E => (T::NONE,     letterset![SHORT_E, LONG_E, AI]),
        L::LATIN_F => (T::DOUBLE,   letterset![TAMIL_P]),
        L::LATIN_G => (T::DOUBLE_H, letterset![TAMIL_K]),
        L::LATIN_H => (T::NONE,     letterset![TAMIL_K, GRANTHA_H, AAYDHAM]),
        L::LATIN_I => (T::NONE,     letterset![SHORT_I, LONG_I, TAMIL_Y]),
        L::LATIN_J => (T::DOUBLE_H, letterset![TAMIL_CH, GRANTHA_J]),
        L::LATIN_K => (T::DOUBLE_H, letterset![TAMIL_K, AAYDHAM]),
        L::LATIN_L => (T::DOUBLE,   letterset![TAMIL_ALVEOLAR_L, TAMIL_RETRO_L, TAMIL_ZH]),
        L::LATIN_M => (T::DOUBLE,   letterset![TAMIL_M]),
        L::LATIN_N => (T::DOUBLE,   letterset![TAMIL_NG, TAMIL_NY, TAMIL_N, TAMIL_ALVEOLAR_N, TAMIL_RETRO_N]),
        L::LATIN_O => (T::NONE,     letterset![SHORT_O, LONG_O, AU]),
        L::LATIN_P => (T::DOUBLE,   letterset![TAMIL_P]),
        L::LATIN_Q => (T::DOUBLE,   letterset![TAMIL_K]),
        L::LATIN_R => (T::DOUBLE,   letterset![TAMIL_R, TAMIL_ALVEOLAR_TR]),
        L::LATIN_S => (T::DOUBLE_H, letterset![TAMIL_CH, GRANTHA_S, GRANTHA_SH, GRANTHA_SSH]),
        L::LATIN_T => (T::DOUBLE_H, letterset![TAMIL_T, TAMIL_ALVEOLAR_TR, TAMIL_RETRO_T]),
        L::LATIN_U => (T::NONE,     letterset![SHORT_U, LONG_U]),
        L::LATIN_V => (T::DOUBLE,   letterset![TAMIL_V]),
        L::LATIN_W => (T::DOUBLE,   letterset![TAMIL_V]),
        L::LATIN_X => (T::NONE,     letterset![GRANTHA_S]),
        L::LATIN_Y => (T::DOUBLE,   letterset![TAMIL_Y]),
        L::LATIN_Z => (T::ALLOW_H,  letterset![TAMIL_ZH]),
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
