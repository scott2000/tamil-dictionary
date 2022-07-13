use std::fmt::{self, Display};

use thiserror::Error;

use unicode_names2 as unicode;

use crate::search::{tree, KindSet, Search, SearchResult, Suggest, SuggestionList};
use crate::tamil::{Category, Letter, LetterSet, Word, PULLI};

mod transform;

// Queries cannot be more than 512 bytes
const MAX_LEN: usize = 512;

// There cannot be more than 10 patterns in a query
const MAX_PATTERNS: usize = 10;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Please enter a search query.")]
    EmptyQuery,
    #[error("Please enter a shorter search query.")]
    LengthExceeded,
    #[error("Search queries cannot contain only excluded words. If you are trying to search for a word which starts
             with a hypen, either remove the hyphen, or surround the entire word in parentheses.")]
    OnlyNegativeWord,
    #[error("Invalid syntax: expected pattern after negation.")]
    EmptyNegative,
    #[error("Invalid syntax: multiple words cannot appear together in double quotes.")]
    SpaceInQuotes,
    #[error("Invalid syntax: alternatives must be surrounded by parentheses.")]
    SurroundWithParens,
    #[error("Invalid syntax: expected character after backslash.")]
    TrailingBackslash,
    #[error("Invalid syntax: expected parentheses after hash symbol.")]
    InvalidExact,
    #[error("Invalid syntax: expected parentheses after percent symbol.")]
    InvalidExpand,
    #[error("Invalid syntax: expected character set for zero-width assertion.")]
    InvalidAssert,
    #[error("Invalid repetition: number must be between 0 and 255.")]
    InvalidRepetitionIndex,
    #[error("Invalid syntax: expected pulli on consonant in character set.")]
    MissingPulli,
    #[error("Invalid range: partial ranges are not allowed.")]
    PartialRange,
    #[error("Invalid syntax: expected character after '{0}' in character set")]
    EmptyUnion(char),
    #[error("Invalid range: cannot create range between {0} and {1}.")]
    InvalidRange(Category, Category),
    #[error("Invalid syntax: character not allowed in repetition: '{0}'.")]
    InvalidRepetition(char),
    #[error("Invalid repetition: lower bound ({0}) must not exceed upper bound ({1}).")]
    InvalidRepetitionRange(u8, u8),
    #[error("Invalid syntax: unknown escape sequence: '\\{0}'.")]
    InvalidEscape(char),
    #[error("Invalid syntax: character not allowed in search: '{0}'.")]
    InvalidCharacter(char),
    #[error("Invalid character: {0}.")]
    InvalidUnicodeCharacter(unicode::Name),
    #[error("Invalid whitespace: {0}. Check for invisible characters.")]
    InvalidUnicodeWhitespace(unicode::Name),
    #[error("Invalid character: {0:?}.")]
    InvalidOtherCharacter(char),
    #[error("Invalid syntax: missing {0}.")]
    Missing(SurroundKind),
    #[error("Invalid syntax: extra {0}.")]
    Extra(SurroundKind),
    #[error("Invalid syntax: expected {expected} but found '{found}'.")]
    ExpectedCharacter { expected: SurroundKind, found: char },
}

#[derive(Debug)]
pub enum SurroundKind {
    Quote,
    Bracket,
    Paren,
    Brace,
}

impl Display for SurroundKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SurroundKind::Quote => write!(f, "double quote"),
            SurroundKind::Bracket => write!(f, "closing bracket"),
            SurroundKind::Paren => write!(f, "closing parenthesis"),
            SurroundKind::Brace => write!(f, "closeing brace"),
        }
    }
}

type Chars<'a> = std::iter::Peekable<std::str::Chars<'a>>;

pub enum SearchKind {
    AsSpecified,
    DefSearch,
}

#[derive(Debug)]
pub struct Query {
    pos_word: Vec<Pattern>,
    neg_word: Vec<Pattern>,
    pos_def: Vec<Pattern>,
    neg_def: Vec<Pattern>,
    kinds: KindSet,
}

impl Query {
    pub fn parse(word: &str, def: &str, kinds: KindSet) -> Result<Query, ParseError> {
        // Make sure the query doesn't exceed the length limit
        if word.len() + def.len() > MAX_LEN {
            return Err(ParseError::LengthExceeded);
        }

        let mut query = Self {
            pos_word: Vec::new(),
            neg_word: Vec::new(),
            pos_def: Vec::new(),
            neg_def: Vec::new(),
            kinds,
        };

        Self::parse_into(word, &mut query.pos_word, &mut query.neg_word)?;
        Self::parse_into(def, &mut query.pos_def, &mut query.neg_def)?;

        let pos_len = query.pos_word.len() + query.pos_def.len();
        let neg_len = query.neg_word.len() + query.neg_def.len();

        // Check for parts which shouldn't be empty
        if pos_len == 0 && kinds.is_empty() {
            if neg_len == 0 {
                return Err(ParseError::EmptyQuery);
            } else {
                return Err(ParseError::OnlyNegativeWord);
            }
        }

        // Make sure there aren't too many patterns
        if pos_len + neg_len > MAX_PATTERNS {
            return Err(ParseError::LengthExceeded);
        }

        Ok(query)
    }

    fn parse_into(
        s: &str,
        pos: &mut Vec<Pattern>,
        neg: &mut Vec<Pattern>,
    ) -> Result<(), ParseError> {
        let mut chars = s.chars().peekable();

        // Parse all of the patterns of the query
        loop {
            // Check for negative patterns
            let mut negative = false;
            if let Some('-') = chars.peek() {
                chars.next();
                negative = true;
            }

            // Check for quoted patterns
            let mut quoted = false;
            if let Some('"') = chars.peek() {
                chars.next();
                quoted = true;
            }

            // Parse the pattern and add it to the appropriate list
            match Pattern::parse_term(&mut chars, false)? {
                Pattern::Empty => {
                    if negative {
                        return Err(ParseError::EmptyNegative);
                    }
                }
                mut pat => {
                    if quoted {
                        pat = pat.quoted();
                    }

                    if negative {
                        neg.push(pat);
                    } else {
                        pos.push(pat);
                    }
                }
            }

            // Check for closing quotes if there were opening quotes
            if quoted {
                Self::expect_quote(&mut chars)?;
            }

            // Check for invalid trailing characters
            match chars.peek() {
                None => break,
                Some(' ') => {
                    chars.next();
                }
                Some('|') => {
                    return Err(ParseError::SurroundWithParens);
                }
                Some('"') => {
                    return Err(ParseError::Extra(SurroundKind::Quote));
                }
                Some(']') => {
                    return Err(ParseError::Extra(SurroundKind::Bracket));
                }
                Some(')') => {
                    return Err(ParseError::Extra(SurroundKind::Paren));
                }
                Some('}') => {
                    return Err(ParseError::Extra(SurroundKind::Brace));
                }
                Some(&ch) => {
                    // If quoted, assume this is the start of another pattern
                    if !quoted {
                        return Err(Self::invalid_character(ch, ParseError::InvalidCharacter));
                    }
                }
            }
        }

        Ok(())
    }

    fn expect_quote(chars: &mut Chars) -> Result<(), ParseError> {
        match chars.peek() {
            Some('"') => {
                chars.next();
                Ok(())
            }

            None => Err(ParseError::Missing(SurroundKind::Quote)),
            Some(' ') => Err(ParseError::SpaceInQuotes),
            Some('|') => Err(ParseError::SurroundWithParens),
            Some(&found) => Err(Self::expected_character(SurroundKind::Quote, found)),
        }
    }

    #[cfg(debug_assertions)]
    pub fn debug(&self) -> crate::search::debug::DebugResult {
        for pat in &self.pos_word {
            eprintln!("[DEBUG] {pat:?}:");
            let _ = pat.debug(ExpandOptions::FULL);
        }

        for pat in &self.neg_word {
            eprintln!("[DEBUG] <neg> {pat:?}:");
            let _ = pat.debug(ExpandOptions::TRANS);
        }

        for pat in &self.pos_def {
            eprintln!("[DEBUG] <def> {pat:?}:");
            let _ = pat.debug(ExpandOptions::MINOR);
        }

        for pat in &self.neg_def {
            eprintln!("[DEBUG] <neg def> {pat:?}:");
            let _ = pat.debug(ExpandOptions::MINOR);
        }

        crate::search::debug::DebugResult
    }

    pub fn search(&self) -> Result<(SearchResult, SearchKind), tree::SearchError> {
        let mut intersect = Vec::new();
        let mut difference = Vec::new();

        // Search using positive word patterns
        for pat in &self.pos_word {
            intersect.push(
                pat.search(tree::search_word(), ExpandOptions::FULL)?
                    .end()?,
            );
        }

        // Search using positive definition patterns
        for pat in &self.pos_def {
            intersect.push(
                pat.search(tree::search_definition(), ExpandOptions::MINOR)?
                    .end()?,
            );
        }

        let mut process_negatives = || {
            // Search using negative word patterns
            for pat in &self.neg_word {
                difference.push(
                    pat.search(tree::search_word(), ExpandOptions::TRANS)?
                        .end()?,
                );
            }

            // Search using negative definition patterns
            for pat in &self.neg_def {
                difference.push(
                    pat.search(tree::search_definition(), ExpandOptions::MINOR)?
                        .end()?,
                );
            }

            Ok(())
        };

        // Give special error for failing negative patterns
        if let Err(tree::SearchError::TooManyResults) = process_negatives() {
            return Err(tree::SearchError::CommonExclusion);
        }

        let result = SearchResult::filter(intersect, difference, self.kinds);

        // If there are no results, try again as a definition
        if result.is_empty() && self.pos_def.is_empty() && self.neg_def.is_empty() {
            let mut intersect = Vec::new();
            let mut difference = Vec::new();

            let mut process_definitions = || -> Result<(), tree::SearchError> {
                // Search using positive word patterns as definition patterns
                for pat in &self.pos_word {
                    intersect.push(
                        pat.search(tree::search_definition(), ExpandOptions::FULL_NO_TRANS)?
                            .end()?,
                    );
                }

                // Search using negative words patterns as definition patterns
                for pat in &self.neg_word {
                    difference.push(
                        pat.search(tree::search_definition(), ExpandOptions::MINOR)?
                            .end()?,
                    );
                }

                Ok(())
            };

            // If successful, return the definition search instead
            if process_definitions().is_ok() {
                let result = SearchResult::filter(intersect, difference, self.kinds);
                return Ok((result, SearchKind::DefSearch));
            }
        }

        Ok((result, SearchKind::AsSpecified))
    }

    pub fn into_pattern(self) -> Option<Pattern> {
        if self.pos_word.len() == 1
            && self.neg_word.is_empty()
            && self.pos_def.is_empty()
            && self.neg_def.is_empty()
        {
            self.pos_word.into_iter().next()
        } else {
            None
        }
    }

    pub fn implicit_transliteration(&self) -> bool {
        if !self.pos_def.is_empty() || !self.neg_def.is_empty() {
            return false;
        }

        self.pos_word
            .iter()
            .any(|pat| pat.implicit_transliteration())
    }

    pub fn escape(s: &str) -> String {
        let mut buffer = String::new();
        let mut has_invalid = true;
        for ch in s.chars() {
            if Letter::is_valid(ch) {
                buffer.push(ch);
                has_invalid = false;
            } else if !has_invalid {
                buffer.push('-');
                has_invalid = true;
            }
        }

        if has_invalid {
            buffer.pop();
        }

        buffer
    }

    fn invalid_character(ch: char, f: fn(char) -> ParseError) -> ParseError {
        if ch.is_ascii_graphic() {
            f(ch)
        } else {
            Self::invalid_unicode(ch)
        }
    }

    fn invalid_unicode(ch: char) -> ParseError {
        if let Some(name) = unicode::name(ch) {
            if ch.is_whitespace() {
                ParseError::InvalidUnicodeWhitespace(name)
            } else {
                ParseError::InvalidUnicodeCharacter(name)
            }
        } else {
            ParseError::InvalidOtherCharacter(ch)
        }
    }

    fn expected_character(expected: SurroundKind, found: char) -> ParseError {
        if found.is_ascii_graphic() || found == ' ' {
            ParseError::ExpectedCharacter { expected, found }
        } else {
            Self::invalid_unicode(found)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
pub enum ExpandLevel {
    Exact,
    Minor,
    Full,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ExpandOptions {
    pub transliterate: bool,
    pub expand: ExpandLevel,
}

impl ExpandOptions {
    pub const EXACT: Self = Self {
        transliterate: false,
        expand: ExpandLevel::Exact,
    };

    pub const MINOR: Self = Self {
        transliterate: false,
        expand: ExpandLevel::Minor,
    };

    pub const TRANS: Self = Self {
        transliterate: true,
        expand: ExpandLevel::Minor,
    };

    pub const FULL_NO_TRANS: Self = Self {
        transliterate: false,
        expand: ExpandLevel::Full,
    };

    pub const FULL: Self = Self {
        transliterate: true,
        expand: ExpandLevel::Full,
    };
}

#[derive(Debug)]
pub enum Pattern {
    Empty,
    MarkExpanded,
    AssertStart,
    AssertMiddle,
    AssertEnd,
    AssertNext(LetterSet),
    AssertPrev(LetterSet),
    Set(LetterSet),
    Literal(Box<Word>),
    Expand(Box<Pattern>, ExpandOptions),
    Repeat(Box<Pattern>, u8, u8),
    Concat(Box<Pattern>, Box<Pattern>),
    Alternative(Box<Pattern>, Box<Pattern>),
}

impl Pattern {
    pub fn parse(s: &str) -> Result<Self, ParseError> {
        if s.len() > MAX_LEN {
            return Err(ParseError::LengthExceeded);
        }

        let mut chars = s.chars().peekable();
        let pat = Self::parse_term(&mut chars, true)?;
        match chars.next() {
            None => Ok(pat),
            Some('|') => Err(ParseError::SurroundWithParens),
            Some('"') => Err(ParseError::Extra(SurroundKind::Quote)),
            Some(']') => Err(ParseError::Extra(SurroundKind::Bracket)),
            Some(')') => Err(ParseError::Extra(SurroundKind::Paren)),
            Some('}') => Err(ParseError::Extra(SurroundKind::Brace)),
            Some(ch) => Err(Query::invalid_character(ch, ParseError::InvalidCharacter)),
        }
    }

    pub fn search<S: Search>(&self, search: S, opts: ExpandOptions) -> Result<S, S::Error> {
        if search.is_empty() {
            return Ok(search);
        }

        match self {
            Self::Empty => Ok(search),
            Self::MarkExpanded => Ok(search.marking_expanded()),
            Self::AssertStart => Ok(search.asserting_start()),
            Self::AssertMiddle => Ok(search.asserting_middle()),
            Self::AssertEnd => Ok(search.asserting_end()),
            &Self::AssertNext(lts) => Ok(search.asserting_next(transform::letter_set(lts, opts))),
            &Self::AssertPrev(lts) => {
                Ok(search.asserting_prev_matching(transform::letter_set(lts, opts))?)
            }
            &Self::Set(lts) => search.matching(transform::letter_set(lts, opts)),
            Self::Literal(word) => transform::literal_search(search, word, opts),
            &Self::Expand(ref pat, opts) => pat.search(search, opts),
            &Self::Repeat(ref pat, a, b) => {
                let mut search = search;
                for _ in 0..a {
                    search = pat.search(search, opts)?;
                }

                let mut result = search.clone();
                for _ in 0..(b - a) {
                    if search.is_empty() {
                        break;
                    }

                    search = pat.search(search, opts)?;
                    result.join(&search)?;
                }

                Ok(result)
            }
            Self::Concat(a, b) => {
                let search = a.search(search, opts)?;
                if search.is_empty() {
                    Ok(search)
                } else {
                    b.search(search, opts)
                }
            }
            Self::Alternative(a, b) => a
                .search(search.clone(), opts)?
                .joining(b.search(search, opts)?),
        }
    }

    pub fn suggest(&self, count: u32) -> Option<SuggestionList> {
        if let Ok(search) = self.search(tree::search_word_prefix(), ExpandOptions::FULL) {
            Some(search.suggest(count))
        } else {
            None
        }
    }

    pub fn implicit_transliteration(&self) -> bool {
        match self {
            &Self::AssertNext(mut lts) | &Self::AssertPrev(mut lts) | &Self::Set(mut lts) => {
                if lts.is_complement() {
                    lts = lts.complement();
                }

                !lts.intersect(LetterSet::latin()).is_empty()
            }

            Self::Literal(word) => word.contains(LetterSet::latin()),

            Self::Repeat(pat, _, _) => pat.implicit_transliteration(),

            Self::Concat(a, b) | Self::Alternative(a, b) => {
                a.implicit_transliteration() || b.implicit_transliteration()
            }

            _ => false,
        }
    }

    pub fn quoted(self) -> Self {
        // Replace "X" with #(<X>)
        Pattern::Expand(
            Box::new(Pattern::Concat(
                Box::new(Pattern::AssertStart),
                Box::new(Pattern::Concat(
                    Box::new(self),
                    Box::new(Pattern::AssertEnd),
                )),
            )),
            ExpandOptions::EXACT,
        )
    }

    pub fn last(&self) -> &Self {
        let mut pat = self;

        while let Self::Concat(_, new_pat) = pat {
            pat = new_pat;
        }

        pat
    }

    pub fn last_mut(&mut self) -> &mut Self {
        let mut pat = self;

        while let Self::Concat(_, new_pat) = pat {
            pat = new_pat;
        }

        pat
    }

    #[cfg(debug_assertions)]
    pub fn debug(&self, opts: ExpandOptions) -> crate::search::debug::DebugResult {
        use crate::search::debug::DebugSearch;

        let search = DebugSearch::start();
        self.search(search, opts)
            .unwrap_or_else(|e| match e {})
            .end()
            .unwrap_or_else(|e| match e {})
    }

    fn parse_group(chars: &mut Chars) -> Result<Self, ParseError> {
        let pat = Self::parse_term(chars, true)?;

        Self::skip_spaces(chars, true);

        if let Some('|') = chars.peek() {
            chars.next();
            let next = Self::parse_group(chars)?;
            Ok(Self::Alternative(Box::new(pat), Box::new(next)))
        } else {
            Ok(pat)
        }
    }

    fn parse_term(chars: &mut Chars, in_group: bool) -> Result<Self, ParseError> {
        Self::skip_spaces(chars, in_group);

        // Parse a single term of the pattern
        let mut split_text = false;
        let pat = match chars.peek() {
            None => Self::Empty,
            Some('#') => Self::parse_exact(chars)?,
            Some('%') => Self::parse_expand(chars)?,
            Some('&') => Self::parse_assert(chars)?,
            Some('(') => Self::parse_paren(chars)?,
            Some('[') => Self::Set(Self::parse_bracket(chars)?),
            Some('\\') => {
                chars.next();
                Self::Set(Self::parse_escape(chars)?)
            }
            Some('@') => {
                chars.next();
                Self::MarkExpanded
            }
            Some('<' | '^') => {
                chars.next();
                Self::AssertStart
            }
            Some('~') => {
                chars.next();
                Self::AssertMiddle
            }
            Some('>' | '$') => {
                chars.next();
                Self::AssertEnd
            }
            Some('_') => {
                chars.next();
                Self::Set(LetterSet::any())
            }
            Some(_) => {
                split_text = true;
                Self::parse_text(chars, in_group)
            }
        };

        if let Self::Empty = pat {
            return Ok(pat);
        }

        // Parse a repetition after the term of the pattern
        let pat = match Self::parse_repeat(chars)? {
            (0, 0) => return Ok(Self::Empty),
            (1, 1) => pat,
            (a, b) => {
                match pat {
                    Self::Literal(text) if split_text && text.len() > 1 => {
                        // Only apply the repetition to the last character in a literal
                        let split = text.len() - 1;
                        Self::Concat(
                            Box::new(Self::Literal(Box::from(&text[..split]))),
                            Box::new(Self::Repeat(
                                Box::from(Self::Literal(Box::from(&text[split..]))),
                                a,
                                b,
                            )),
                        )
                    }
                    _ => Self::Repeat(Box::new(pat), a, b),
                }
            }
        };

        // Continue parsing if possible, and concatenate the result
        let next = Self::parse_term(chars, in_group)?;
        if let Self::Empty = next {
            Ok(pat)
        } else {
            Ok(Self::Concat(Box::new(pat), Box::new(next)))
        }
    }

    fn parse_escape(chars: &mut Chars) -> Result<LetterSet, ParseError> {
        if let Some(ch) = chars.next() {
            if let Some(lts) = LetterSet::parse_escape(ch) {
                Ok(lts)
            } else {
                Err(Query::invalid_character(ch, ParseError::InvalidEscape))
            }
        } else {
            Err(ParseError::TrailingBackslash)
        }
    }

    fn parse_exact(chars: &mut Chars) -> Result<Self, ParseError> {
        chars.next();

        if let Some('(') = chars.peek() {
            Ok(Pattern::Expand(
                Box::new(Self::parse_paren(chars)?),
                ExpandOptions::EXACT,
            ))
        } else {
            Err(ParseError::InvalidExact)
        }
    }

    fn parse_expand(chars: &mut Chars) -> Result<Self, ParseError> {
        chars.next();

        if let Some('(') = chars.peek() {
            Ok(Pattern::Expand(
                Box::new(Self::parse_paren(chars)?),
                ExpandOptions::FULL,
            ))
        } else {
            Err(ParseError::InvalidExpand)
        }
    }

    fn parse_assert(chars: &mut Chars) -> Result<Self, ParseError> {
        chars.next();

        let mut assert: fn(LetterSet) -> Self = Pattern::AssertNext;
        if chars.peek() == Some(&'<') {
            chars.next();
            assert = Pattern::AssertPrev;
        }

        match chars.peek() {
            Some('[') => Ok(assert(Self::parse_bracket(chars)?)),
            Some('\\') => {
                chars.next();
                Ok(assert(Self::parse_escape(chars)?))
            }
            _ => Err(ParseError::InvalidAssert),
        }
    }

    fn parse_paren(chars: &mut Chars) -> Result<Self, ParseError> {
        chars.next();

        let pat = Self::parse_group(chars)?;

        Self::skip_spaces(chars, true);

        match chars.peek() {
            None => Err(ParseError::Missing(SurroundKind::Paren)),
            Some(')') => {
                chars.next();
                Ok(pat)
            }
            Some(&found) => Err(Query::expected_character(SurroundKind::Paren, found)),
        }
    }

    fn parse_bracket(chars: &mut Chars) -> Result<LetterSet, ParseError> {
        chars.next();

        let mut allowed = LetterSet::any();

        let mut last_char = '[';
        let mut negated = false;

        if let Some('^') = chars.peek() {
            chars.next();
            last_char = '^';
            negated = true;
        }

        loop {
            let mut set = Self::parse_union(chars)?;
            if set.is_empty() {
                return Err(ParseError::EmptyUnion(last_char));
            }

            if negated {
                set = set.complement();
            }

            allowed = allowed.intersect(set);

            match chars.next() {
                None => {
                    return Err(ParseError::Missing(SurroundKind::Bracket));
                }
                Some(']') => {
                    return Ok(allowed);
                }
                Some('&') => {
                    last_char = '&';
                    negated = false;
                }
                Some('^') => {
                    last_char = '^';
                    negated = true;
                }
                Some(ch) => {
                    return Err(Query::expected_character(SurroundKind::Bracket, ch));
                }
            }
        }
    }

    fn parse_union(chars: &mut Chars) -> Result<LetterSet, ParseError> {
        let mut set = LetterSet::empty();

        loop {
            match chars.peek() {
                None | Some(']' | '&' | '^') => {
                    return Ok(set);
                }
                Some('-') => {
                    return Err(ParseError::PartialRange);
                }
                Some('\\') => {
                    chars.next();
                    set = set.union(Self::parse_escape(chars)?);
                }
                Some(' ') => {
                    chars.next();
                }
                Some(&ch) => {
                    chars.next();
                    set = set.union(Self::parse_range(chars, ch)?);
                }
            }
        }
    }

    fn parse_range(chars: &mut Chars, ch: char) -> Result<LetterSet, ParseError> {
        if let Some(start) = Letter::parse(ch) {
            Self::parse_pulli(chars, start)?;

            if let Some('-') = chars.peek() {
                chars.next();

                if let Some(ch) = chars.next() {
                    if let Some(end) = Letter::parse(ch) {
                        Self::parse_pulli(chars, end)?;

                        start
                            .range(end)
                            .map_err(|(a, b)| ParseError::InvalidRange(a, b))
                    } else {
                        Err(ParseError::PartialRange)
                    }
                } else {
                    Err(ParseError::PartialRange)
                }
            } else {
                Ok(letterset![start])
            }
        } else {
            Err(Query::expected_character(SurroundKind::Bracket, ch))
        }
    }

    fn parse_pulli(chars: &mut Chars, lt: Letter) -> Result<(), ParseError> {
        if lt.is_consonant() {
            match chars.next() {
                None => {
                    return Err(ParseError::Missing(SurroundKind::Bracket));
                }
                Some(PULLI) => {}
                Some(_) => {
                    return Err(ParseError::MissingPulli);
                }
            }
        }

        Ok(())
    }

    fn parse_text(chars: &mut Chars, in_group: bool) -> Self {
        let mut buffer = String::new();
        while let Some(&ch) = chars.peek() {
            if !Letter::is_valid(ch) {
                break;
            }

            chars.next();
            buffer.push(ch);

            Self::skip_spaces(chars, in_group);
        }

        if buffer.is_empty() {
            Self::Empty
        } else {
            Self::Literal(Word::parse(&buffer))
        }
    }

    fn parse_repeat(chars: &mut Chars) -> Result<(u8, u8), ParseError> {
        match chars.peek() {
            Some('?') => {
                chars.next();
                Ok((0, 1))
            }
            Some('*') => {
                chars.next();
                Ok((0, u8::MAX))
            }
            Some('+') => {
                chars.next();
                Ok((1, u8::MAX))
            }
            Some('{') => {
                chars.next();

                let a = Self::parse_bound(chars, 0)?;
                match chars.next() {
                    None => Err(ParseError::Missing(SurroundKind::Brace)),
                    Some('}') => Ok((a, a)),
                    Some(':' | ',') => {
                        let b = Self::parse_bound(chars, u8::MAX)?;

                        if a > b {
                            return Err(ParseError::InvalidRepetitionRange(a, b));
                        }

                        match chars.next() {
                            None => Err(ParseError::Missing(SurroundKind::Brace)),
                            Some('}') => Ok((a, b)),
                            Some(found) => {
                                Err(Query::expected_character(SurroundKind::Brace, found))
                            }
                        }
                    }
                    Some(ch) => Err(Query::invalid_character(ch, ParseError::InvalidRepetition)),
                }
            }
            _ => Ok((1, 1)),
        }
    }

    fn parse_bound(chars: &mut Chars, default: u8) -> Result<u8, ParseError> {
        let mut buffer = String::new();
        while let Some(&ch) = chars.peek() {
            if !ch.is_ascii_digit() {
                break;
            }

            chars.next();
            buffer.push(ch);
        }

        if buffer.is_empty() {
            Ok(default)
        } else {
            buffer
                .parse()
                .map_err(|_| ParseError::InvalidRepetitionIndex)
        }
    }

    fn skip_spaces(chars: &mut Chars, in_group: bool) {
        while let Some(&ch) = chars.peek() {
            if ch == '-' || (in_group && ch == ' ') {
                chars.next();
            } else {
                break;
            }
        }
    }
}
