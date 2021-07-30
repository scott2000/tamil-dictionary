use std::fmt::{self, Display};

use thiserror::Error;

use unicode_names2 as unicode;

use crate::tamil::{PULLI, Word, Letter, LetterSet, Category};
use crate::search::{SearchError, Search, SearchResult, tree};

mod transform;

const MAX_LEN: usize = 512;

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
    InvalidTransliteration,
    #[error("Invalid syntax: expected brackets after ampersand for assertion.")]
    InvalidAssert,
    #[error("Invalid repetition: number must be between 0 and 255.")]
    InvalidRepetitionIndex,
    #[error("Invalid syntax: expected pulli on consonant in character set.")]
    MissingPulli,
    #[error("Invalid range: partial ranges are not allowed.")]
    PartialRange,
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
    ExpectedCharacter {
        expected: SurroundKind,
        found: char,
    },
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

#[derive(Debug)]
pub struct Query {
    word_patterns: Vec<Pattern>,
    definition_patterns: Vec<Pattern>,
    negative_word_patterns: Vec<Pattern>,
    negative_definition_patterns: Vec<Pattern>,
}

impl Query {
    pub fn parse(s: &str) -> Result<Query, ParseError> {
        // Make sure the query doesn't exceed the length limit
        if s.len() > MAX_LEN {
            return Err(ParseError::LengthExceeded);
        }

        let mut chars = s.chars().peekable();
        let mut query = Self {
            word_patterns: Vec::new(),
            definition_patterns: Vec::new(),
            negative_word_patterns: Vec::new(),
            negative_definition_patterns: Vec::new(),
        };

        // Default to parsing word patterns
        let mut positives = &mut query.word_patterns;
        let mut negatives = &mut query.negative_word_patterns;

        // Parse all of the patterns of the query
        let mut positive_empty = true;
        let mut negative_empty = true;
        loop {
            // Check for negative patterns
            let mut negative = false;
            if let Some('!' | '-') = chars.peek() {
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
            match Pattern::parse(&mut chars, false)? {
                Pattern::Empty => {
                    if negative {
                        return Err(ParseError::EmptyNegative);
                    }
                },
                mut pat => {
                    if quoted {
                        // Replace "X" with #(<X>)
                        pat = Pattern::Exact(Box::new(Pattern::Concat(
                            Box::new(Pattern::AssertStart),
                            Box::new(Pattern::Concat(Box::new(pat), Box::new(Pattern::AssertEnd))))));
                    }

                    if negative {
                        negative_empty = false;
                        negatives.push(pat);
                    } else {
                        positive_empty = false;
                        positives.push(pat);
                    }
                },
            }

            // Check for closing quotes if there were opening quotes
            if quoted {
                match chars.peek() {
                    None => {
                        return Err(ParseError::Missing(SurroundKind::Quote));
                    }
                    Some('"') => {
                        chars.next();
                    }
                    Some(' ') => {
                        return Err(ParseError::SpaceInQuotes);
                    }
                    Some('|') => {
                        return Err(ParseError::SurroundWithParens);
                    }
                    Some(&found) => {
                        return Err(Self::expected_character(SurroundKind::Quote, found));
                    }
                }
            }

            // Check for invalid trailing characters
            match chars.peek() {
                None => break,
                Some(' ') => {
                    chars.next();
                }
                Some(':') => {
                    // Switch to parsing definition patterns
                    chars.next();
                    positives = &mut query.definition_patterns;
                    negatives = &mut query.negative_definition_patterns;
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

        match (positive_empty, negative_empty) {
            (true, true) => {
                return Err(ParseError::EmptyQuery);
            }
            (true, false) => {
                // Don't allow negative word patterns by themselves
                if query.negative_definition_patterns.is_empty() {
                    return Err(ParseError::OnlyNegativeWord);
                }

                // If only negative, push a pattern which will match anything
                query.word_patterns.push(Pattern::Concat(
                        Box::new(Pattern::AssertStart),
                        Box::new(Pattern::Set(LetterSet::any()))));
            }
            _ => {}
        }

        Ok(query)
    }

    pub fn search(self) -> Result<SearchResult, SearchError> {
        let mut intersect = Vec::new();
        let mut difference = Vec::new();

        // Search using positive word patterns
        for pat in &self.word_patterns {
            intersect.push(pat.search(&tree::search_word(), true, true)?.end()?);
        }

        // Search using positive definition patterns
        for pat in &self.definition_patterns {
            intersect.push(pat.search(&tree::search_definition(), true, false)?.end()?);
        }

        let mut process_negatives = || {
            // Search using negative word patterns
            for pat in &self.negative_word_patterns {
                difference.push(pat.search(&tree::search_word(), false, true)?.end()?);
            }

            // Search using negative definition patterns
            for pat in &self.negative_definition_patterns {
                difference.push(pat.search(&tree::search_definition(), false, false)?.end()?);
            }

            Ok(())
        };

        // Give special error for failing negative patterns
        match process_negatives() {
            Err(SearchError::TooManyResults) => {
                return Err(SearchError::CommonExclusion);
            }
            _ => {}
        }

        let result = SearchResult::intersect_difference(intersect, difference);

        // If there are no results, try again as a definition
        if result.is_empty()
            && self.definition_patterns.is_empty()
            && self.negative_definition_patterns.is_empty()
        {
            let mut intersect = Vec::new();
            let mut difference = Vec::new();

            let mut process_definitions = || -> Result<(), SearchError> {
                // Search using positive word patterns as definition patterns
                for pat in &self.word_patterns {
                    intersect.push(pat.search(&tree::search_definition(), true, false)?.end()?);
                }

                // Search using negative words patterns as definition patterns
                for pat in &self.negative_word_patterns {
                    difference.push(pat.search(&tree::search_definition(), false, false)?.end()?);
                }

                Ok(())
            };

            // If successful, return the definition search instead
            if let Ok(_) = process_definitions() {
                return Ok(SearchResult::intersect_difference(intersect, difference));
            }
        }

        Ok(result)
    }

    pub fn escape(s: &str) -> String {
        let mut buffer = String::new();
        let mut has_space = true;
        for ch in s.chars() {
            if Letter::is_valid(ch) || ch == '-' {
                buffer.push(ch);
                has_space = false;
            } else if !has_space {
                buffer.push(' ');
                has_space = true;
            }
        }

        if has_space {
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
            ParseError::ExpectedCharacter {
                expected,
                found,
            }
        } else {
            Self::invalid_unicode(found)
        }
    }
}

#[derive(Debug)]
pub enum Pattern {
    Empty,
    AssertStart,
    AssertMiddle,
    AssertEnd,
    Assert(LetterSet),
    Set(LetterSet),
    Literal(Box<Word>),
    Exact(Box<Pattern>),
    Trans(Box<Pattern>),
    Repeat(Box<Pattern>, u8, u8),
    Concat(Box<Pattern>, Box<Pattern>),
    Alternative(Box<Pattern>, Box<Pattern>),
}

impl Pattern {
    pub fn search<S: Search>(&self, search: &S, expand: bool, trans: bool) -> Result<S, S::Error> {
        match self {
            Self::Empty => Ok(search.clone()),
            Self::AssertStart => Ok(search.asserting_start()),
            Self::AssertMiddle => Ok(search.asserting_middle()),
            Self::AssertEnd => Ok(search.asserting_end()),
            &Self::Assert(lts) => Ok(search.asserting(lts)),
            &Self::Set(lts) => search.matching(transform::letter_set(lts, expand, trans)),
            Self::Literal(word) => transform::literal_search(search, word, expand, trans),
            Self::Exact(pat) => pat.search(search, false, false),
            Self::Trans(pat) => pat.search(search, expand, true),
            &Self::Repeat(ref pat, a, b) => {
                let mut search = search.clone();
                for _ in 0..a {
                    search = pat.search(&search, expand, trans)?;
                }

                let mut result = search.clone();
                for _ in 0..(b-a) {
                    if search.is_empty() {
                        break;
                    }

                    search = pat.search(&search, expand, trans)?;
                    result.join(&search)?;
                }

                Ok(result)
            },
            Self::Concat(a, b) => {
                let search = a.search(search, expand, trans)?;
                if search.is_empty() {
                    Ok(search)
                } else {
                    b.search(&search, expand, trans)
                }
            },
            Self::Alternative(a, b) => {
                a.search(search, expand, trans)?
                    .joining(&b.search(search, expand, trans)?)
            },
        }
    }

    fn parse_group(chars: &mut Chars) -> Result<Self, ParseError> {
        let pat = Self::parse(chars, true)?;

        Self::skip_spaces(chars, true);

        if let Some('|') = chars.peek() {
            chars.next();
            let next = Self::parse_group(chars)?;
            Ok(Self::Alternative(Box::new(pat), Box::new(next)))
        } else {
            Ok(pat)
        }
    }

    fn parse(chars: &mut Chars, in_group: bool) -> Result<Self, ParseError> {
        Self::skip_spaces(chars, in_group);

        // Parse a single term of the pattern
        let mut split_text = false;
        let pat = match chars.peek() {
            None => Self::Empty,
            Some('#') => Self::parse_exact(chars)?,
            Some('%') => Self::parse_trans(chars)?,
            Some('&') => Self::parse_assert(chars)?,
            Some('(') => Self::parse_paren(chars)?,
            Some('[') => Self::Set(Self::parse_bracket(chars)?),
            Some('\\') => {
                chars.next();
                Self::Set(Self::parse_escape(chars)?)
            }
            Some('<' | '^') => {
                chars.next();
                Self::AssertStart
            },
            Some('~') => {
                chars.next();
                Self::AssertMiddle
            },
            Some('>' | '$') => {
                chars.next();
                Self::AssertEnd
            },
            Some('_') => {
                chars.next();
                Self::Set(LetterSet::any())
            },
            Some(_) => {
                split_text = true;
                Self::parse_text(chars, in_group)
            },
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
                            Box::new(Self::Repeat(Box::from(Self::Literal(Box::from(&text[split..]))), a, b)))
                    },
                    _ => Self::Repeat(Box::new(pat), a, b),
                }
            },
        };

        // Continue parsing if possible, and concatenate the result
        let next = Self::parse(chars, in_group)?;
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
            Ok(Pattern::Exact(Box::new(Self::parse_paren(chars)?)))
        } else {
            Err(ParseError::InvalidExact)
        }
    }

    fn parse_trans(chars: &mut Chars) -> Result<Self, ParseError> {
        chars.next();

        if let Some('(') = chars.peek() {
            Ok(Pattern::Trans(Box::new(Self::parse_paren(chars)?)))
        } else {
            Err(ParseError::InvalidTransliteration)
        }
    }

    fn parse_assert(chars: &mut Chars) -> Result<Self, ParseError> {
        chars.next();

        if let Some('[') = chars.peek() {
            Ok(Pattern::Assert(Self::parse_bracket(chars)?))
        } else {
            Err(ParseError::InvalidAssert)
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
            },
            Some(&found) => Err(Query::expected_character(SurroundKind::Paren, found)),
        }
    }

    fn parse_bracket(chars: &mut Chars) -> Result<LetterSet, ParseError> {
        chars.next();

        let mut allowed = LetterSet::empty();
        let mut negated = false;
        if let Some('!' | '^') = chars.peek() {
            chars.next();
            negated = true;
        }

        loop {
            match chars.next() {
                None => {
                    return Err(ParseError::Missing(SurroundKind::Bracket));
                }
                Some('-') => {
                    return Err(ParseError::PartialRange);
                }
                Some(']') => {
                    if negated {
                        allowed = allowed.complement();
                    }

                    return Ok(allowed);
                }
                Some('\\') => {
                    allowed = allowed.union(Self::parse_escape(chars)?);
                }
                Some(' ') => {}
                Some(ch) => {
                    allowed = allowed.union(Self::parse_range(chars, ch)?);
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

                        start.range(end).map_err(|(a, b)| ParseError::InvalidRange(a, b))
                    } else {
                        Err(ParseError::PartialRange)
                    }
                } else {
                    Err(ParseError::PartialRange)
                }
            } else {
                Ok(LetterSet::single(start))
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
            Self::skip_spaces(chars, in_group);

            if !Letter::is_valid(ch) {
                break;
            }

            chars.next();
            buffer.push(ch);
        }

        if buffer.is_empty() {
            Self::Empty
        } else {
            Self::Literal(Letter::parse_str(&buffer))
        }
    }

    fn parse_repeat(chars: &mut Chars) -> Result<(u8, u8), ParseError> {
        match chars.peek() {
            Some('?') => {
                chars.next();
                Ok((0, 1))
            },
            Some('*') => {
                chars.next();
                Ok((0, u8::MAX))
            },
            Some('+') => {
                chars.next();
                Ok((1, u8::MAX))
            },
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
                            Some(found) => Err(Query::expected_character(SurroundKind::Brace, found)),
                        }
                    }
                    Some(ch) => Err(Query::invalid_character(ch, ParseError::InvalidRepetition)),
                }
            },
            _ => Ok((1, 1))
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
            buffer.parse().map_err(|_| ParseError::InvalidRepetitionIndex)
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
