use std::fmt::{self, Display};

use thiserror::Error;

use unicode_names2 as unicode;

use crate::dictionary::{PULLI, Word, Letter, LetterSet, Category};
use crate::search::{SearchError, Search, SearchResult, tree};

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
    #[error("Invalid syntax: alternatives must be surrounded by parentheses.")]
    SurroundWithParens,
    #[error("Invalid syntax: expected parentheses after hash symbol.")]
    InvalidExact,
    #[error("Invalid repetition: number must be between 0 and 255.")]
    InvalidRepetitionIndex,
    #[error("Invalid range: partial ranges are not allowed.")]
    PartialRange,
    #[error("Invalid range: cannot create range between {0} and {1}.")]
    InvalidRange(Category, Category),
    #[error("Invalid syntax: character not allowed in repetition: '{0}'.")]
    InvalidRepetition(char),
    #[error("Invalid repetition: lower bound ({0}) must not exceed upper bound ({1}).")]
    InvalidRepetitionRange(u8, u8),
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
                        // Replace "X" with <#(X)>
                        pat = Pattern::Concat(
                            Box::new(Pattern::AssertStart),
                            Box::new(Pattern::Concat(
                                Box::new(Pattern::Exact(Box::new(pat))),
                                Box::new(Pattern::AssertEnd))));
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
                    None | Some(' ') | Some(':') => {
                        return Err(ParseError::Missing(SurroundKind::Quote));
                    }
                    Some('"') => {
                        chars.next();
                    }
                    Some('|') => {
                        return Err(ParseError::SurroundWithParens);
                    }
                    Some(&found) => {
                        return Err(ParseError::ExpectedCharacter {
                            expected: SurroundKind::Quote,
                            found,
                        });
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
                    return Err(Self::invalid_character(ch, ParseError::InvalidCharacter));
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
            intersect.push(pat.search(&tree::search_word())?.end()?);
        }

        // Search using positive definition patterns
        for pat in &self.definition_patterns {
            intersect.push(pat.search(&tree::search_definition())?.end()?);
        }

        let mut process_negatives = || {
            // Search using negative word patterns
            for pat in &self.negative_word_patterns {
                difference.push(pat.search(&tree::search_word())?.end()?);
            }

            // Search using negative definition patterns
            for pat in &self.negative_definition_patterns {
                difference.push(pat.search(&tree::search_definition())?.end()?);
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
                    intersect.push(pat.search(&tree::search_definition())?.end()?);
                }

                // Search using negative words patterns as definition patterns
                for pat in &self.negative_word_patterns {
                    difference.push(pat.search(&tree::search_definition())?.end()?);
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
        } else if let Some(name) = unicode::name(ch) {
            if ch.is_whitespace() {
                ParseError::InvalidUnicodeWhitespace(name)
            } else {
                ParseError::InvalidUnicodeCharacter(name)
            }
        } else {
            ParseError::InvalidOtherCharacter(ch)
        }
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for pat in &self.word_patterns {
            if first {
                first = false;
            } else {
                write!(f, " ")?;
            }
            write!(f, "{}", pat)?;
        }

        for pat in &self.negative_word_patterns {
            if first {
                first = false;
            } else {
                write!(f, " ")?;
            }
            write!(f, "!{}", pat)?;
        }

        write!(f, ":")?;
        for pat in &self.definition_patterns {
            write!(f, " {}", pat)?;
        }

        for pat in &self.negative_definition_patterns {
            write!(f, " !{}", pat)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
enum Pattern {
    Empty,
    AssertStart,
    AssertEnd,
    Set(LetterSet),
    Literal(Box<Word>),
    Exact(Box<Pattern>),
    Repeat(Box<Pattern>, u8, u8),
    Concat(Box<Pattern>, Box<Pattern>),
    Alternative(Box<Pattern>, Box<Pattern>),
}

impl Pattern {
    fn search<S: Search>(&self, search: &S) -> Result<S, SearchError> {
        match self {
            Self::Empty => Ok(search.clone()),
            Self::AssertStart => Ok(search.asserting_start()),
            Self::AssertEnd => Ok(search.asserting_end()),
            &Self::Set(lts) => search.matching(lts),
            Self::Literal(word) => Ok(search.literal(word)),
            Self::Exact(pat) => pat.search(search),
            &Self::Repeat(ref pat, a, b) => {
                let mut search = search.clone();
                for _ in 0..a {
                    search = pat.search(&search)?;
                }

                let mut result = search.clone();
                for _ in 0..(b-a) {
                    if search.is_empty() {
                        break;
                    }

                    search = pat.search(&search)?;
                    result.join(&search)?;
                }

                Ok(result)
            },
            Self::Concat(a, b) => {
                let search = a.search(search)?;
                if search.is_empty() {
                    Ok(search)
                } else {
                    b.search(&search)
                }
            },
            Self::Alternative(a, b) => a.search(search)?.joining(&b.search(search)?),
        }
    }

    fn parse_group(chars: &mut Chars) -> Result<Self, ParseError> {
        let pat = Self::parse(chars, true)?;

        if let Some('|') = chars.peek() {
            chars.next();
            let next = Self::parse_group(chars)?;
            Ok(Self::Alternative(Box::new(pat), Box::new(next)))
        } else {
            Ok(pat)
        }
    }

    fn parse(chars: &mut Chars, in_group: bool) -> Result<Self, ParseError> {
        // Parse leading whitespace if in a group
        if in_group {
            while let Some(' ') = chars.peek() {
                chars.next();
            }
        }

        // Parse a single term of the pattern
        let mut split_text = false;
        let pat = match chars.peek() {
            None => Self::Empty,
            Some('#') => Self::parse_exact(chars)?,
            Some('(') => Self::parse_paren(chars)?,
            Some('[') => Self::parse_bracket(chars)?,
            Some('<' | '^') => {
                chars.next();
                Self::AssertStart
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
                Self::parse_text(chars)
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

    fn parse_exact(chars: &mut Chars) -> Result<Self, ParseError> {
        chars.next();

        if let Some('(') = chars.peek() {
            Ok(Pattern::Exact(Box::new(Self::parse_paren(chars)?)))
        } else {
            Err(ParseError::InvalidExact)
        }
    }

    fn parse_paren(chars: &mut Chars) -> Result<Self, ParseError> {
        chars.next();

        let pat = Self::parse_group(chars)?;
        while let Some(' ') = chars.peek() {
            chars.next();
        }

        match chars.peek() {
            None => Err(ParseError::Missing(SurroundKind::Paren)),
            Some(')') => {
                chars.next();
                Ok(pat)
            },
            Some(&found) => Err(ParseError::ExpectedCharacter {
                expected: SurroundKind::Paren,
                found,
            }),
        }
    }

    fn parse_bracket(chars: &mut Chars) -> Result<Self, ParseError> {
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

                    return Ok(Self::Set(allowed));
                }
                Some(ch) => {
                    allowed = allowed.union(Self::parse_range(chars, ch)?);
                }
            }
        }
    }

    fn parse_range(chars: &mut Chars, ch: char) -> Result<LetterSet, ParseError> {
        if let Some(ch) = Letter::parse(ch) {
            if let Some(&PULLI) = chars.peek() {
                chars.next();
            }

            if let Some('-') = chars.peek() {
                chars.next();
                if let Some(&x) = chars.peek() {
                    if let Some(end) = Letter::parse(x) {
                        let mut result = LetterSet::empty();
                        for i in ch.range(end).map_err(|(a, b)| ParseError::InvalidRange(a, b))? {
                            result = result.union(LetterSet::single(i));
                        }

                        Ok(result)
                    } else {
                        Err(ParseError::PartialRange)
                    }
                } else {
                    Err(ParseError::PartialRange)
                }
            } else {
                Ok(LetterSet::single(ch))
            }
        } else {
            Ok(LetterSet::empty())
        }
    }

    fn parse_text(chars: &mut Chars) -> Self {
        let mut buffer = String::new();
        while let Some(&ch) = chars.peek() {
            if ch == '-' {
                chars.next();
                continue;
            }

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
                            Some(found) => Err(ParseError::ExpectedCharacter {
                                expected: SurroundKind::Brace,
                                found,
                            }),
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
}

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "()"),
            Self::AssertStart => write!(f, "<"),
            Self::AssertEnd => write!(f, ">"),
            Self::Set(cs) => write!(f, "{}", cs),
            Self::Literal(s) => write!(f, "({})", Letter::to_str(s)),
            Self::Exact(pat) => write!(f, "#({})", pat),
            Self::Repeat(pat, a, b) => write!(f, "{}{{{},{}}}", pat, a, b),
            Self::Concat(a, b) => write!(f, "{}{}", a, b),
            Self::Alternative(a, b) => write!(f, "({}|{})", a, b),
        }
    }
}
