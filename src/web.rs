#![allow(clippy::blocks_in_conditions)]
#![allow(clippy::needless_borrows_for_generic_args)]
#![allow(clippy::result_large_err)]

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::fmt::Write;
use std::mem;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

use once_cell::sync::OnceCell;

use serde::Serialize;

use rand::seq::SliceRandom;

use regex::Regex;

use rocket::http::{uri, RawStr, Status};
use rocket::response::content::RawHtml;
use rocket::response::Redirect;
use rocket::serde::json::Json;
use rocket::Request;
use rocket_dyn_templates::Template;

use crate::annotate::{TextSegment, WordCount};
use crate::dictionary::{self, *};
use crate::query::{ExpandOptions, Pattern, Query, SearchKind};
use crate::refs::{self, RefWithSub};
use crate::search::word::WordSearch;
use crate::search::{Search, SearchRankingEntry};
use crate::tamil::{self, LetterSet, Word};

const EXAMPLE_REFRESH_TIME_SECS: u64 = 30;
const EXAMPLE_CYCLE_PERIOD: usize = 9;

const MAX_OTHER_SECTIONS: usize = 5;
const MAX_EXPAND: usize = 250;

static RESULT_COUNT: AtomicU64 = AtomicU64::new(0);
static SEARCH_COUNT: AtomicU64 = AtomicU64::new(0);
static SUGGEST_COUNT: AtomicU64 = AtomicU64::new(0);
static ANNOTATE_COUNT: AtomicU64 = AtomicU64::new(0);

pub fn render_template(template: &'static str, context: impl Serialize) -> Template {
    #[derive(Serialize)]
    struct Versioned<T: Serialize> {
        version: &'static str,
        #[serde(flatten)]
        inner: T,
    }

    Template::render(
        template,
        Versioned {
            version: crate::version(),
            inner: context,
        },
    )
}

#[derive(Serialize, Debug)]
pub struct Example {
    latin: &'static str,
    tamil: Box<str>,
}

impl Example {
    fn new(&(latin, tamil): &(&'static str, &Word)) -> Self {
        // Check whether the Latin pattern matches the Tamil (only in debug)
        debug_assert!(
            Pattern::parse(latin)
                .expect("invalid pattern")
                .search(WordSearch::new(tamil), ExpandOptions::TRANS)
                .unwrap_or_else(|e| match e {})
                .end()
                .unwrap_or_else(|e| match e {}),
            "pattern {latin:?} does not match {tamil:?}",
        );

        Self {
            latin,
            tamil: tamil.to_string().into_boxed_str(),
        }
    }
}

pub fn current_example() -> &'static Example {
    static EXAMPLES: OnceCell<Box<[Example]>> = OnceCell::new();
    static INDICES: OnceCell<Box<[u8]>> = OnceCell::new();

    #[rustfmt::skip]
    let examples = EXAMPLES.get_or_init(|| {
        let examples = &[
            ("appuram",    word![A, P, P, U, AlveolarR, A, M]),
            ("anubavam",   word![A, AlveolarN, U, P, A, V, A, M]),
            ("aayiram",    word![LongA, Y, I, R, A, M]),
            ("ippadi",     word![I, P, P, A, RetroT, I]),
            ("ishtam",     word![I, Sh, RetroT, A, M]),
            ("eeram",      word![LongI, R, A, M]),
            ("utkaar",     word![U, RetroT, K, LongA, R]),
            ("ulagam",     word![U, AlveolarL, A, K, A, M]),
            ("ellaam",     word![E, AlveolarL, AlveolarL, LongA, M]),
            ("emaatru",    word![LongE, M, LongA, AlveolarR, AlveolarR, U]),
            ("orumai",     word![O, R, U, M, Ai]),
            ("onbadhu",    word![O, AlveolarN, P, A, T, U]),
            ("kadai",      word![K, A, RetroT, Ai]),
            ("kadhai",     word![K, A, T, Ai]),
            ("kashtam",    word![K, A, Sh, RetroT, A, M]),
            ("kaatru",     word![K, LongA, AlveolarR, AlveolarR, U]),
            ("kaalam",     word![K, LongA, AlveolarL, A, M]),
            ("koottam",    word![K, LongU, RetroT, RetroT, A, M]),
            ("kooppidu",   word![K, LongU, P, P, I, RetroT, U]),
            ("konduvaa",   word![K, O, RetroN, RetroT, U, V, LongA]),
            ("sattendru",  word![Ch, A, RetroT, RetroT, E, AlveolarN, AlveolarR, U]),
            ("sandhosham", word![Ch, A, N, T, LongO, Sh, A, M]),
            ("saappidu",   word![Ch, LongA, P, P, I, RetroT, U]),
            ("nyaabagam",  word![Ny, LongA, P, A, K, A, M]),
            ("thangam",    word![T, A, Ng, K, A, M]),
            ("thamizh",    word![T, A, M, I, Zh]),
            ("thottam",    word![T, LongO, RetroT, RetroT, A, M]),
            ("niyaayam",   word![N, I, Y, LongA, Y, A, M]),
            ("nenjam",     word![N, E, Ny, Ch, A, M]),
            ("nuzhai",     word![N, U, Zh, Ai]),
            ("pandhu",     word![P, A, N, T, U]),
            ("pazham",     word![P, A, Zh, A, M]),
            ("puthagam",   word![P, U, T, T, A, K, A, M]),
            ("mazhai",     word![M, A, Zh, Ai]),
            ("maatram",    word![M, LongA, AlveolarR, AlveolarR, A, M]),
            ("maunam",     word![M, Au, AlveolarN, A, M]),
            ("yaanai",     word![Y, LongA, AlveolarN, Ai]),
            ("vanakkam",   word![V, A, RetroN, A, K, K, A, M]),
            ("vinyaani",   word![V, I, Ny, Ny, LongA, AlveolarN, I]),
            ("veedu",      word![V, LongI, RetroT, U]),
        ];

        examples.iter().map(Example::new).collect()
    });

    let indices = INDICES.get_or_init(|| {
        let count = examples.len();
        assert!(count <= u8::MAX as usize);

        let mut indices = Vec::with_capacity(count * EXAMPLE_CYCLE_PERIOD);

        let mut rng = rand::thread_rng();
        for _ in 0..EXAMPLE_CYCLE_PERIOD {
            let mut slice: Box<[u8]> = (0..count).map(|i| i as u8).collect();
            slice.shuffle(&mut rng);
            indices.extend_from_slice(&slice);
        }

        indices.into_boxed_slice()
    });

    let uptime = crate::uptime();
    let num_refreshes = uptime.as_secs() / EXAMPLE_REFRESH_TIME_SECS;
    let index = indices[num_refreshes as usize % indices.len()];
    &examples[index as usize]
}

#[derive(Serialize, Debug)]
struct IndexTemplate {
    example: &'static Example,
    advanced: bool,
}

#[get("/")]
pub fn index() -> Template {
    let example = current_example();
    render_template(
        "index",
        IndexTemplate {
            example,
            advanced: false,
        },
    )
}

#[get("/advanced")]
pub fn advanced() -> Template {
    let example = current_example();
    render_template(
        "index",
        IndexTemplate {
            example,
            advanced: true,
        },
    )
}

#[get("/grammar")]
pub fn grammar() -> Template {
    render_template("grammar", ())
}

#[get("/annotate")]
pub fn annotate() -> Template {
    render_template("annotate", ())
}

#[derive(Serialize, FromForm, UriDisplayQuery, Default, Debug)]
pub struct QueryKindSet {
    v: bool,
    va: bool,
    vm: bool,
    tv: bool,
    p: bool,
    pa: bool,
    sp: bool,
    vp: bool,
    i: bool,
    ii: bool,
}

impl uri::fmt::Ignorable<uri::fmt::Query> for QueryKindSet {}

impl QueryKindSet {
    pub fn to_kind_set(&self) -> KindSet {
        #[rustfmt::skip]
        let &QueryKindSet { v, va, vm, tv, p, pa, sp, vp, i, ii } = self;

        [v, va, vm, tv, p, pa, sp, vp, i, ii]
            .into_iter()
            .enumerate()
            .filter_map(|(i, b)| {
                if b {
                    Some(KindSet::single(ALL_KINDS[i]))
                } else {
                    None
                }
            })
            .reduce(KindSet::union)
            .unwrap_or(KindSet::empty())
    }
}

fn link(word: &str) -> String {
    link_no_escape(&Query::escape(word))
}

fn link_no_escape(escaped: &str) -> String {
    uri!(search(escaped, _, _)).to_string()
}

#[derive(Debug)]
struct RenderState {
    entry: &'static Entry,
    definition_count: usize,
    highlight_ranges: Vec<WordRange>,
}

#[derive(Serialize, Debug)]
struct ResultSegment {
    text: &'static str,
    uri: String,
    tag: Option<&'static str>,
    close_tag: bool,
}

impl ResultSegment {
    fn new(kind: SegmentKind, text: &'static str) -> Self {
        let (uri, tag, close_tag) = match kind {
            SegmentKind::Text => (String::new(), None, true),
            SegmentKind::Reference => (link(text), None, true),
            SegmentKind::Superscript => (String::new(), Some("sup"), true),
            SegmentKind::Bold => (String::new(), Some("strong"), true),
            SegmentKind::WordBreak => (String::new(), Some("wbr"), false),
        };

        Self {
            text,
            uri,
            tag,
            close_tag,
        }
    }

    fn render_ref_with_sub(state: &RenderState, ref_with_sub: RefWithSub) -> Vec<Self> {
        let (word, sub_str) = ref_with_sub.get_parts(&state.entry.text);

        let Ok(sub) = sub_str.parse() else {
            return vec![Self::new(SegmentKind::Reference, word)];
        };

        let uri = refs::get_entry(word, sub)
            .map(|entry| uri!(entries(entry.to_string())).to_string())
            .unwrap_or_else(|| link(word));

        let ref_seg = Self {
            text: word,
            uri,
            tag: None,
            close_tag: true,
        };

        let sub_seg = Self::new(SegmentKind::Superscript, sub_str);

        vec![ref_seg, sub_seg]
    }

    fn render(state: &mut RenderState, seg: &'static Segment) -> Vec<Self> {
        let entry = state.entry;

        if seg.kind() == SegmentKind::Reference {
            let text = &entry.text[seg.start()..seg.end()];
            return vec![Self::new(SegmentKind::Reference, text)];
        }

        let mut segments = Vec::with_capacity(1);
        let mut push = |kind, start, end| {
            let text = &entry.text[start..end];
            if !text.is_empty() {
                segments.push(Self::new(kind, text));
            }
        };

        // Highlight any words which appear in this segment
        let mut start = seg.start();
        let end = seg.end();
        while let Some(range) = state.highlight_ranges.last() {
            let range_start = range.start();
            let range_end = range.end();

            // If this highlight was meant for a previous ref, ignore it
            if range_end < start {
                state.highlight_ranges.pop();
                continue;
            }

            // If this highlight was meant for the next segment, leave it
            if range_end > end {
                break;
            }

            // Split the segment into parts to add a bold segment
            state.highlight_ranges.pop();
            push(seg.kind(), start, range_start);
            push(SegmentKind::Bold, range_start, range_end);
            start = range_end;
        }

        // Push any remaining text in the segment
        push(seg.kind(), start, end);
        segments
    }
}

#[derive(Serialize, Default, Debug)]
struct ResultParagraph {
    segments: Vec<ResultSegment>,
}

impl ResultParagraph {
    fn render(state: &mut RenderState, para: &'static Paragraph) -> Self {
        let segments = refs::group_refs(para.iter())
            .flat_map(|seg| match seg {
                Ok(seg) => ResultSegment::render_ref_with_sub(state, seg),
                Err(seg) => ResultSegment::render(state, seg),
            })
            .collect();

        Self { segments }
    }
}

#[derive(Serialize, Debug)]
struct ResultSection {
    is_header: bool,
    definition_start: usize,
    section: ResultParagraph,
    paragraphs: Vec<ResultParagraph>,
}

impl ResultSection {
    fn render_all(entry: &'static Entry, words: BTreeSet<WordIndex>) -> Vec<Self> {
        // Create a stack of highlighted word ranges
        let mut highlight_ranges = Vec::new();
        for &index in words.iter().rev() {
            if index == NO_WORD {
                continue;
            }

            highlight_ranges.push(entry.word_ranges[index as usize]);
        }

        let mut state = RenderState {
            entry,
            definition_count: 1,
            highlight_ranges,
        };

        // Render all of the sections for the entry
        let mut sections: Vec<_> = entry
            .sections
            .iter()
            .enumerate()
            .map(|(i, sec)| Self::render(&mut state, i == 0, sec))
            .collect();

        // Check for single-segment, single-definition sections to join with the header
        if sections.len() == 1 {
            let only = &mut sections[0];
            if only.paragraphs.len() == 1 {
                let segments = &mut only.section.segments;

                // Pick the joining string based on the last character of the header
                let joiner = match segments.last().and_then(|seg| seg.text.chars().last()) {
                    None | Some('.' | ',' | ':' | ';') => " ",
                    _ => ": ",
                };

                // Push a joining segment before appending the definition
                segments.push(ResultSegment::new(SegmentKind::Text, joiner));
                segments.append(&mut only.paragraphs[0].segments);
                only.paragraphs = Vec::new();
            }
        }

        sections
    }

    fn collapsed(entry: &'static Entry) -> Self {
        let mut state = RenderState {
            entry,
            definition_count: 1,
            highlight_ranges: Vec::new(),
        };

        let mut section = ResultParagraph::render(&mut state, &entry.sections[0][0]);

        if entry.sections.len() > 1 || entry.sections[0].len() > 1 {
            section
                .segments
                .push(ResultSegment::new(SegmentKind::Text, " [...]"));
        }

        Self {
            is_header: true,
            definition_start: 0,
            section,
            paragraphs: Vec::new(),
        }
    }

    fn render(state: &mut RenderState, is_header: bool, sec: &'static Section) -> Self {
        let definition_start = state.definition_count;

        let mut iter = sec.iter().map(|para| ResultParagraph::render(state, para));

        let section = iter.next().unwrap_or_default();
        let paragraphs: Vec<_> = iter.collect();
        state.definition_count += paragraphs.len();

        Self {
            is_header,
            definition_start,
            section,
            paragraphs,
        }
    }
}

#[derive(Serialize, Debug)]
struct ResultEntry {
    uri: String,
    word: &'static str,
    pronunciation: Option<&'static str>,
    index: u16,
    subword: Option<u8>,
    kind: String,
    sections: Vec<ResultSection>,
}

impl ResultEntry {
    fn render_all(entries: Vec<SearchRankingEntry>, expanded: bool) -> Vec<Self> {
        entries
            .into_iter()
            .map(|entry| Self::render(entry, expanded))
            .collect()
    }

    fn render(entry: SearchRankingEntry, expanded: bool) -> Self {
        let SearchRankingEntry { entry, words } = entry;

        let sections = if expanded || words.len() != 1 || !words.contains(&NO_WORD) {
            ResultSection::render_all(entry, words)
        } else {
            vec![ResultSection::collapsed(entry)]
        };

        let mut kind = entry.kind_strs.join("/");

        if let Some(hint) = &entry.hint {
            kind = format!("{hint} {kind}");
        }

        Self {
            uri: link(entry.primary_word()),
            word: &entry.word,
            pronunciation: entry.pronunciation_str.as_deref(),
            index: entry.index,
            subword: entry.subword,
            kind,
            sections,
        }
    }
}

#[derive(Serialize, Debug)]
struct NumWithPlural {
    num: usize,
    is_plural: bool,
}

impl From<usize> for NumWithPlural {
    fn from(num: usize) -> Self {
        Self {
            num,
            is_plural: num != 1,
        }
    }
}

fn looks_english(s: &str) -> bool {
    static ENGLISH_REGEX: OnceCell<Regex> = OnceCell::new();

    let english_regex = ENGLISH_REGEX.get_or_init(|| {
        Regex::new(
            r#"^[td]r|[kgcspbw][lrwsy]|[td][lwsy]|s[ckmnpsty]|[fqx]|[kghcjstdpb]$|ng$|[aeiou].e$"#,
        )
        .unwrap()
    });

    english_regex.is_match(s)
}

#[derive(Serialize, Debug)]
struct SearchTemplate<'a> {
    query: &'a str,
    definition: &'a str,
    other_uri: String,
    kinds: QueryKindSet,
    #[serde(skip)]
    kind_set: KindSet,
    explicit: bool,
    advanced: bool,
    hide_other: bool,
    error: bool,
    def_uri: Option<String>,
    message: Option<String>,
    exact: Vec<ResultEntry>,
    best: Vec<ResultEntry>,
    related: Vec<ResultEntry>,
    other: Vec<ResultEntry>,
    exact_and_best_count: NumWithPlural,
    related_count: NumWithPlural,
    other_count: NumWithPlural,
}

impl<'a> SearchTemplate<'a> {
    fn new(query: &'a str, definition: &'a str, kinds: QueryKindSet, all: bool) -> Self {
        let query = query.trim();
        let definition = definition.trim();
        let other_uri = if all {
            String::new()
        } else if definition.is_empty() {
            uri!(search_all(query, _, &kinds)).to_string()
        } else {
            uri!(search_all(query, Some(definition), &kinds)).to_string()
        };

        let kind_set = kinds.to_kind_set();
        let advanced = !definition.is_empty() || !kind_set.is_empty();

        Self {
            query,
            definition,
            other_uri,
            kinds,
            kind_set,
            explicit: false,
            advanced,
            hide_other: !all,
            error: false,
            def_uri: None,
            message: None,
            exact: Vec::new(),
            best: Vec::new(),
            related: Vec::new(),
            other: Vec::new(),
            exact_and_best_count: 0.into(),
            related_count: 0.into(),
            other_count: 0.into(),
        }
    }

    fn is_empty(&self) -> bool {
        self.query.is_empty() && self.definition.is_empty() && self.kind_set.is_empty()
    }

    fn error(&mut self, err: impl ToString) {
        self.error = true;
        self.message = Some(err.to_string());
    }

    fn message(&mut self, msg: impl ToString) {
        if !self.error {
            self.message = Some(msg.to_string());
        }
    }

    fn search(&mut self, query: Query) {
        match query.search() {
            Err(err) => self.error(err),
            Ok((result, kind)) => {
                // Convert the search results to a ranked page
                let ranking = result.rank();
                if ranking.is_empty() {
                    self.message("No results found.");
                } else {
                    // Count how many of each result kind there are
                    self.exact_and_best_count = (ranking.exact.len() + ranking.best.len()).into();
                    self.related_count = ranking.related.len().into();
                    self.other_count = ranking.other.len().into();

                    // Render the search results so they can be displayed
                    let mut total = self.exact_and_best_count.num;
                    self.exact = ResultEntry::render_all(ranking.exact, total <= MAX_EXPAND);
                    self.best = ResultEntry::render_all(ranking.best, total <= MAX_EXPAND);

                    total += self.related_count.num;
                    self.related = ResultEntry::render_all(ranking.related, total <= MAX_EXPAND);

                    total += self.other_count.num;
                    self.other = ResultEntry::render_all(ranking.other, total <= MAX_EXPAND);

                    RESULT_COUNT.fetch_add(total as u64, Ordering::Relaxed);

                    // Add special warnings for unintuitive situations
                    match kind {
                        SearchKind::AsSpecified => {
                            // Check for unwanted implicit transliteration
                            if !self.advanced
                                && (!ranking.good_search || looks_english(self.query))
                                && query.implicit_transliteration()
                            {
                                let d = Some(format!("{} {}", self.query, self.definition));
                                self.def_uri = Some(uri!(search("", d, _)).to_string());
                            }
                        }

                        SearchKind::DefSearch => {
                            self.message("No words found, searching definitions.");
                        }
                    }
                }

                // Only hide the other results if they are too long
                if self.other_count.num <= MAX_OTHER_SECTIONS {
                    // Count the number of sections in the other section
                    let other_section_count = self
                        .other
                        .iter()
                        .map(|entry| entry.sections.len())
                        .sum::<usize>();

                    if other_section_count <= MAX_OTHER_SECTIONS {
                        self.hide_other = false;
                    }
                }
            }
        }
    }

    fn explicit(&mut self, results: &BTreeSet<EntryIndex>) {
        self.explicit = true;

        let count = results.len();
        if count == 0 {
            self.message("No results found.");
            return;
        }

        let entries = dictionary::entries();

        let results = results
            .iter()
            .map(|&index| SearchRankingEntry {
                entry: &entries[index as usize],
                words: BTreeSet::new(),
            })
            .collect();

        self.exact_and_best_count = count.into();
        self.exact = ResultEntry::render_all(results, true);
        self.hide_other = true;

        RESULT_COUNT.fetch_add(count as u64, Ordering::Relaxed);
    }
}

#[get("/entries/<ids>")]
pub fn entries(ids: &str) -> Template {
    SEARCH_COUNT.fetch_add(1, Ordering::Relaxed);

    let entry_count = dictionary::entries().len();

    let set = ids
        .split(',')
        .map(str::trim)
        .filter_map(|id| id.parse().ok())
        .filter(|&id| (id as usize) < entry_count)
        .collect();

    let mut search = SearchTemplate::new("", "", QueryKindSet::default(), false);
    search.explicit(&set);

    render_template("search", search)
}

#[get("/random")]
pub fn random() -> Result<Template, Redirect> {
    let word = Query::escape(Entry::random().primary_word());
    search_query(&word, "", QueryKindSet::default(), false)
}

#[get("/search?all&<q>&<d>&<k..>")]
pub fn search_all(q: &str, d: Option<&str>, k: QueryKindSet) -> Result<Template, Redirect> {
    search_query(q, d.unwrap_or(""), k, true)
}

#[get("/search?<q>&<d>&<k..>")]
pub fn search(q: &str, d: Option<&str>, k: QueryKindSet) -> Result<Template, Redirect> {
    search_query(q, d.unwrap_or(""), k, false)
}

#[get("/search")]
pub fn search_no_query() -> Redirect {
    Redirect::to(uri!(index()))
}

fn search_query(q: &str, d: &str, k: QueryKindSet, all: bool) -> Result<Template, Redirect> {
    SEARCH_COUNT.fetch_add(1, Ordering::Relaxed);

    let mut search = SearchTemplate::new(q, d, k, all);
    if search.is_empty() {
        return Err(Redirect::to(uri!(index())));
    }

    match Query::parse(search.query, search.definition, search.kind_set) {
        Err(err) => search.error(err),
        Ok(query) => search.search(query),
    }

    Ok(render_template("search", search))
}

#[derive(Serialize)]
pub struct SuggestResponseEntry {
    word: &'static str,
    completion: String,
    uri: String,
}

impl From<&'static Entry> for SuggestResponseEntry {
    fn from(entry: &'static Entry) -> Self {
        let completion = Query::escape(entry.primary_word());
        let uri = link_no_escape(&completion);

        Self {
            word: &entry.word,
            completion,
            uri,
        }
    }
}

#[get("/api/suggest?<q>&<n>")]
pub fn suggest(q: &str, n: u32) -> Json<Vec<SuggestResponseEntry>> {
    SUGGEST_COUNT.fetch_add(1, Ordering::Relaxed);

    let mut query = Cow::from(q);

    // Check for trailing "a", and allow other letters as well
    let mut append_a = false;
    if let Some(last_character) = query.chars().next_back() {
        if tamil::is_consonant(last_character) {
            query += "\u{bcd}";
            append_a = true;
        }
    }

    // Parse the query into a single pattern
    let Ok(mut pat) = Pattern::parse(&query) else {
        return Json(Vec::new());
    };

    let count = n.min(100);

    // Make the pattern more general if there was a trailing "a"
    if append_a {
        const VOWEL_NOT_A: LetterSet = LetterSet::vowel().difference(letterset![A]);

        // The pattern should always end with a literal, since anything but
        // concatenation would have added some delimiters at the end.
        // Since some pattern expansions rely on a literal being unbroken,
        // we need to directly add the A into the literal, replacing just
        // the literal with an alternative.

        let last = pat.last_mut();
        if let Pattern::Literal(lit) = last {
            let with_a = lit.as_ref() + word![A];
            let without_a = mem::replace(last, Pattern::Empty);

            // <last> = (<last>A|<last>(&[\V^A]|(&[^\V]|>)@))
            *last = Pattern::Alternative(
                Box::new(Pattern::Literal(with_a)),
                Box::new(Pattern::Concat(
                    Box::new(without_a),
                    Box::new(Pattern::Alternative(
                        Box::new(Pattern::AssertNext(VOWEL_NOT_A)),
                        Box::new(Pattern::Concat(
                            Box::new(Pattern::Alternative(
                                Box::new(Pattern::AssertNext(LetterSet::vowel().complement())),
                                Box::new(Pattern::AssertEnd),
                            )),
                            Box::new(Pattern::MarkExpanded),
                        )),
                    )),
                )),
            );
        } else {
            // If for some reason the pattern doesn't end with a literal,
            // fall back to adding just a vowel assertion. This is
            // equivalent since A is always first in dictionary order,
            // so whether its an exact match will not affect the ordering.

            // <pat> = <pat>(&\V|@)
            pat = Pattern::Concat(
                Box::new(pat),
                Box::new(Pattern::Alternative(
                    Box::new(Pattern::AssertNext(LetterSet::vowel())),
                    Box::new(Pattern::MarkExpanded),
                )),
            );
        }
    }

    let Some(list) = pat.suggest(count) else {
        return Json(Vec::new());
    };

    Json(list.suggestions().map(SuggestResponseEntry::from).collect())
}

#[get("/api/annotate?<q>")]
pub fn annotate_api_get(q: &str) -> RawHtml<String> {
    annotate_api(q)
}

#[post("/api/annotate", format = "plain", data = "<body>")]
pub fn annotate_api(body: &str) -> RawHtml<String> {
    ANNOTATE_COUNT.fetch_add(1, Ordering::Relaxed);

    let mut html = String::with_capacity(body.len() * 3 / 2);
    let mut last_was_tamil = false;
    for segment in TextSegment::annotate(body) {
        match segment {
            TextSegment::NonTamil(text) => {
                last_was_tamil = false;
                html.push_str(&RawStr::new(text).html_escape());
            }

            TextSegment::Tamil(word, None) => {
                // Always an individual word, so last_was_tamil is irrelevant
                write!(html, "{}", word).unwrap();
            }

            TextSegment::Tamil(word, Some(choice)) => {
                // Add a word break between segments to break up long words
                if last_was_tamil {
                    html.push_str("<wbr>");
                } else {
                    last_was_tamil = true;
                }

                write!(
                    html,
                    r#"<a href="{}">{}</a>"#,
                    uri!(entries(choice.ids())),
                    word,
                )
                .unwrap();
            }
        }
    }

    RawHtml(html)
}

#[derive(Serialize)]
pub struct AnnotateResponse<'a> {
    segments: Vec<AnnotateResponseEntry<'a>>,
    top: Vec<(&'static str, usize)>,
}

#[derive(Serialize)]
pub struct AnnotateResponseEntry<'a> {
    word: Cow<'a, str>,
    ids: Option<BTreeSet<EntryIndex>>,
}

#[get("/api/annotate/raw?<q>&<n>&<min>")]
pub fn annotate_raw_get(q: &str, n: Option<u32>, min: Option<u32>) -> Json<AnnotateResponse> {
    annotate_raw(n, min, q)
}

#[post("/api/annotate/raw?<n>&<min>", format = "plain", data = "<body>")]
pub fn annotate_raw(n: Option<u32>, min: Option<u32>, body: &str) -> Json<AnnotateResponse> {
    ANNOTATE_COUNT.fetch_add(1, Ordering::Relaxed);

    let n = n.unwrap_or(25).min(100) as usize;
    let min = min.unwrap_or(2) as usize;

    let mut count = if n == 0 {
        None
    } else {
        Some(WordCount::default())
    };

    let mut segments = Vec::new();
    for segment in TextSegment::annotate(body) {
        match segment {
            TextSegment::NonTamil(text) => {
                segments.push(AnnotateResponseEntry {
                    word: text.into(),
                    ids: None,
                });
            }

            TextSegment::Tamil(word, None) => {
                segments.push(AnnotateResponseEntry {
                    word: word.to_string().into(),
                    ids: Some(BTreeSet::new()),
                });
            }

            TextSegment::Tamil(word, Some(choice)) => {
                // If counting words, add this choice to the count
                if let Some(count) = &mut count {
                    count.insert(&choice);
                }

                // Convert the choice to be owned
                let entries = match Rc::try_unwrap(choice) {
                    Ok(choice) => choice.entries,
                    Err(choice) => choice.entries.clone(),
                };

                segments.push(AnnotateResponseEntry {
                    word: word.to_string().into(),
                    ids: Some(entries),
                });
            }
        }
    }

    // If counting, take the top <n> words with at least <min> uses
    let top = if let Some(count) = count {
        let mut vec = count.into_vec(min);
        if vec.len() > n {
            vec.drain(n..);
        }
        vec
    } else {
        Vec::new()
    };

    Json(AnnotateResponse { segments, top })
}

#[get("/api/stats")]
pub fn stats() -> String {
    let secs = crate::uptime().as_secs();
    let (mins, secs) = (secs / 60, secs % 60);
    let (hours, mins) = (mins / 60, mins % 60);

    let result_count = RESULT_COUNT.load(Ordering::Acquire);
    let search_count = SEARCH_COUNT.load(Ordering::Acquire);
    let suggest_count = SUGGEST_COUNT.load(Ordering::Acquire);
    let annotate_count = ANNOTATE_COUNT.load(Ordering::Acquire);

    format!(
        concat!(
            "uptime={:}:{:02}:{:02}\n",
            "version={}\n",
            "result_count={}\n",
            "search_count={}\n",
            "suggest_count={}\n",
            "annotate_count={}\n",
        ),
        hours,
        mins,
        secs,
        crate::version(),
        result_count,
        search_count,
        suggest_count,
        annotate_count,
    )
}

#[derive(Serialize, Default)]
pub struct Error {
    uri: String,
    not_found: bool,
    code: u16,
    reason: &'static str,
    method: String,
    headers: Vec<String>,
}

#[catch(default)]
pub fn error(status: Status, req: &Request) -> Template {
    let not_found = status == Status::NotFound;

    let mut error = Error {
        uri: req.uri().to_string(),
        not_found,
        code: status.code,
        reason: status.reason_lossy(),
        ..Error::default()
    };

    if !not_found {
        error.headers = req.headers().iter().map(|h| h.to_string()).collect();
        error.method = req.method().to_string();
    }

    render_template("error", error)
}
