use std::borrow::Cow;
use std::collections::BTreeSet;
use std::sync::atomic::{AtomicU64, Ordering};

use serde::Serialize;

use rand::seq::SliceRandom;

use rocket::http::uri;
use rocket::http::Status;
use rocket::response::Redirect;
use rocket::serde::json::Json;
use rocket::Request;
use rocket_dyn_templates::Template;

use crate::dictionary::*;
use crate::query::{Pattern, Query, SearchKind};
use crate::search::word::WordSearch;
use crate::search::{Search, SearchRankingEntry};
use crate::tamil::{self, LetterSet, Word};

const EXAMPLE_REFRESH_TIME_SECS: u64 = 30;
const EXAMPLE_CYCLE_PERIOD: usize = 12;

const MAX_OTHER_SECTIONS: usize = 5;
const MAX_EXPAND: usize = 250;

static RESULT_COUNT: AtomicU64 = AtomicU64::new(0);
static SEARCH_COUNT: AtomicU64 = AtomicU64::new(0);
static SUGGEST_COUNT: AtomicU64 = AtomicU64::new(0);

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
        // Check whether the Latin pattern matches the Tamil
        let matches = Pattern::parse(latin)
            .expect("invalid pattern")
            .search(WordSearch::new(tamil), false, true)
            .unwrap()
            .end()
            .unwrap();

        if !matches {
            panic!("pattern {:?} does not match {:?}", latin, tamil);
        }

        Self {
            latin,
            tamil: tamil.to_string().into_boxed_str(),
        }
    }
}

pub fn current_example() -> &'static Example {
    #[rustfmt::skip]
    lazy_static! {
        static ref EXAMPLES: Box<[Example]> = {
            let examples = &[
                ("appuram",    word![A, P, P, U, AlveolarR, A, M]),
                ("aayiram",    word![LongA, Y, I, R, A, M]),
                ("ippadi",     word![I, P, P, A, RetroT, I]),
                ("ishtam",     word![I, Sh, RetroT, A, M]),
                ("eeram",      word![LongI, R, A, M]),
                ("utkaar",     word![U, RetroT, K, LongA, R]),
                ("ulagam",     word![U, AlveolarL, A, K, A, M]),
                ("ellaam",     word![E, AlveolarL, AlveolarL, LongA, M]),
                ("onbadhu",    word![O, AlveolarN, P, A, T, U]),
                ("kadhai",     word![K, A, T, Ai]),
                ("kaalam",     word![K, LongA, AlveolarL, A, M]),
                ("kaatru",     word![K, LongA, AlveolarR, AlveolarR, U]),
                ("koottam",    word![K, LongU, RetroT, RetroT, A, M]),
                ("kooppidu",   word![K, LongU, P, P, I, RetroT, U]),
                ("konduvaa",   word![K, O, RetroN, RetroT, U, V, LongA]),
                ("sattendru",  word![Ch, A, RetroT, RetroT, E, AlveolarN, AlveolarR, U]),
                ("sandhosham", word![Ch, A, N, T, LongO, Sh, A, M]),
                ("nyaabagam",  word![Ny, LongA, P, A, K, A, M]),
                ("thangam",    word![T, A, Ng, K, A, M]),
                ("thamizh",    word![T, A, M, I, Zh]),
                ("nenjam",     word![N, E, Ny, Ch, A, M]),
                ("pandhu",     word![P, A, N, T, U]),
                ("pazham",     word![P, A, Zh, A, M]),
                ("puthagam",   word![P, U, T, T, A, K, A, M]),
                ("mazhai",     word![M, A, Zh, Ai]),
                ("maunam",     word![M, Au, AlveolarN, A, M]),
                ("yaanai",     word![Y, LongA, AlveolarN, Ai]),
                ("vanakkam",   word![V, A, RetroN, A, K, K, A, M]),
                ("vinyaanam",  word![V, I, Ny, Ny, LongA, AlveolarN, A, M]),
                ("veedham",    word![V, LongI, T, A, M]),
            ];

            examples.iter().map(Example::new).collect()
        };

        static ref INDICES: Box<[u8]> = {
            let count = EXAMPLES.len();
            assert!(count <= u8::MAX as usize);

            let mut indices = Vec::with_capacity(count * EXAMPLE_CYCLE_PERIOD);

            let mut rng = rand::thread_rng();
            for _ in 0..EXAMPLE_CYCLE_PERIOD {
                let mut slice: Box<[u8]> = (0..count).map(|i| i as u8).collect();
                slice.shuffle(&mut rng);
                indices.extend_from_slice(&slice);
            }

            indices.into_boxed_slice()
        };
    }

    let uptime = crate::uptime();
    let num_refreshes = uptime.as_secs() / EXAMPLE_REFRESH_TIME_SECS;
    let index = INDICES[num_refreshes as usize % INDICES.len()];
    &EXAMPLES[index as usize]
}

#[derive(Serialize, Debug)]
struct IndexTemplate {
    example: &'static Example,
}

#[get("/")]
pub fn index() -> Template {
    let example = current_example();
    render_template("index", IndexTemplate { example })
}

#[get("/grammar")]
pub fn grammar() -> Template {
    render_template("grammar", ())
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

impl<'a> From<&'a QueryKindSet> for KindSet {
    #[rustfmt::skip]
    fn from(kinds: &'a QueryKindSet) -> Self {
        let &QueryKindSet { v, va, vm, tv, p, pa, sp, vp, i, ii } = kinds;

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
            .reduce(std::ops::BitOr::bitor)
            .unwrap_or(KindSet::empty())
    }
}

fn link(word: &str) -> String {
    link_no_escape(&Query::escape(word))
}

fn link_no_escape(escaped: &str) -> String {
    uri!(search(escaped, _)).to_string()
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

    fn render(state: &mut RenderState, seg: &'static Segment) -> Vec<Self> {
        let entry = state.entry;
        let mut segments = Vec::new();
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
        let segments = para
            .iter()
            .flat_map(|seg| ResultSegment::render(state, seg))
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
    exact: bool,
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

    #[rustfmt::skip]
    fn render(entry: SearchRankingEntry, expanded: bool) -> Self {
        let SearchRankingEntry { entry, words, exact } = entry;

        let sections = if expanded || exact || words.len() != 1 || !words.contains(&NO_WORD) {
            ResultSection::render_all(entry, words)
        } else {
            vec![ResultSection::collapsed(entry)]
        };

        let mut kind = entry.kind_strs.join("/");

        if let Some(hint) = &entry.hint {
            kind = format!("{} {}", hint, kind);
        }

        Self {
            uri: link(entry.primary_word()),
            word: &entry.word,
            exact,
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

#[derive(Serialize, Debug)]
struct SearchTemplate {
    query: String,
    kinds: QueryKindSet,
    other_uri: String,
    hide_other: bool,
    error: bool,
    def_uri: Option<String>,
    message: Option<String>,
    best: Vec<ResultEntry>,
    related: Vec<ResultEntry>,
    other: Vec<ResultEntry>,
    best_count: NumWithPlural,
    related_count: NumWithPlural,
    other_count: NumWithPlural,
}

impl SearchTemplate {
    fn new(query: &str, kinds: QueryKindSet, all: bool) -> Self {
        let query = query.trim();
        let other_uri = if all {
            String::new()
        } else {
            uri!(search_all(query, &kinds)).to_string()
        };

        Self {
            query: String::from(query),
            kinds,
            other_uri,
            hide_other: !all,
            error: false,
            def_uri: None,
            message: None,
            best: Vec::new(),
            related: Vec::new(),
            other: Vec::new(),
            best_count: 0.into(),
            related_count: 0.into(),
            other_count: 0.into(),
        }
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
        match query.search((&self.kinds).into()) {
            Err(err) => self.error(err),
            Ok((result, kind)) => {
                // Convert the search results to a ranked page
                let ranking = result.rank();
                if ranking.is_empty() {
                    self.message("No results found.");
                } else {
                    // Count how many of each result kind there are
                    self.best_count = ranking.best.len().into();
                    self.related_count = ranking.related.len().into();
                    self.other_count = ranking.other.len().into();

                    // Render the search results so they can be displayed
                    let mut total = self.best_count.num;
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
                            if !ranking.good_search && query.implicit_transliteration() {
                                self.def_uri = Some(
                                    uri!(search(format!(":{}", self.query), &self.kinds))
                                        .to_string(),
                                );
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
}

#[get("/random")]
pub fn random() -> Template {
    let word = Query::escape(Entry::random().primary_word());
    search_query(&word, QueryKindSet::default(), false)
}

#[get("/search?all&<q>&<k..>")]
pub fn search_all(q: &str, k: QueryKindSet) -> Template {
    search_query(q, k, true)
}

#[get("/search?<q>&<k..>")]
pub fn search(q: &str, k: QueryKindSet) -> Template {
    search_query(q, k, false)
}

#[get("/search")]
pub fn search_no_query() -> Redirect {
    Redirect::to(uri!(index()))
}

fn search_query(q: &str, k: QueryKindSet, all: bool) -> Template {
    SEARCH_COUNT.fetch_add(1, Ordering::Relaxed);

    let mut search = SearchTemplate::new(q, k, all);
    match Query::parse(&search.query) {
        Err(err) => search.error(err),
        Ok(query) => search.search(query),
    }

    render_template("search", search)
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
    if let Ok(parsed_query) = Query::parse(&query) {
        if let Some(mut pat) = parsed_query.into_pattern() {
            let mut count = n.min(100);

            // If there is implicit transliteration, reserve a row for definition search
            let add_definition = count > 3 && pat.implicit_transliteration();
            if add_definition {
                count -= 1;
            }

            // Make the pattern more general if there was a trailing "a"
            if append_a {
                pat = Pattern::Concat(
                    Box::new(pat),
                    Box::new(Pattern::Alternative(
                        Box::new(Pattern::Assert(LetterSet::vowel())),
                        Box::new(Pattern::MarkExpanded),
                    )),
                );
            }

            if let Some(list) = pat.suggest(count) {
                let mut suggestions: Vec<_> =
                    list.suggestions().map(SuggestResponseEntry::from).collect();

                // Use ":" to indicate this last row should be used for definition search
                if add_definition {
                    let completion = format!(":{}", q);
                    suggestions.push(SuggestResponseEntry {
                        word: ":",
                        uri: uri!(search(&completion, _)).to_string(),
                        completion,
                    });
                }

                return Json(suggestions);
            }
        }
    }

    Json(Vec::new())
}

#[get("/api/stats")]
pub fn stats() -> String {
    let secs = crate::uptime().as_secs();
    let (mins, secs) = (secs / 60, secs % 60);
    let (hours, mins) = (mins / 60, mins % 60);

    let result_count = RESULT_COUNT.load(Ordering::Acquire);
    let search_count = SEARCH_COUNT.load(Ordering::Acquire);
    let suggest_count = SUGGEST_COUNT.load(Ordering::Acquire);

    format!(
        concat!(
            "uptime={:}:{:02}:{:02}\n",
            "version={}\n",
            "result_count={}\n",
            "search_count={}\n",
            "suggest_count={}\n",
        ),
        hours,
        mins,
        secs,
        crate::version(),
        result_count,
        search_count,
        suggest_count,
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
