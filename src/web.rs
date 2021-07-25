use std::iter;

use serde::Serialize;

use rocket::response::Redirect;
use rocket_dyn_templates::Template;

use crate::dictionary::{NO_WORD, Entry, Section, Paragraph, Segment, SegmentKind};
use crate::search::{SearchResult, SearchError, SearchRankingEntry};
use crate::query::Query;

const MAX_OTHER_WORDS: usize = 5;

#[get("/")]
pub fn index() -> Template {
    Template::render("index", ())
}

fn link_alts<'a>(words: impl Iterator<Item = &'a str>) -> String {
    let mut buffer = String::new();
    for word in words {
        if !buffer.is_empty() {
            buffer.push_str(" | ");
        }

        buffer.push_str(&Query::escape(word));
    }

    if buffer.starts_with('-') || buffer.contains(' ') {
        buffer.insert(0, '(');
        buffer.push(')');
    }

    uri!(search(buffer)).to_string()
}

fn link(word: &str) -> String {
    link_alts(iter::once(word))
}

#[derive(Debug)]
struct RenderState {
    entry: &'static Entry,
    definition_count: usize,
    highlight_ranges: Vec<(u32, u32)>,
}

#[derive(Serialize, Debug)]
struct ResultSegment {
    text: &'static str,
    uri: String,
    tag: Option<&'static str>,
}

impl ResultSegment {
    fn new(kind: SegmentKind, text: &'static str) -> Self {
        let (uri, tag) = match kind {
            SegmentKind::Text => (String::new(), None),
            SegmentKind::Reference => (link(text), None),
            SegmentKind::Superscript => (String::new(), Some("sup")),
            SegmentKind::Bold => (String::new(), Some("strong")),
        };

        Self {
            text,
            uri,
            tag,
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

        let mut start = seg.start as usize;
        let end = seg.end as usize;
        while let Some(&(range_start, range_end)) = state.highlight_ranges.last() {
            let range_start = range_start as usize;
            let range_end = range_end as usize;

            if range_end > end {
                break;
            }

            state.highlight_ranges.pop();
            push(seg.kind, start, range_start);
            push(SegmentKind::Bold, range_start, range_end);
            start = range_end;
        }

        push(seg.kind, start, end);
        segments
    }
}

#[derive(Serialize, Default, Debug)]
struct ResultParagraph {
    segments: Vec<ResultSegment>,
}

impl ResultParagraph {
    fn render(state: &mut RenderState, para: &'static Paragraph) -> Self {
        let segments = para.iter()
            .flat_map(|seg| ResultSegment::render(state, seg))
            .collect();

        Self {
            segments,
        }
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
    fn render(state: &mut RenderState, is_header: bool, sec: &'static Section) -> Self {
        let definition_start = state.definition_count;

        let mut iter = sec.iter()
            .map(|para| ResultParagraph::render(state, para));

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
    subword: Option<u8>,
    sections: Vec<ResultSection>,
}

impl ResultEntry {
    fn render_all(results: Vec<SearchRankingEntry>) -> Vec<Self> {
        results.into_iter()
            .map(Self::render)
            .collect()
    }

    fn render(SearchRankingEntry { entry, words }: SearchRankingEntry) -> Self {
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

        let mut sections: Vec<_> = entry.sections.iter()
            .enumerate()
            .map(|(i, sec)| ResultSection::render(&mut state, i == 0, sec))
            .collect();

        if sections.len() == 1 {
            let only = &mut sections[0];
            if only.paragraphs.len() == 1 {
                only.section.segments.push(ResultSegment::new(SegmentKind::Text, ": "));
                only.section.segments.append(&mut only.paragraphs[0].segments);
                only.paragraphs = Vec::new();
            }
        }

        Self {
            uri: link_alts(entry.words()),
            word: &entry.word,
            subword: entry.subword,
            sections,
        }
    }
}

#[derive(Serialize, Debug)]
struct Search {
    query: String,
    other_uri: String,
    error_message: Option<String>,
    definition_uri: Option<String>,
    best_or_exact: bool,
    exact: Vec<ResultEntry>,
    best: Vec<ResultEntry>,
    related: Vec<ResultEntry>,
    hide_other: bool,
    other_count: usize,
    other: Vec<ResultEntry>,
}

impl Search {
    fn new(query: &str, all: bool) -> Self {
        let query = query.trim();
        let other_uri = if all {
            String::new()
        } else {
            uri!(search_all(query)).to_string()
        };

        Self {
            query: String::from(query),
            other_uri,
            error_message: None,
            definition_uri: None,
            best_or_exact: false,
            exact: Vec::new(),
            best: Vec::new(),
            related: Vec::new(),
            hide_other: !all,
            other_count: 0,
            other: Vec::new(),
        }
    }

    fn error(&mut self, err: impl ToString) {
        self.error_message = Some(err.to_string());
    }

    fn no_results(&mut self) {
        self.error("No results found.");
    }

    fn result(&mut self, result: Result<SearchResult, SearchError>) {
        match result {
            Err(SearchError::TryDefinition) => {
                self.no_results();
                let new_query = format!(": {}", self.query);
                self.definition_uri = Some(if self.hide_other {
                    uri!(search(new_query))
                } else {
                    uri!(search_all(new_query))
                }.to_string());
            },
            Err(err) => self.error(err),
            Ok(result) => {
                let ranking = result.rank();
                if ranking.is_empty() {
                    self.no_results();
                } else {
                    self.exact = ResultEntry::render_all(ranking.exact);
                    self.best = ResultEntry::render_all(ranking.best);
                    self.related = ResultEntry::render_all(ranking.related);
                    self.other = ResultEntry::render_all(ranking.other);

                    self.best_or_exact = !self.exact.is_empty() || !self.best.is_empty();
                }

                self.other_count = self.other.len();
                if self.other_count <= MAX_OTHER_WORDS {
                    self.hide_other = false;
                }
            },
        }
    }
}

#[get("/search?<q>&all")]
pub fn search_all(q: &str) -> Template {
    search_query(q, true)
}

#[get("/search?<q>")]
pub fn search(q: &str) -> Template {
    search_query(q, false)
}

#[get("/search")]
pub fn search_empty() -> Redirect {
    Redirect::to(uri!(index()))
}

fn search_query(q: &str, all: bool) -> Template {
    let mut search = Search::new(q, all);
    match Query::parse(&search.query) {
        Err(err) => search.error(err),
        Ok(query) => search.result(query.search()),
    }

    Template::render("search", search)
}
