use std::iter;

use serde::Serialize;

use rocket::response::Redirect;
use rocket_dyn_templates::Template;

use crate::dictionary::{NO_WORD, Entry, Section, Paragraph, Segment, SegmentKind};
use crate::search::{SearchResult, SearchError, SearchRankingEntry};
use crate::query::Query;

const MAX_OTHER_SECTIONS: usize = 5;

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

        // Highlight any words which appear in this segment
        let mut start = seg.start as usize;
        let end = seg.end as usize;
        while let Some(&(range_start, range_end)) = state.highlight_ranges.last() {
            let range_start = range_start as usize;
            let range_end = range_end as usize;

            if range_end > end {
                break;
            }

            // Split the segment into parts to add a bold segment
            state.highlight_ranges.pop();
            push(seg.kind, start, range_start);
            push(SegmentKind::Bold, range_start, range_end);
            start = range_end;
        }

        // Push any remaining text in the segment
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
        let mut sections: Vec<_> = entry.sections.iter()
            .enumerate()
            .map(|(i, sec)| ResultSection::render(&mut state, i == 0, sec))
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

        Self {
            uri: link_alts(entry.words()),
            word: &entry.word,
            subword: entry.subword,
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
    fn from(num: usize) -> Self  {
        Self {
            num,
            is_plural: num != 1,
        }
    }
}

#[derive(Serialize, Debug)]
struct Search {
    query: String,
    other_uri: String,
    hide_other: bool,
    error: bool,
    message: Option<String>,
    exact: Vec<ResultEntry>,
    best: Vec<ResultEntry>,
    related: Vec<ResultEntry>,
    other: Vec<ResultEntry>,
    best_count: NumWithPlural,
    related_count: NumWithPlural,
    other_count: NumWithPlural,
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
            hide_other: !all,
            error: false,
            message: None,
            exact: Vec::new(),
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

    fn no_results(&mut self) {
        if !self.error {
            self.message = Some(String::from("No results found."));
        }
    }

    fn result(&mut self, result: Result<SearchResult, SearchError>) {
        match result {
            Err(err) => self.error(err),
            Ok(result) => {
                // Convert the search results to a ranked page
                let ranking = result.rank();
                if ranking.is_empty() {
                    self.no_results();
                } else {
                    // Render the search results so they can be displayed
                    self.exact = ResultEntry::render_all(ranking.exact);
                    self.best = ResultEntry::render_all(ranking.best);
                    self.related = ResultEntry::render_all(ranking.related);
                    self.other = ResultEntry::render_all(ranking.other);

                    self.best_count = (self.exact.len() + self.best.len()).into();
                    self.related_count = self.related.len().into();
                    self.other_count = self.other.len().into();
                }

                // Only hide the other results if they are too long
                if self.other_count.num <= 1 {
                    self.hide_other = false;
                } else if self.other_count.num <= MAX_OTHER_SECTIONS {
                    // Count the number of sections in the other section
                    let other_section_count = self.other.iter()
                        .flat_map(|entry| entry.sections.iter())
                        .count();

                    if other_section_count <= MAX_OTHER_SECTIONS {
                        self.hide_other = false;
                    }
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
