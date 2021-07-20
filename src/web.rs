use std::mem;

use serde::Serialize;

use rocket::response::Redirect;
use rocket_dyn_templates::Template;

use crate::dictionary::Entry;
use crate::search::{SearchResult, SearchError};
use crate::query::Query;

const MAX_OTHER_WORDS: usize = 5;

#[get("/")]
pub fn index() -> Template {
    Template::render("index", ())
}

#[derive(Serialize)]
struct ResultEntry<'a> {
    uri: String,
    word: &'a str,
    definition: &'a str,
}

impl<'a> ResultEntry<'a> {
    fn convert(vec: Vec<&'a Entry>) -> Vec<Self> {
        vec.into_iter()
            .map(ResultEntry::from)
            .collect()
    }
}

impl<'a> From<&'a Entry> for ResultEntry<'a> {
    fn from(entry: &'a Entry) -> Self {
        let mut buffer = String::new();
        for word in entry.words() {
            if !buffer.is_empty() {
                buffer.push_str(" | ");
            }

            buffer.push_str(&Query::escape(word));
        }

        if buffer.starts_with('-') || buffer.contains(' ') {
            buffer.insert(0, '(');
            buffer.push(')');
        }

        Self {
            uri: uri!(search(buffer)).to_string(),
            word: &entry.word,
            definition: &entry.definition,
        }
    }
}

#[derive(Serialize)]
struct Search {
    query: String,
    other_uri: String,
    error_message: Option<String>,
    definition_uri: Option<String>,
    best: Vec<ResultEntry<'static>>,
    related: Vec<ResultEntry<'static>>,
    hide_other: bool,
    other_count: usize,
    other: Vec<ResultEntry<'static>>,
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
                let mut best = result.exact();
                best.append(&mut result.prefix());

                self.best = ResultEntry::convert(best);
                self.related = ResultEntry::convert(result.affix());
                self.other = ResultEntry::convert(result.other());

                if self.best.is_empty() {
                    mem::swap(&mut self.best, &mut self.related);
                }

                if self.best.is_empty() {
                    mem::swap(&mut self.best, &mut self.other);
                }

                if self.best.is_empty() {
                    self.no_results();
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
