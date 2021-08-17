#[macro_use]
extern crate rocket;
#[macro_use]
extern crate lazy_static;

use std::time::{Duration, Instant};

use rocket::fs::{relative, FileServer};
use rocket_dyn_templates::Template;

use tokio::task;

#[doc(hidden)]
macro_rules! letterset_impl {
    ([$n:expr]) => { $n };
    ([$n:expr] $lt:expr, $($tt:tt)*) => {
        letterset_impl!([$n | (1 << ($lt) as u8)] $($tt)*)
    };
}

macro_rules! letterset {
    ($($lt:expr),* $(,)?) => {{
        #[allow(unused_imports)]
        use $crate::tamil::Letter::*;
        $crate::tamil::LetterSet(letterset_impl!([0] $($lt,)*))
    }};
}

macro_rules! word {
    ($($tt:tt)*) => {{
        #[allow(unused_imports)]
        use $crate::tamil::Letter::*;
        [$($tt)*][..].into()
    }};
}

pub mod dictionary;
pub mod intern;
pub mod query;
pub mod search;
pub mod tamil;
pub mod web;

pub fn uptime() -> Duration {
    lazy_static! {
        static ref START: Instant = Instant::now();
    }

    Instant::now().saturating_duration_since(*START)
}

pub fn version() -> &'static str {
    lazy_static! {
        static ref VERSION: Box<str> = {
            if let Ok(version) = std::env::var("RES_VERSION") {
                assert!(!version.is_empty());
                assert!(version.chars().all(|ch| ch.is_ascii_alphanumeric()));
                version.into_boxed_str()
            } else {
                Box::from("dev")
            }
        };
    }

    &VERSION
}

#[rocket::main]
#[rustfmt::skip]
async fn main() -> Result<(), rocket::Error> {
    // Initialize the examples for the front page
    web::current_example();

    let rocket_handle = tokio::spawn(async {
        // Host resources at a path including the version number
        let res_path = format!("/res/{}", version());

        rocket::build()
            .mount(
                "/",
                routes![
                    web::index,
                    web::grammar,
                    web::search_all,
                    web::search,
                    web::search_empty_query,
                    web::search_no_query,
                    web::suggest,
                    web::stats,
                ],
            )
            .mount(res_path, FileServer::from(relative!("res")))
            .attach(Template::fairing())
            .launch()
            .await
    });

    let word_handle = task::spawn_blocking(search::tree::search_word);
    let definition_handle = task::spawn_blocking(search::tree::search_definition);

    word_handle.await.expect("failed to build word trees");
    definition_handle.await.expect("failed to build definition trees");
    rocket_handle.await.expect("rocket panicked")
}
