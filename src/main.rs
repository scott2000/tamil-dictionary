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

pub mod annotate;
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

#[launch]
#[rustfmt::skip]
async fn rocket() -> _ {
    // Initialize the examples for the front page
    web::current_example();

    // Start building the word and definition trees immediately
    task::spawn_blocking(search::tree::search_word);
    task::spawn_blocking(search::tree::search_definition);

    // Host resources at a path including the version number
    let res_path = format!("/res/{}", version());

    rocket::build()
        .mount(
            "/",
            routes![
                web::index,
                web::advanced,
                web::grammar,
                web::entries,
                web::random,
                web::search_all,
                web::search,
                web::search_no_query,
                web::suggest,
                web::stats,
            ],
        )
        .mount(res_path, FileServer::from(relative!("res")))
        .register("/", catchers![web::error])
        .attach(Template::fairing())
}
