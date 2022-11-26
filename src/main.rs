#[macro_use]
extern crate rocket;

use std::hash::BuildHasherDefault;
use std::time::{Duration, Instant};

use once_cell::sync::Lazy;

use rocket::fs::{relative, FileServer};
use rocket_dyn_templates::Template;

use seahash::SeaHasher;

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
pub mod refs;
pub mod search;
pub mod tamil;
pub mod web;

pub type HashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<SeaHasher>>;
pub type HashSet<T> = std::collections::HashSet<T, BuildHasherDefault<SeaHasher>>;

pub fn uptime() -> Duration {
    static START: Lazy<Instant> = Lazy::new(Instant::now);

    Instant::now().saturating_duration_since(*START)
}

pub fn version() -> &'static str {
    static VERSION: Lazy<Box<str>> = Lazy::new(|| {
        if let Ok(version) = std::env::var("RES_VERSION") {
            assert!(!version.is_empty());
            assert!(version.chars().all(|ch| ch.is_ascii_alphanumeric()));
            version.into_boxed_str()
        } else {
            Box::from("dev")
        }
    });

    &VERSION
}

#[launch]
fn rocket() -> _ {
    // Initialize the examples for the front page
    web::current_example();

    // Start building the word, definition, and stem data structures
    task::spawn_blocking(|| {
        let _ = annotate::supported();
        let _ = search::tree::search_word();
        let _ = search::tree::search_definition();
    });

    // Host resources at a path including the version number
    let res_path = format!("/res/{}", version());

    rocket::build()
        .mount(
            "/",
            routes![
                // Index and other pages
                web::index,
                web::advanced,
                web::grammar,
                web::annotate,
                // Search pages
                web::entries,
                web::random,
                web::search_all,
                web::search,
                web::search_no_query,
                // API endpoints
                web::annotate_api_get,
                web::annotate_raw_get,
                web::annotate_api,
                web::annotate_raw,
                web::suggest,
                web::stats,
            ],
        )
        .mount(res_path, FileServer::from(relative!("res")))
        .register("/", catchers![web::error])
        .attach(Template::fairing())
}
