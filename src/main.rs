#[macro_use]
extern crate rocket;

use std::hash::BuildHasherDefault;
use std::time::{Duration, Instant};

use once_cell::sync::OnceCell;

use rocket::fs::{FileServer, relative};
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
    ($($tt:tt)*) => {
        (&{
            #[allow(unused_imports)]
            use $crate::tamil::Letter::*;
            [$($tt)*]
        })[..].into()
    };
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

// The path the server hosts resources on.
pub const SERVER_RESOURCE_PATH: &str = "/res";

pub fn uptime() -> Duration {
    static START: OnceCell<Instant> = OnceCell::new();

    let &start = START.get_or_init(Instant::now);
    Instant::now().saturating_duration_since(start)
}

pub fn resource_path() -> &'static str {
    static INSTANCE: OnceCell<Box<str>> = OnceCell::new();

    INSTANCE.get_or_init(|| {
        if let Ok(path) = std::env::var("RESOURCE_PATH")
            && !path.is_empty()
        {
            assert!(!path.ends_with("/"));
            path.into_boxed_str()
        } else {
            Box::from(SERVER_RESOURCE_PATH)
        }
    })
}

#[launch]
async fn rocket() -> _ {
    // Initialize the examples for the front page
    web::current_example();

    // Start building the word, definition, and stem data structures
    task::spawn_blocking(|| {
        let _ = annotate::supported();
        let _ = search::tree::search_word();
        let _ = search::tree::search_definition();
    });

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
                web::health,
                web::info,
            ],
        )
        .mount(SERVER_RESOURCE_PATH, FileServer::from(relative!("res")))
        .register("/", catchers![web::error])
        .attach(Template::fairing())
}
