#[macro_use]
extern crate rocket;

use std::hash::BuildHasherDefault;
use std::time::{Duration, Instant};

use once_cell::sync::OnceCell;

use rocket::fs::{FileServer, relative};
use rocket::http::uri::Origin;
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

pub fn base_path() -> &'static str {
    static INSTANCE: OnceCell<Box<str>> = OnceCell::new();

    INSTANCE.get_or_init(|| {
        if let Ok(path) = std::env::var("BASE_PATH") {
            assert!(!path.ends_with("/"));
            path.into_boxed_str()
        } else {
            Box::from("")
        }
    })
}

pub fn origin() -> Origin<'static> {
    static INSTANCE: OnceCell<Origin<'static>> = OnceCell::new();

    INSTANCE
        .get_or_init(|| {
            let with_slash = format!("{}/", base_path()).leak();
            Origin::parse(with_slash).expect("base path is valid origin")
        })
        .clone()
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
            format!("{}{SERVER_RESOURCE_PATH}", base_path()).into_boxed_str()
        }
    })
}

#[launch]
async fn rocket() -> _ {
    // Initialize the examples for the front page
    web::current_example();

    // Build the definition data structures in the background
    task::spawn_blocking(|| {
        let _ = search::tree::search_definition();
    });

    // Build the word and annotation structures immediately
    let word_task = task::spawn_blocking(|| {
        let _ = search::tree::search_word();
    });
    let annotation_task = task::spawn_blocking(|| {
        let _ = annotate::supported();
    });

    let mut builder = rocket::build()
        .mount(
            origin(),
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
                web::word_of_the_day,
                // API endpoints
                web::annotate_api_get,
                web::annotate_top_get,
                web::annotate_raw_get,
                web::annotate_api,
                web::annotate_top,
                web::annotate_raw,
                web::suggest,
                web::word_of_the_day_api,
                // Other endpoints
                web::health,
                web::health_post,
                web::info,
            ],
        )
        .register("/", catchers![web::error])
        .attach(Template::fairing());

    // Only mount the resource path if it's hosted by this server
    if resource_path().starts_with("/") {
        builder = builder.mount(resource_path(), FileServer::from(relative!("res")));
    }

    // Wait for the word and annotation tasks to finish
    word_task
        .await
        .expect("word trees should build successfully");
    annotation_task
        .await
        .expect("annotation structures should build successfully");

    builder
}
