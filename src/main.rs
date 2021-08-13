#[macro_use]
extern crate rocket;
#[macro_use]
extern crate lazy_static;

#[doc(hidden)]
macro_rules! letterset_impl {
    ([$n:expr]) => { $n };
    ([$n:expr], ) => { $n };
    ([$n:expr], $lt:ident $($tt:tt)*) => {
        letterset_impl!([$n | (1 << $crate::tamil::num::$lt)] $($tt)*)
    };
}

macro_rules! letterset {
    ($($tt:tt)*) => {{
        const _LETTER_SET: $crate::tamil::LetterSet = $crate::tamil::LetterSet(letterset_impl!([0], $($tt)*));
        _LETTER_SET
    }};
}

macro_rules! word {
    ($($tt:tt)*) => {
        [$($tt)*][..].into()
    };
}

use rocket::fs::{relative, FileServer};
use rocket_dyn_templates::Template;

use tokio::task;

pub mod dictionary;
pub mod intern;
pub mod query;
pub mod search;
pub mod tamil;
pub mod web;

use search::tree;

#[rocket::main]
#[rustfmt::skip]
async fn main() -> Result<(), rocket::Error> {
    let rocket_handle = tokio::spawn(async {
        rocket::build()
            .mount(
                "/",
                routes![
                    web::index,
                    web::search_all,
                    web::search,
                    web::search_empty_query,
                    web::search_no_query,
                    web::suggest,
                ],
            )
            .mount("/resources", FileServer::from(relative!("resources")))
            .attach(Template::fairing())
            .launch()
            .await
    });

    let word_handle = task::spawn_blocking(tree::search_word);
    let definition_handle = task::spawn_blocking(tree::search_definition);

    word_handle.await.expect("failed to build word trees");
    definition_handle.await.expect("failed to build definition trees");
    rocket_handle.await.expect("rocket panicked")
}
