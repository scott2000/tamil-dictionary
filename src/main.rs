#[macro_use] extern crate rocket;
#[macro_use] extern crate lazy_static;

use rocket::fs::{FileServer, relative};
use rocket_dyn_templates::Template;

use tokio::task;

pub mod tamil;
pub mod intern;
pub mod dictionary;
pub mod search;
pub mod query;
pub mod web;

use search::tree;

#[rocket::main]
async fn main() -> Result<(), rocket::Error> {
    let rocket_handle = tokio::spawn(async {
        rocket::build()
            .mount("/", routes![
                web::index,
                web::search_all,
                web::search,
                web::search_empty,
            ])
            .mount("/", FileServer::from(relative!("static")))
            .attach(Template::fairing())
            .launch()
            .await
    });

    let word_handle = task::spawn_blocking(|| {
        tree::search_word();
    });

    let definition_handle = task::spawn_blocking(|| {
        tree::search_definition();
    });

    word_handle.await.unwrap();
    definition_handle.await.unwrap();
    rocket_handle.await.unwrap()
}
