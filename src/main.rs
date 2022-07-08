#[macro_use]
extern crate rocket;

use std::hash::BuildHasherDefault;
use std::time::{Duration, Instant};

use itertools::Itertools;

use once_cell::sync::OnceCell;

use seahash::SeaHasher;

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
    static START: OnceCell<Instant> = OnceCell::new();

    let &start = START.get_or_init(Instant::now);
    Instant::now().saturating_duration_since(start)
}

pub fn version() -> &'static str {
    static INSTANCE: OnceCell<Box<str>> = OnceCell::new();

    INSTANCE.get_or_init(|| {
        if let Ok(version) = std::env::var("RES_VERSION") {
            assert!(!version.is_empty());
            assert!(version.chars().all(|ch| ch.is_ascii_alphanumeric()));
            version.into_boxed_str()
        } else {
            Box::from("dev")
        }
    })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use annotate::*;
    use std::collections::BTreeSet;
    use std::fs::File;
    use std::io::prelude::*;
    use std::io::BufReader;

    let mut args = std::env::args();
    let _ = args.next();
    let path = args.next().expect("no file provided");
    let min_count = args
        .next()
        .map(|s| s.parse().expect("invalid minimum count"))
        .unwrap_or(2);

    let file = BufReader::new(File::open(path)?);

    let mut lines = file.lines().peekable();
    let (exclude, _) = if let Some(Ok(line)) = lines.peek() {
        TextSegment::strip_exclude(line)
    } else {
        eprintln!("empty file!");
        return Ok(());
    };

    if !exclude.is_empty() {
        eprintln!("excluded words: {}", exclude.len());
    }

    let mut count = WordCount::default();
    let mut unknown = BTreeSet::new();
    let mut progress = 0u64;
    for line in lines {
        let line = line?;

        for seg in TextSegment::parse(&line).flat_map(|seg| seg.group_excluding(&exclude)) {
            if let TextSegment::Tamil(word, None) = seg {
                unknown.insert(word);
                continue;
            }

            count.insert_segment(&seg);

            progress += 1;
            if progress % 1_000_000 == 0 {
                eprintln!("parsed {}M words...", progress / 1_000_000);
            }
        }
    }

    let definition_lengths = dictionary::entries()
        .iter()
        .into_grouping_map_by(|entry| entry.primary_word())
        .fold(0, |acc, _, entry| acc + entry.parsed_text.len());

    println!(r#""word","count","definition_length""#);

    for (word, count) in count.into_vec(min_count) {
        println!(
            r#""{word}",{count},{}"#,
            definition_lengths.get(word).copied().unwrap_or(0)
        );
    }

    if !unknown.is_empty() {
        eprintln!("unknown words: {}", unknown.len());
    }

    Ok(())
}
