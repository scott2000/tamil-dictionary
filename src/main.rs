use std::hash::BuildHasherDefault;
use std::time::{Duration, Instant};

use once_cell::sync::OnceCell;

use seahash::SeaHasher;

use dictionary::*;
use std::collections::*;
use std::fs::File;
use std::io::{self, BufWriter, Write};
use tamil::*;

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

fn freqs(entries: &[Entry]) -> io::Result<()> {
    println!("Counting letters...");

    let mut file = BufWriter::new(File::create("freqs.csv")?);

    let mut map = BTreeMap::new();
    for entry in entries.iter() {
        for word in &*entry.parsed_text {
            let mut last = None;
            for letter in word.iter() {
                let (count, no_double) = map.get(&letter).copied().unwrap_or((0, 0));
                let no_double = no_double + (last != Some(letter)) as u32;
                map.insert(letter, (count + 1, no_double));
                last = Some(letter);
            }
        }
    }

    writeln!(file, r#""letter","count","no_double","kind""#)?;
    for (letter, (count, no_double)) in map {
        let kind = match letter.category() {
            Category::LatinAlpha => "latin",
            Category::TamilGrantha => "grantha",
            _ => "tamil",
        };
        writeln!(file, "\"{}\",{},{},\"{}\"", letter, count, no_double, kind)?;
    }

    Ok(())
}

fn write_letter(file: &mut BufWriter<File>, letter: Option<Letter>) -> io::Result<()> {
    if let Some(letter) = letter {
        write!(file, "\"{:?}\",", letter)
    } else {
        write!(file, "\"$\",")
    }
}

fn bigrams(entries: &[Entry]) -> io::Result<()> {
    println!("Counting bigrams...");

    let mut file = BufWriter::new(File::create("bigrams.csv")?);

    let mut map = BTreeMap::new();
    for entry in entries.iter() {
        for word in &*entry.parsed_text {
            let mut last = None;
            for letter in word.iter().map(Some).chain(std::iter::once(None)) {
                let key = (last, letter);
                let count = map.get(&key).copied().unwrap_or(0);
                map.insert(key, count + 1);
                last = letter;
            }
        }
    }

    writeln!(file, r#""left","right","count""#)?;
    for (&(left, right), count) in map.iter() {
        write_letter(&mut file, left)?;
        write_letter(&mut file, right)?;
        writeln!(file, "{}", count)?;
    }

    drop(file);

    let mut file = BufWriter::new(File::create("clusters.csv")?);
    let mut key_file = BufWriter::new(File::create("clusters.key.csv")?);

    const CONSONANTS: LetterSet = LetterSet::consonant().difference(LetterSet::grantha());

    for left in CONSONANTS.iter() {
        for right in CONSONANTS.iter() {
            let count = map.get(&(Some(left), Some(right))).copied().unwrap_or(0);

            if right != Letter::K {
                write!(file, ",")?;
                write!(key_file, ",")?;
            }

            write!(file, "{}", count)?;
            write!(key_file, "\"{}{}\"", left, right)?;
        }

        writeln!(file)?;
        writeln!(key_file)?;
    }

    Ok(())
}

fn main() -> io::Result<()> {
    let entries: &[Entry] = entries();
    freqs(entries)?;
    bigrams(entries)?;
    Ok(())
}
