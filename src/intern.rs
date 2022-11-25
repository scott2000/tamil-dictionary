use std::collections::BTreeSet;
use std::sync::Mutex;

use once_cell::sync::Lazy;

use crate::tamil::Word;

static WORDS: Lazy<Mutex<BTreeSet<&'static Word>>> = Lazy::new(|| Mutex::new(BTreeSet::new()));

pub fn word(word: Box<Word>) -> &'static Word {
    // Cast the word reference to a pointer
    let ptr = word.as_ref() as *const Word;

    // Get any existing interned static copy
    let mut words = WORDS.lock().expect("cannot lock mutex");
    let existing = unsafe {
        let word: &'static Word = &*ptr;

        words
            .range(word..)
            .next()
            .and_then(|&full_word| full_word.take_prefix(word))
    };

    if let Some(word) = existing {
        // Return the existing static copy of the word
        word
    } else {
        // Leak the word so that it is available statically
        let word = Box::leak(word);

        // Add the word to the set of interned words
        words.insert(word);

        word
    }
}

pub fn done() {
    let mut words = WORDS.lock().expect("cannot lock mutex");
    *words = BTreeSet::new();
}
