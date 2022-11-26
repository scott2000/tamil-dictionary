use std::collections::BTreeSet;
use std::sync::Mutex;

use once_cell::sync::OnceCell;

use crate::tamil::Word;

static WORDS: OnceCell<Mutex<BTreeSet<&'static Word>>> = OnceCell::new();

pub fn word(word: Box<Word>) -> &'static Word {
    // Cast the word reference to a pointer
    let ptr = word.as_ref() as *const Word;

    // Get a lock on the WORDS mutex
    let mut words = WORDS
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .expect("cannot lock mutex");

    // Get any existing interned static copy
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
    if let Some(mutex) = WORDS.get() {
        let mut words = mutex.lock().expect("cannot lock mutex");
        *words = BTreeSet::new();
    }
}
