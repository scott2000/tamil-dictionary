use std::mem;
use std::sync::Mutex;
use std::collections::HashSet;

use crate::dictionary::Word;

lazy_static! {
    static ref WORDS: Mutex<HashSet<&'static Word>> = Mutex::new(HashSet::new());
}

pub fn word(word: Box<Word>) -> &'static Word {
    // Cast the word reference to a pointer
    let ptr = word.as_ref() as *const Word;

    // Get any existing interned static copy
    let mut words = WORDS.lock().expect("cannot lock mutex");
    let existing = unsafe {
        words.get(&(&*ptr)).cloned()
    };

    if let Some(word) = existing {
        // Return the existing static copy of the word
        word
    } else {
        // Leak the word so that it is available statically
        let word = Box::leak(word);

        // Add the word and every suffix of the word
        for i in 0..word.len() {
            words.insert(&word[i..]);
        }

        word
    }
}

pub fn done() {
    let mut words = WORDS.lock().expect("cannot lock mutex");
    mem::swap(&mut *words, &mut HashSet::new());
}
