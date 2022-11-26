use std::{iter, sync::RwLock};

use once_cell::sync::OnceCell;

use crate::dictionary::{self, EntryIndex, Segment, SegmentKind};
use crate::search::{tree, Search};
use crate::tamil::Word;
use crate::HashMap;

pub fn get_entry(word: &'static str, sub: u8) -> Option<EntryIndex> {
    static REF_MAP: OnceCell<RwLock<HashMap<(&'static str, u8), EntryIndex>>> = OnceCell::new();

    let map = REF_MAP.get_or_init(|| RwLock::new(HashMap::default()));

    {
        // Try to find the value in the existing map
        let guard = map.read().unwrap();
        if let Some(&index) = guard.get(&(word, sub)) {
            return Some(index);
        }
    }

    let entries = dictionary::entries();

    // Find the index for the corresponding entry
    let index = tree::search_word_prefix()
        .literal(&Word::parse(word))
        .asserting_end()
        .end()
        .ok()?
        .entries()
        .find(|&entry| entries[entry as usize].subword == Some(sub))?;

    {
        // Insert the index into the map to cache it
        let mut guard = map.write().unwrap();
        guard.insert((word, sub), index);
    }

    Some(index)
}

#[derive(Copy, Clone, Debug)]
pub struct RefWithSub {
    pub ref_start: usize,
    pub ref_end: usize,
    pub sub_start: usize,
    pub sub_end: usize,
}

impl RefWithSub {
    pub fn get_parts<'a>(&self, text: &'a str) -> (&'a str, &'a str) {
        let word = &text[self.ref_start..self.ref_end];
        let sub = &text[self.sub_start..self.sub_end];

        (word, sub)
    }
}

pub fn group_refs<'a>(
    segs: impl Iterator<Item = &'a Segment>,
) -> impl Iterator<Item = Result<RefWithSub, &'a Segment>> {
    let mut segs = segs.peekable();

    iter::from_fn(move || {
        let seg = segs.next()?;

        if seg.kind() != SegmentKind::Reference {
            return Some(Err(seg));
        }

        let Some(&next) = segs.peek() else {
            return Some(Err(seg));
        };

        if next.kind() != SegmentKind::Superscript {
            return Some(Err(seg));
        }

        segs.next();
        Some(Ok(RefWithSub {
            ref_start: seg.start(),
            ref_end: seg.end(),
            sub_start: next.start(),
            sub_end: next.end(),
        }))
    })
}
