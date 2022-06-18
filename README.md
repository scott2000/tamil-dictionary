# Tamil Dictionary

This program hosts a Tamil dictionary website allowing fast searching in both
headwords and definitions. It uses a custom query syntax based on a restricted
subset of regular expressions designed specifically for Tamil text. It can
be configured to host any Tamil dictionary data, if it is in the correct format.

## Implementation

The web server and search engine are implemented in Rust using the Rocket web
framework and Handlebars for templating. The pages are written in HTML, CSS,
and vanilla JavsScript. The dictionary uses suffix trees to allow for O(n)
searches with respect to the length of the query string.

## Features

* Word and definition search
* Filter by part of speech
* Suggestions in search bar
* Ranking of results based on similarity to query
* True dictionary sort order (not based on Unicode order)
* Automatic transliteration (`maram` => `மரம்`)
* Awareness of grammar rules (`கடல்கரை` => `கடற்கரை`)
* Automatic light/dark theme depending on device settings
* Annotater which adds links to definitions for words (at `/annotate`)
* Random word search (at `/random`)

## Installation

Clone the repository:

```sh
git clone https://github.com/scott2000/tamil-dictionary.git
```

### Dictionary file

Create a file called `dictionary.json` with the contents of your dictionary
represented as a list of objects. In each object, there should be a `word` field
containing the headword (which may optionally contain commas separating multiple
words), an optional `sub` field containing the index of the subword if there are
multiple entries for the same word, an optional `hint` field containing text to
place at the start of the definition, a `kind` field indicating the part of
speech, and a list of sections.

The `kind` field contains an array of parts of speech which may be:

* `v` - வினைச்சொல் (verb)
* `va` - வினையடை (adverb)
* `vm` - வினைமுற்று (finite verb)
* `tv` - துணை வினை (auxiliary verb)
* `p` - பெயர்ச்சொல் (noun)
* `pa` - பெயரடை (adjective)
* `sp` - சுட்டுப் பெயர்ச்சொல் (pronoun)
* `spa` - சுட்டுப் பெயரடை (adjective of pronoun)
* `vp` - வினாப் பெயர்ச்சொல் (question word)
* `vpa` - வினாப் பெயரடை (adjective of question word)
* `i` - இடைச்சொல் (grammatical particle)
* `ii` - இணையிடைச்சொல் (conjunction)
* `vi` - விளி இடைச்சொல் (vocative)

Each section contains a list of paragraphs, with the first paragraph being
treated as a header for the section, and the rest being treated as numbered
definitions. A paragraph contains a list of segments, with each segment
containing one of `txt`, `ref`, `sup`, `bld`, `wbr` and a string. The `txt`
segment renders plain text, `ref` denotes a reference to another word in the
dictionary, `sup` creates a superscript, `bld` bolds text, and `wbr` inserts
`<wbr>` before text. Here is an example object:

```json
{
  "word": "மரம்",
  "sub": 1,
  "hint": "(common word)",
  "kind": ["p"],
  "secs": [
    [
      [
        ["txt", "Oblique: "],
        ["bld", "மரத்து"]
      ],
      [
        ["txt", "tree (see also: "],
        ["ref", "ஆலமரம்"],
        ["sup", "1"],
        ["txt", ")"]
      ],
      [
        ["txt", "wood"]
      ]
    ],
    [
      [
        ["txt", "Another section:"]
      ],
      [
        ["txt", "ThisIsAlongWordSoItMay"],
        ["wbr", "BeHelpfulToAddAWordBreak"]
      ]
    ]
  ]
}
```

Which will be rendered as:

<hr>
<div>
<p><strong>மரம்</strong><sup>1</sup> (common word) பெ. Oblique: <strong>மரத்து</strong></p>
<ol start="1">
<li>tree (see also: <a href="#">ஆலமரம்</a><sup>1</sup>)</li>
<li>wood</li>
</ol>
<p>Another section:</p>
<ol start="3">
<li>ThisIsAlongWordSoItMay<wbr>BeHelpfulToAddAWordBreak</li>
</ol>
</div>
<hr>

### Verbs file

There is an optional feature located at `/annotate` which takes some input text,
finds the definitions corresponding to each word in the text, and adds links to
those words. Since Tamil verbs can be conjugated in many different ways, this
feature requires a specification of how to conjugate each verb in the
dicitonary. This is done by providing all possible வினையெச்சம் forms of each verb
in a file called `verbs.json`. This file contains an array of objects. Here is
an example object:

```json
{
  "word": "வா",
  "sub": 1,
  "ve": ["வர", "வந்து"]
}
```

If there's a word which shares a conjugation with another verb, you may use
hyphens to indicate that only a partial conjugation was given:

```json
{
  "word": "கொண்டுவா",
  "ve": ["-வர", "-வந்து"]
}
```

### Starting the server

Make sure you have installed Rust and Cargo, and run `cargo run --release`
to start the server on `localhost:8000`. A `Rocket.toml` file can be used to
[configure the server further](https://rocket.rs/v0.5-rc/guide/configuration/).
