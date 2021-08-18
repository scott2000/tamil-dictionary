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
* Ranking of results based on similarity to query
* True dictionary sort order (not based on Unicode order)
* Automatic transliteration (`maram` => `மரம்`)
* Awareness of grammar rules (`கடல்கரை` => `கடற்கரை`)
* Automatic light/dark theme depending on device settings

## Installation

Clone the repository:

```sh
git clone https://github.com/scott2000/tamil-dictionary.git
```

Create a file called `dictionary.json` with the contents of your dictionary
represented as a list of objects. In each object, there should be a `word` field
containing the headword (which may optionally contain commas separating multiple
words), an optional `sub` field containing the index of the subword if there are
multiple entries for the same word, and a list of sections. Each section
contains a list of paragraphs, with the first paragraph being treated as a
header for the section, and the rest being treated as numbered definitions. A
paragraph contains a list of segments, with each segment containing one of
`txt`, `ref`, `sup`, `bld`, `wbr` and a string. The `txt` segment renders plain
text, `ref` denotes a reference to another word in the dictionary, `sup` creates
a superscript, `bld` bolds text, and `wbr` inserts `<wbr>` before text. Here
is an example object:

```json
{
  "word": "மரம்",
  "sub": 1,
  "secs": [
    [
      [
        ["txt", "n. First section."]
      ],
      [
        ["txt", "tree"]
      ],
      [
        ["txt", "wood (see also: "],
        ["ref", "ஆலமரம்"],
        ["sup", "2"],
        ["txt", ")"]
      ]
    ],
    [
      [
        ["txt", "Another section. "],
        ["bld", "This is bolded."]
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
<p><strong>மரம்</strong><sup>1</sup> n. First section.</p>
<ol start="1">
<li>tree</li>
<li>wood (see also: <a href="#">ஆலமரம்</a><sup>2</sup>)</li>
</ol>
<p>Another section. <strong>This is bolded.</strong></p>
<ol start="3">
<li>ThisIsAlongWordSoItMay<wbr>BeHelpfulToAddAWordBreak</li>
</ol>
</div>
<hr>

Make sure you have installed Rust and Cargo, and run `cargo run --release`
to start the server on `localhost:8000`. A `Rocket.toml` file can be used to
[configure the server further](https://rocket.rs/v0.5-rc/guide/configuration/).
