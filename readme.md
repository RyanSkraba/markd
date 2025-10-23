Markd - A scala library for parsing and cleaning Markdown
==============================================================================

[![Java CI with Maven](https://github.com/RyanSkraba/markd/actions/workflows/maven.yml/badge.svg)](https://github.com/RyanSkraba/markd/actions/workflows/maven.yml)

**TL;DR**: Read markdown text into scala data structures that you can manipulate programmatically, and write the contents back out to plain text.

While there isn't any rigorous specification for markdown, this project will target [CommonMark](https://commonmark.org).

This library supports parsing [Markdown](https://en.wikipedia.org/wiki/Markdown) text into a data structure, and supports some common (but not all) markdown features:

* ✅ **Headers** (both atx and setext styles). Headers are a tree-like structure; each header contains the paragraphs and other markdown elements in its section, including subheaders.
* ✅ **Fenced code blocks** (including JSON formatting)
* ✅ **Link references**
* ✅ **Comments** (not part of CommonMark)
* ✅ **Tables** (not part of CommonMark)
* ✅ **Paragraphs** (includes all non-supported elements)

Unsupported markdown can be passed through parsing/rewriting _without loss_.

* ❌ Thematic breaks
* ❌ Block quotes
* ❌ Indented code bocks
* ❌ HTML blocks
* ❌ Unrecognized markdown
* ❌ Most inlined elements like links, images, bold, italics, monospaced are just treated like plain text in a Paragraph
* ❌ Ordered and unordered lists
* ❌ HTML rendering (other JVM libraries do this very [well](https://github.com/commonmark/commonmark-java))

<!--
Internal notes:

https://spec.commonmark.org/current/#thematic-breaks

TODO:
- Add Break parsing
- Add indented code block parsing  
- FormatCfg 
  - minify
  - atx or setext
  - fenced break style

```bash
  __query_examples=$(byexample_go_markd query --query $'..Using MarkdQL.Examples.!Query' readme.md)
  awk -v newblock="$__query_examples" '
            BEGIN { replacing=0 }
            /{{{/{ print; print newblock; print "}}}"; replacing=1; next }
            /}}}/ && replacing { replacing=0; next }
            !replacing { print }
          ' "$(find . -name MarkdQL.scala)" >/tmp/MarkdQL.scala &&
    mv /tmp/MarkdQL.scala "$(find . -name MarkdQL.scala)"
```

-->

Using Markd
------------------------------------------------------------------------------

You can import the library into your project from [maven central](https://central.sonatype.com/artifact/com.tinfoiled/markd_2.13):

```xml
<dependency>
  <groupId>com.tinfoiled</groupId>
  <artifactId>markd_2.13</artifactId>
  <version>0.0.4</version>
</dependency>
```

Then you can use the API to parse and rewrite markdown text:

```scala
// Some simple markdown text in a string
val txt = {
"""English
  |===
  |Hello world
  |# French
  |Bonjour tout le monde
  |""".stripMargin
}

// Parse it into a structure
val md: Header = Markd.parse(txt)

// This gives this structure: Markd is an "invisible" top level node that
// contains all the whole document, which is composed of two top-level 
// headers, each containing one paragraph
val result = Markd(
  Header("English", 1, List(Paragraph("Hello world"))),
  Header("French", 1, List(Paragraph("Bonjour tout le monde")))
)
  
// And print it out again  
println(md.build().toString)
```

The output of the snippet above would be:

```m̀arkdown
English
==============================================================================

Hello world

French
==============================================================================

Bonjour tout le monde
```

Using MarkdQL
------------------------------------------------------------------------------

### Examples

| Query                  | Description                                                                                                                             |
|------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| `One.Two.Three[*]`     | Find the level one header with the name `One`, with a subheader named `Two` and a third-level header `Three` and return those contents. |
| `Top`                  | Find and return the level one header with the title "Top"                                                                               |
| `"..Top"`              | Find and return the level one header with the title "..Top"                                                                             |
| `/Week .* Failures/`   | ❌ Find and return the level one header that matches the regex, such as `Week 21 Failures`                                               |
| `Monthly..2025-02`     | Find the level one header with the title `Monthly` and return the first subheader named `2025-02` at any level inside                   |
| `Weekly!To Do`         | Find the level one header with the title `Weekly` and return the `To Do` table that it contains.                                        |
| `..!Status[12]`        | Find any `Status` table and return the 12th table row (note that row 0 is always the column headers).                                   |
| `..!Status[0][3]`      | ❌ Find any `Status` table and return the name of the 4th column (row 0 is the headers, and columns are zero indexed).                   |
| `..!Status[Key,rowId]` | Find any Status table and return the cell under the column `Key` with the row header `rowId`  **Note that this is column-first!**       |
| `..Weekly[0]`          | Any header with the title `Weekly` and return the first element it contains.                                                            |
| `Weekly[code][0]`      | ❌ Find the top `Weekly` header and return the first code block it contains.                                                             |
| `Weekly[0][4]`         | ❌ Find the top `Weekly` header, go to its first child and return that elements 5th child.                                               |

Building
------------------------------------------------------------------------------

```sh
# Build, format and run all tests
mvn spotless:apply clean verify
```
