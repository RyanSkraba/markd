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

-->

Using Markd
------------------------------------------------------------------------------

You can import the library into your project from [maven central](https://central.sonatype.com/artifact/com.tinfoiled/markd):

```xml
<dependency>
  <groupId>com.tinfoiled</groupId>
  <artifactId>markd_2.13</artifactId>
  <version>0.0.2</version>
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

Building Markd
------------------------------------------------------------------------------

```sh
# Local build running all the tests
mvn spotless:apply clean install
```
