Markd - A scala library for parsing and cleaning Markdown
==============================================================================

[![Java CI with Maven](https://github.com/RyanSkraba/markd/actions/workflows/maven.yml/badge.svg)](https://github.com/RyanSkraba/markd/actions/workflows/maven.yml)

**TL;DR**: Read markdown text into scala data structures that you can manipulate programmatically, and write the contents back out to plain text.

This library supports parsing [Markdown](https://en.wikipedia.org/wiki/Markdown) text into a data structure, and supports some common (but not all) markdown features:

* ✅ Headers are parsed into a tree-like structure: smaller sub-headers are "children" to the higher level headers, and paragraphs "belong" to the closest header.
* ✅ Paragraphs (includes all non-supported elements)
* ✅ Comments
* ✅ Fenced code blocks (including JSON formatting)
* ✅ Links and link references

Unsupported markdown is just treated like plain text in a paragraph, and can be passed through parsing/rewriting _without loss_.

* ❌ Ordered and unordered lists
* ❌ Section breaks
*  ❌ HTML rendering (other JVM libraries do this very [well](https://github.com/commonmark/commonmark-java))


There isn't any rigorous specification for markdown, but future work will target [CommonMark](https://commonmark.org).

Using Markd
------------------------------------------------------------------------------

You can import the library into your project from [maven central](https://central.sonatype.com/artifact/com.tinfoiled/markd):

```xml
<dependency>
  <groupId>com.tinfoiled</groupId>
  <artifactId>markd_2.13</artifactId>
  <version>0.0.1</version>
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
val md: Header = Header.parse(txt)

// This gives this structure, note that the Header(0) contains the whole 
// document, which is composed of two top-level headers, each containing one
// paragraph
val result = Header("", 0, List(
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
