# Markd - A scala library for parsing and cleaning Markdown

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
* ❌ HTML rendering (other JVM libraries do this very [well](https://github.com/commonmark/commonmark-java)).

There isn't any rigorous specification for markdown, but future work will target [CommonMark](https://commonmark.org).

## Using Markd

You can import the library into your project from [maven central](https://central.sonatype.com/artifact/com.tinfoiled/markd):

```xml
<dependency>
  <groupId>com.tinfoiled</groupId>
  <artifactId>markd_2.13</artifactId>
  <version>0.0.1</version>
</dependency>
```

## Building Markd

```sh
# Local build running all the tests
mvn spotless:apply clean install
```
