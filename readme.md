# Markd - A scala library for parsing and cleaning Markdown

[![Java CI with Maven](https://github.com/RyanSkraba/markd/actions/workflows/maven.yml/badge.svg)](https://github.com/RyanSkraba/markd/actions/workflows/maven.yml)

**TL;DR**: Read markdown text into scala data structures that you can manipulate programmatically, and write the contents back out to plain text.

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
