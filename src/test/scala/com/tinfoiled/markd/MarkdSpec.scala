package com.tinfoiled.markd

import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Markd]]
  */
class MarkdSpec extends AnyFunSpecLike with Matchers {

  describe("Parsing a paragraph") {

    it("should be empty if there aren't any contents") {
      val md = Header.parse("     \t\n\n")
      md shouldBe Header(0, "")

      val cleaned = md.build().toString
      cleaned shouldBe ""
      Header.parse(cleaned) shouldBe md
    }

    for (
      (tag, content) <- Seq(
        "simple" -> "Hello world",
        "simplePreWhitespace" -> "\n\n\t  Hello world",
        "simplePostWhitespace" -> "Hello world\n\t  \n\n",
        "internalNewline" -> "Hello\nworld",
        "internalNewlinePreWhitespace" -> "\n\n\t  Hello\nworld",
        "internalNewlinePostWhitespace" -> "Hello\nworld\n\t  \n\n",
        "internalWhitespace" -> "Hello     \n     world",
        "internalWhitespacePreWhitespace" -> "\n\n\t  Hello     \n     world",
        "internalWhitespacePostWhitespace" -> "Hello     \n     world\n\t  \n\n"
      )
    ) {
      describe(s"for the $tag paragraph") {
        it("creates a simple paragraph in no section") {
          val md = Header.parse(content)
          md shouldBe Header(0, "", Paragraph(content.trim))

          val cleaned = md.build().toString
          cleaned shouldBe s"${content.trim}\n"
          Header.parse(cleaned) shouldBe md
        }

        it("creates a simple paragraph in a section") {
          val md = Header.parse(s"# Main\n$content")
          md shouldBe Header(0, "", Header(1, "Main", Paragraph(content.trim)))

          val cleaned = md.build().toString
          cleaned shouldBe
            s"""Main
              |==============================================================================
              |
              |${content.trim}
              |""".stripMargin
          Header.parse(cleaned) shouldBe md
        }
      }
    }

    for (
      (tag, content) <- Seq(
        "simple" -> "Hello\n\nworld",
        "simplePreWhitespace" -> "\n\n\t  Hello\n\nworld",
        "simplePostWhitespace" -> "Hello\n\nworld\n\t  \n\n",
        "internalPreWhitespace" -> "Hello  \t  \n\nworld",
        "internalMidWhitespace" -> "Hello\n  \t  \nworld",
        "internalPostWhitespace" -> "Hello\n\n  \t  world",
        "internalLotsWhitespace" -> "Hello\n\n\n\n\n\nworld"
      )
    ) {
      describe(s"for the $tag paragraphs") {
        it("creates paragraphs in no sections") {
          val md = Header.parse(content)
          md shouldBe Header(0, "", Paragraph("Hello"), Paragraph("world"))

          val cleaned = md.build().toString
          cleaned shouldBe
            s"""Hello
               |
               |world
               |""".stripMargin
          Header.parse(cleaned) shouldBe md
        }

        it("creates paragraphs in a section") {
          val md = Header.parse(s"# Main\n$content")
          md shouldBe Header(
            0,
            "",
            Header(1, "Main", Paragraph("Hello"), Paragraph("world"))
          )

          val cleaned = md.build().toString
          cleaned shouldBe
            s"""Main
               |==============================================================================
               |
               |Hello
               |
               |world
               |""".stripMargin
          Header.parse(cleaned) shouldBe md
        }
      }
    }

    describe("with a code block") {
      it("should find a standalone element") {
        val md = Header.parse("""
            |```bash
            |echo Hello world
            |```
        """.stripMargin)
        md shouldBe Header(0, "", Code("bash", "echo Hello world\n"))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```bash
            |echo Hello world
            |```
            |""".stripMargin
        Header.parse(cleaned) shouldBe md
      }

      it("should ignore headers inside a code block") {
        val md = Header.parse("""
                                |```bash
                                |# echo Hello world
                                |```
        """.stripMargin)
        md shouldBe Header(0, "", Code("bash", "# echo Hello world\n"))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```bash
            |# echo Hello world
            |```
            |""".stripMargin
        Header.parse(cleaned) shouldBe md
      }

      it("should ignore unnecessary whitespace") {
        val md = Header.parse("""
            |```bash.....
            |.....echo Hello world.....
            |```.....
        """.stripMargin.replace(".", " "))
        md shouldBe Header(0, "", Code("bash", "     echo Hello world     \n"))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```bash
            |.....echo Hello world.....
            |```
            |""".stripMargin.replace(".", " ")
        Header.parse(cleaned) shouldBe md
      }

      it("should ignore a code block with bad whitespace") {
        val md = Header.parse("""
                                |   ```bash
                                |echo Hello world
                                |```
        """.stripMargin.replace(".", " "))
        // TODO: What do we expect here?  This is probably not what we want and cleaning breaks.
        // md shouldBe Header(0, "", Paragraph("   ```bash\necho Hello world\n```"))

        val cleaned = md.build().toString
        cleaned shouldBe
          """``bash
            |echo Hello world
            |
            |``
            |""".stripMargin.replace(".", " ")
        Header.parse(cleaned) shouldBe md
      }

      // TODO: This doesn't quite work.
      ignore("should ignore an internal code block") {
        val md = Header.parse("""
            |echo ```Hello``` world
            |""".stripMargin)
        md shouldBe Header(0, "", Paragraph("echo ```Hello``` world"))

        val cleaned = md.build().toString
        cleaned shouldBe """echo ```Hello``` world"""
        Header.parse(cleaned) shouldBe md
      }

      it("should prettify a JSON code block") {
        val md = Header.parse("""
            |```json
            |{"id": 1, "names": ["One", "Un"]}
            |```
            |""".stripMargin.replace(".", " "))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```json
            |{
            |  "id" : 1,
            |  "names" : [ "One", "Un" ]
            |}
            |```
            |""".stripMargin.replace(".", " ")
        Header.parse(cleaned).build().toString shouldBe cleaned
      }

      it("should not prettify a JSON code block with invalid JSON") {
        val md = Header.parse("""
            |```json
            |{"id": ##, "names": ["One", "Un"]}
            |```
            |""".stripMargin.replace(".", " "))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```json
            |{"id": ##, "names": ["One", "Un"]}
            |```
            |""".stripMargin.replace(".", " ")
        Header.parse(cleaned) shouldBe md
      }

      it("should prettify a jsonline code block") {
        val md = Header.parse("""
            |```jsonline
            |{"id": 1, "names": ["One", "Un"]}
            |{"id": 2, "names": ["Two", "Deux"]}
            |```
            |""".stripMargin.replace(".", " "))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```jsonline
            |{"id":1,"names":["One","Un"]}
            |{"id":2,"names":["Two","Deux"]}
            |```
            |""".stripMargin.replace(".", " ")
        Header.parse(cleaned).build().toString shouldBe cleaned
      }

      it(
        "should prettify only the valid lines in jsonline or json line code blocks"
      ) {
        val md = Header.parse("""```jsonline
            |jsonline can't be split over lines
            |{"id": 1,
            | "names": ["One", "Un"]}
            |{"id": ##, "names": ["One", "Un"]}
            |{"id": 2, "names": ["Two", "Deux"]}
            |```
            |```json line
            |{"id": 3, "names": ["Three", "Trois"]}
            |{"id": 4,     "names": ["Four", "Quatre"}
            |```
            |```jsonlines
            |{"id": 5, "names": ["Five", "Cinq"]}
            |{"id": 6,     "names": ["Six"}
            |```
            |```json lines
            |{"id": 7, "names": ["Sept", "Seven"]}
            |{"id": 8,     "names": ["Eight", "Huit"}
            |```
            |""".stripMargin.replace(".", " "))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```jsonline
            |jsonline can't be split over lines
            |{"id": 1,
            | "names": ["One", "Un"]}
            |{"id": ##, "names": ["One", "Un"]}
            |{"id":2,"names":["Two","Deux"]}
            |```
            |
            |```json line
            |{"id":3,"names":["Three","Trois"]}
            |{"id": 4,     "names": ["Four", "Quatre"}
            |```
            |
            |```jsonlines
            |{"id":5,"names":["Five","Cinq"]}
            |{"id": 6,     "names": ["Six"}
            |```
            |
            |```json lines
            |{"id":7,"names":["Sept","Seven"]}
            |{"id": 8,     "names": ["Eight", "Huit"}
            |```
            |""".stripMargin.replace(".", " ")
        Header.parse(cleaned).build().toString shouldBe cleaned
      }
    }

    describe("with a comment block") {
      it("should find a standalone element") {
        val md = Header.parse("<!-- Hello world -->")
        md shouldBe Header(0, "", Comment(" Hello world "))

        val cleaned = md.build().toString
        cleaned shouldBe "<!-- Hello world -->\n"
        Header.parse(cleaned) shouldBe md
      }

      it("should ignore unnecessary whitespace") {
        val md = Header.parse("\n    \t<!-- Hello\n\tworld -->\n    \t")
        md shouldBe Header(0, "", Comment(" Hello\n\tworld "))

        val cleaned = md.build().toString
        cleaned shouldBe "<!-- Hello\n\tworld -->\n"
        Header.parse(cleaned) shouldBe md
      }

      it("should separate it carefully from other elements") {
        val md = Header.parse("Hello\t<!-- Hello\n\tworld -->     world")
        md shouldBe Header(
          0,
          "",
          Paragraph("Hello"),
          Comment(" Hello\n\tworld "),
          Paragraph("world")
        )

        val cleaned = md.build().toString
        cleaned shouldBe
          """Hello
            |
            |<!-- Hello
            |.world -->
            |
            |world
            |""".stripMargin.replace(".", "\t")
        Header.parse(cleaned) shouldBe md
      }
    }

    describe("should parse linkrefs") {
      val linkrefs =
        """
          |[ref-bare]:
          |[dup]: dup
          |[dup]: dup "dup"
          |[url]: url
          |[url-prews]: url-prews
          |[url-postws]: url-postws
          |[title]: "title"
          |[title-prews]: "title-prews"
          |[title-postws]: "title-postws"
          |[title-empty]:
          |[title-empty-prews]:
          |[title-empty-postws]:
          |[all]: all "all"
          |[all-prews]: all-prews "all-prews"
          |[dup]: dup "lastdup"
          |[all-midws]: all-midws "all-midws"
          |[all-postws]: all-postws "all-postws"
          |[all-empty-title]: all-empty-title
          |""".stripMargin.replace(".", " ")

      it("and sort, clean and deduplicate by default") {
        val md = Header.parse(linkrefs)
        val cleaned = md.build().toString
        cleaned shouldBe """[all]: all "all"
                         |[all-empty-title]: all-empty-title
                         |[all-midws]: all-midws "all-midws"
                         |[all-postws]: all-postws "all-postws"
                         |[all-prews]: all-prews "all-prews"
                         |[dup]: dup "lastdup"
                         |[ref-bare]:
                         |[title]: "title"
                         |[title-empty]:
                         |[title-empty-postws]:
                         |[title-empty-prews]:
                         |[title-postws]: "title-postws"
                         |[title-prews]: "title-prews"
                         |[url]: url
                         |[url-postws]: url-postws
                         |[url-prews]: url-prews
                         |""".stripMargin
        Header.parse(cleaned) shouldBe md
      }

      it("should escape and unescape link titles correctly") {
        val linkRefTitles =
          """[a]: url "title"
            |[b]: url "postws  "
            |[c]: url "  prews"
            |[d]: url "Quo\\th \""
            |[e]: "title"
            |[f]: "postws  "
            |[g]: "  prews"
            |[h]: "Quo\\th \""
            |""".stripMargin
        // The round trip shouldn't change the text at all
        val md = Header.parse(linkRefTitles)
        val cleaned = md.build().toString
        cleaned shouldBe linkRefTitles
        Header.parse(cleaned) shouldBe md

        // But shouldParse
        md.mds should have size 8
        md.mds.head shouldBe LinkRef("a", "url", "title")
        md.mds(1) shouldBe LinkRef("b", "url", "postws  ")
        md.mds(2) shouldBe LinkRef("c", "url", "  prews")
        md.mds(3) shouldBe LinkRef("d", "url", "Quo\\th \"")
        md.mds(4) shouldBe LinkRef("e", None, Some("title"))
        md.mds(5) shouldBe LinkRef("f", None, Some("postws  "))
        md.mds(6) shouldBe LinkRef("g", None, Some("  prews"))
        md.mds(7) shouldBe LinkRef("h", None, Some("Quo\\th \""))
      }

      it("but allow leaving unsorted and undeduplicated") {
        val md =
          Header.parse(linkrefs, cfg = new ParserCfg(sortLinkRefs = false))

        val cleaned = md.build().toString
        cleaned shouldBe
          """[ref-bare]:
            |[dup]: dup
            |[dup]: dup "dup"
            |[url]: url
            |[url-prews]: url-prews
            |[url-postws]: url-postws
            |[title]: "title"
            |[title-prews]: "title-prews"
            |[title-postws]: "title-postws"
            |[title-empty]:
            |[title-empty-prews]:
            |[title-empty-postws]:
            |[all]: all "all"
            |[all-prews]: all-prews "all-prews"
            |[dup]: dup "lastdup"
            |[all-midws]: all-midws "all-midws"
            |[all-postws]: all-postws "all-postws"
            |[all-empty-title]: all-empty-title
            |""".stripMargin
        Header.parse(
          cleaned,
          cfg = new ParserCfg(sortLinkRefs = false)
        ) shouldBe md

        md.mds should have size 18
        md.mds.head shouldBe LinkRef("ref-bare", None, None)
        md.mds(1) shouldBe LinkRef("dup", "dup")
        md.mds(2) shouldBe LinkRef("dup", "dup", "dup")
        md.mds(3) shouldBe LinkRef("url", "url")
        md.mds(4) shouldBe LinkRef("url-prews", "url-prews")
        md.mds(5) shouldBe LinkRef("url-postws", "url-postws")
        md.mds(6) shouldBe LinkRef("title", None, Some("title"))
        md.mds(7) shouldBe LinkRef("title-prews", None, Some("title-prews"))
        md.mds(8) shouldBe LinkRef("title-postws", None, Some("title-postws"))
        md.mds(9) shouldBe LinkRef("title-empty", None, None)
        md.mds(10) shouldBe LinkRef("title-empty-prews", None, None)
        md.mds(11) shouldBe LinkRef("title-empty-postws", None, None)
        md.mds(12) shouldBe LinkRef("all", "all", "all")
        md.mds(13) shouldBe LinkRef("all-prews", "all-prews", "all-prews")
        md.mds(14) shouldBe LinkRef("dup", "dup", "lastdup")
        md.mds(15) shouldBe LinkRef("all-midws", "all-midws", "all-midws")
        md.mds(16) shouldBe LinkRef("all-postws", "all-postws", "all-postws")
        md.mds(17) shouldBe LinkRef("all-empty-title", "all-empty-title")
      }
    }
  }

  it("should ignore non-linkref") {
    val md = Header.parse("""
                            |[url]: url
                            | [space-before]: Leading space?  Not a link ref
                            |""".stripMargin.replace(".", " "))

    val cleaned = md.build().toString
    cleaned shouldBe """[space-before]: Leading space?  Not a link ref
                       |
                       |[url]: url
                       |""".stripMargin
    // TODO: The round-trip is still broken because of cleaning up whitespace.
    // Header.parse(cleaned) shouldBe md

    md.mds should have size 2
    md.mds.head shouldBe Paragraph(
      "[space-before]: Leading space?  Not a link ref"
    )
    md.mds(1) shouldBe LinkRef("url", "url")
  }

  describe("Parsing markdown into sections") {

    it("should separate into level 1 headers") {
      val md = Header.parse("""English
          |===
          |Hello world
          |# French
          |Bonjour tout le monde""".stripMargin)
      md.mds should have size 2

      val cleaned = md.build().toString
      cleaned shouldBe
        """English
          |==============================================================================
          |
          |Hello world
          |
          |French
          |==============================================================================
          |
          |Bonjour tout le monde
          |""".stripMargin
      Header.parse(cleaned) shouldBe md
    }

    it("should nicely nest sections ") {
      val md = Header.parse("""
          |### Three
          |## Two
          |# One
          |## Two
          |### Three
          |""".stripMargin)
      md.mds should have size 3

      val cleaned = md.build().toString
      cleaned shouldBe
        """### Three
          |
          |Two
          |------------------------------------------------------------------------------
          |
          |One
          |==============================================================================
          |
          |Two
          |------------------------------------------------------------------------------
          |
          |### Three
          |""".stripMargin
      Header.parse(cleaned) shouldBe md
    }

    it("should separate into headers and links") {
      val md = Header.parse("""
          |outside
          |[refout]: https://www.refout.com
          |# header1
          |h1txt
          |## header1a
          |[ref1a]: https://www.ref1a.com
          |h1atxt
          |[ref1a_dup]: https://www.ref1a.com
          |## header1b
          |h1btxt
          |### header1b1
          |h1b1txt
          |# header2
          |h2txt
          |[ref2]: https://www.ref2.com
          |## header2a
          |h2atxt
          |## header2b
          |h2btxt
          |[ref2b]: https://www.ref2b.com
          |""".stripMargin)

      val cleaned = md.build().toString
      cleaned shouldBe
        """outside
          |
          |[refout]: https://www.refout.com
          |
          |header1
          |==============================================================================
          |
          |h1txt
          |
          |header1a
          |------------------------------------------------------------------------------
          |
          |h1atxt
          |
          |[ref1a]: https://www.ref1a.com
          |[ref1a_dup]: https://www.ref1a.com
          |
          |header1b
          |------------------------------------------------------------------------------
          |
          |h1btxt
          |
          |### header1b1
          |
          |h1b1txt
          |
          |header2
          |==============================================================================
          |
          |h2txt
          |
          |[ref2]: https://www.ref2.com
          |
          |header2a
          |------------------------------------------------------------------------------
          |
          |h2atxt
          |
          |header2b
          |------------------------------------------------------------------------------
          |
          |h2btxt
          |
          |[ref2b]: https://www.ref2b.com
          |""".stripMargin
      Header.parse(cleaned) shouldBe md

      md.mds should have size 4
      md.mds.head shouldBe Paragraph("outside")
      md.mds(1) shouldBe LinkRef("refout", "https://www.refout.com")
      md.mds(2) shouldBe a[Header]
      md.mds(3) shouldBe a[Header]
      val h1 = md.mds(2).asInstanceOf[Header]
      val h2 = md.mds(3).asInstanceOf[Header]

      h1.mds should have size 3
      h1.mds.head shouldBe Paragraph("h1txt")
      h1.mds(1) shouldBe a[Header]
      h1.mds(2) shouldBe a[Header]
      h1.mds(1) shouldBe Header(
        2,
        "header1a",
        Paragraph("h1atxt"),
        LinkRef("ref1a", "https://www.ref1a.com"),
        LinkRef("ref1a_dup", "https://www.ref1a.com")
      )
      h1.mds(2) shouldBe Header(
        2,
        "header1b",
        Paragraph("h1btxt"),
        Header(3, "header1b1", Paragraph("h1b1txt"))
      )

      h2.mds should have size 4
      h2.mds.head shouldBe Paragraph("h2txt")
      h2.mds(1) shouldBe LinkRef("ref2", "https://www.ref2.com")
      h2.mds(2) shouldBe Header(2, "header2a", Paragraph("h2atxt"))
      h2.mds(3) shouldBe Header(
        2,
        "header2b",
        Paragraph("h2btxt"),
        LinkRef("ref2b", "https://www.ref2b.com")
      )
    }
  }

  describe("Parsing markdown into tables") {

    it("should parse lines into cells") {
      Table.parseRow("") shouldBe Seq()
      Table.parseRow("|") shouldBe Seq()
      Table.parseRow("||") shouldBe Seq()
      Table.parseRow("one|two|three") shouldBe Seq("one", "two", "three")
      Table.parseRow("|two|three") shouldBe Seq("", "two", "three")
      Table.parseRow("one||three") shouldBe Seq("one", "", "three")
      Table.parseRow("one|two|") shouldBe Seq("one", "two")
      Table.parseRow("one|two||||") shouldBe Seq("one", "two")
      Table.parseRow(raw"\||two|three") shouldBe Seq(
        raw"\|",
        "two",
        "three"
      )
      Table.parseRow(raw"\||two|three||\|") shouldBe Seq(
        raw"\|",
        "two",
        "three",
        "",
        raw"\|"
      )
      Table.parseRow(raw"one\||t\|wo|\|three") shouldBe Seq(
        raw"one\|",
        raw"t\|wo",
        raw"\|three"
      )
    }

    it("should access table cells and rows") {
      val md = Table
        .parse("""A  | B  | A
                 !---|----|----
                 !a  | b1 | c1
                 !a2 | b2 | c2
                 !a  | b3 | c3
                 !a4 | b4 |
                 !   | b5 |
                 !   |    |
                 !""".stripMargin('!'))
        .value

      // Get a row by index
      md.title shouldBe "A"
      md.colSize shouldBe 3
      md.rowSize shouldBe 7
      md(0) shouldBe TableRow.from("A", "B", "A")
      md(1) shouldBe TableRow.from("a", "b1", "c1")
      md(2) shouldBe TableRow.from("a2", "b2", "c2")
      md(3) shouldBe TableRow.from("a", "b3", "c3")
      md(4) shouldBe TableRow.from("a4", "b4")
      md(5) shouldBe TableRow.from("", "b5")
      md(6) shouldBe TableRow.from()
      // There's no way to distinguish from a TableRow that actually exists in
      // the table, and one that is out of range
      md(100) shouldBe TableRow.from()
      md(-1) shouldBe TableRow.from()

      // Get a row by name
      md("A") shouldBe TableRow.from("A", "B", "A")
      md("a") shouldBe TableRow.from("a", "b1", "c1")
      md("a2") shouldBe TableRow.from("a2", "b2", "c2")
      md("a4") shouldBe TableRow.from("a4", "b4")
      md("") shouldBe TableRow.from("", "b5")
      md("no-exist") shouldBe TableRow.from()
      // There's no way to get the row with the duplicate head by name, only
      // by index

      // Get all existing cells by index
      val cells = for (col <- 0 to 2; row <- 0 to 6) yield md(col, row)
      cells shouldBe Seq(
        Seq("A", "a", "a2", "a", "a4", "", ""),
        Seq("B", "b1", "b2", "b3", "b4", "b5", ""),
        Seq("A", "c1", "c2", "c3", "", "", "")
      ).flatten
      // There's no way to distinguish between an empty cell and one out of
      // range
      md(2, 4) shouldBe ""
      md(-1, -1) shouldBe ""
      md(-1, 100) shouldBe ""
      md(100, -1) shouldBe ""
      md(100, 100) shouldBe ""

      // Get all existing cells by column index and row headers
      val cells2 =
        for (col <- 0 to 2; row <- Seq("A", "a", "a2", "a4", ""))
          yield md(col, row)
      cells2 shouldBe Seq(
        Seq("A", "a", "a2", "a4", ""),
        Seq("B", "b1", "b2", "b4", "b5"),
        Seq("A", "c1", "c2", "", "")
      ).flatten
      // There's no way to distinguish between an empty cell and one out of
      // range
      md(2, "a4") shouldBe ""
      md(-1, "a") shouldBe ""
      md(100, "a") shouldBe ""
      md(-1, "no-exist") shouldBe ""
      md(0, "no-exist") shouldBe ""
      md(100, "no-exist") shouldBe ""

      // Get all existing cells by column and row headers
      val cells3 =
        for (col <- Seq("A", "B"); row <- Seq("A", "a", "a2", "a4", ""))
          yield md(col, row)
      cells3 shouldBe Seq(
        Seq("A", "a", "a2", "a4", ""),
        Seq("B", "b1", "b2", "b4", "b5")
      ).flatten
      // There's no way to distinguish between an empty cell and one out of
      // range
      md("A", "") shouldBe ""
      md("NO", "a") shouldBe ""
      md("A", "no-exist") shouldBe ""
      md("NO", "no-exist") shouldBe ""
    }

    it("should parse and update TableRows") {
      val md = Table
        .parse("""Id | Name
                 !---|------
                 !1  | One
                 !2  |
                 !   |
                 !""".stripMargin('!'))
        .value

      val tb1 = TableRow.from("1", "One")
      val tb2 = TableRow.from("2")
      val tb3 = TableRow.from()

      md.title shouldBe "Id"
      md.colSize shouldBe 2
      md.rowSize shouldBe 4
      md shouldBe Table.from(
        Seq(Align.LEFT, Align.LEFT),
        TableRow.from("Id", "Name"),
        tb1,
        tb2,
        tb3
      )

      // Verify the shortcut to the head
      tb1.head shouldBe "1"
      tb2.head shouldBe "2"
      tb3.head shouldBe ""

      // Verify the shortcut to the cell
      tb1(0) shouldBe "1"
      tb1(1) shouldBe "One"
      tb1(2) shouldBe empty
      tb2(0) shouldBe "2"
      tb2(1) shouldBe empty
      tb2(2) shouldBe empty
      tb3(0) shouldBe empty
      tb3(1) shouldBe empty
      tb3(2) shouldBe empty

      // Verify the shortcut to the cell update
      tb1.updated(0, "Un") shouldBe TableRow.from("Un", "One")
      tb1.updated(1, "Un") shouldBe TableRow.from("1", "Un")
      tb1.updated(2, "Un") shouldBe TableRow.from("1", "One", "Un")
      tb1.updated(4, "Un") shouldBe TableRow.from("1", "One", "", "", "Un")
      tb1.updated(0, "") shouldBe TableRow.from("", "One")
      tb1.updated(1, "") shouldBe TableRow.from("1")
    }

    it("should clean up a simple table") {
      val md = Header.parse("""Before
          !
          !Id        | Name
          !---    | ---
          !   [1](https://en.wikipedia.org/wiki/1)    |      One
          !2|Two
          !3|Three
          !
          !
          !After
          !""".stripMargin('!'))

      md.mds should have size 3
      md.mds.head shouldBe Paragraph("Before")
      md.mds(1) shouldBe Table.from(
        Seq(Align.LEFT, Align.LEFT),
        TableRow.from("Id", "Name"),
        TableRow.from("[1](https://en.wikipedia.org/wiki/1)", "One"),
        TableRow.from("2", "Two"),
        TableRow.from("3", "Three")
      )
      md.mds(2) shouldBe Paragraph("After")

      val cleaned = md.build().toString
      cleaned shouldBe
        """Before
          !
          !| Id                                   | Name  |
          !|--------------------------------------|-------|
          !| [1](https://en.wikipedia.org/wiki/1) | One   |
          !| 2                                    | Two   |
          !| 3                                    | Three |
          !
          !After
          !""".stripMargin('!')
      Header.parse(cleaned) shouldBe md
    }

    it("should detect column alignment") {
      val md = Header.parse("""
          !Id1|Id2|Id3|Name
          !:--   |   :--: |------:  |--:
          !   1    |1    |1    |      One
          !22|22|22|Two
          !333|333|333|Three
          !""".stripMargin('!'))

      md.mds should have size 1
      md.mds.head shouldBe Table.from(
        Seq(
          Align.LEFT,
          Align.CENTER,
          Align.RIGHT,
          Align.RIGHT
        ),
        TableRow.from("Id1", "Id2", "Id3", "Name"),
        TableRow.from("1", "1", "1", "One"),
        TableRow.from("22", "22", "22", "Two"),
        TableRow.from("333", "333", "333", "Three")
      )

      val cleaned = md.build().toString
      cleaned shouldBe
        """| Id1 | Id2 | Id3 |  Name |
          !|-----|:---:|----:|------:|
          !| 1   |  1  |   1 |   One |
          !| 22  | 22  |  22 |   Two |
          !| 333 | 333 | 333 | Three |
          !""".stripMargin('!')
      Header.parse(cleaned) shouldBe md
    }

    it("should handle ragged rows") {
      val md = Table
        .parse("""AAA|BBB|CCC|DDD|||||
          !---|:-:|--:|---
          !a
          !b|b
          !c|c|c
          !d|d|d|d
          !e|e|e|e|eee
          !f|f|f|f|ff|f|f|f|||||
          !
          !a
          !|b
          !||c
          !|||d
          !||||e
          !|||||||f|||||
          !""".stripMargin('!'))
        .value
      val cleaned = md.build().toString
      cleaned shouldBe
        """| AAA | BBB | CCC | DDD |
          !|-----|:---:|----:|-----|
          !| a   |     |     |     |
          !| b   |  b  |     |     |
          !| c   |  c  |   c |     |
          !| d   |  d  |   d | d   |
          !| e   |  e  |   e | e   | eee |
          !| f   |  f  |   f | f   | ff | f | f | f |
          !|     |     |     |     |
          !| a   |     |     |     |
          !|     |  b  |     |     |
          !|     |     |   c |     |
          !|     |     |     | d   |
          !|     |     |     |     | e |
          !|     |     |     |     |  |  |  | f |
          !""".stripMargin('!')
      md.title shouldBe "AAA"
      md.colSize shouldBe 4
      md.rowSize shouldBe 14
      Table.parse(cleaned).value shouldBe md
    }

    it("should handle empty column headers") {
      val md = Table
        .parse("""|   ||
          !---|:-:|--:|---
          !a|b|c|d
          !""".stripMargin('!'))
        .value
      val cleaned = md.build().toString
      cleaned shouldBe
        """|   |   |   |   |
          !|---|:-:|--:|---|
          !| a | b | c | d |
          !""".stripMargin('!')
      md.title shouldBe ""
      md.colSize shouldBe 4
      md.rowSize shouldBe 2
      Table.parse(cleaned).value shouldBe md
    }

    it("should handle extra pipes") {
      val md = Table
        .parse("""|Id|Name|
                 !|---|---|
                 !|1  |One|
                 ! 2  |Two|
                 !|3  | Three
                 ! 4  | Four
                 ! |5|Five||||
                 !""".stripMargin('!'))
        .value
      val cleaned = md.build().toString
      cleaned shouldBe
        """| Id | Name  |
          !|----|-------|
          !| 1  | One   |
          !| 2  | Two   |
          !| 3  | Three |
          !| 4  | Four  |
          !|    | 5     | Five |
          !""".stripMargin('!')
      md.title shouldBe "Id"
      md.colSize shouldBe 2
      md.rowSize shouldBe 6
      Table.parse(cleaned).value shouldBe md
    }

    it("should handle escaped pipes") {
      val md = Table
        .parse("""|   \|\||\||
                 !---|:-:|--:|---
                 !a\|a|b\||\|c|d
                 !""".stripMargin('!'))
        .value
      val cleaned = md.build().toString
      cleaned shouldBe
        """|      | \|\| |  \| |   |
          !|------|:----:|----:|---|
          !| a\|a | b\|  | \|c | d |
          !""".stripMargin('!')
      md.title shouldBe ""
      md.colSize shouldBe 4
      md.rowSize shouldBe 2
      Table.parse(cleaned).value shouldBe md
    }

    it("should ignore non-tables") {
      // Missing or bad alignment lines
      Table.parse("Hello world") shouldBe None
      Table.parse("A|B|C") shouldBe None
      Table.parse("A|B|C\na|b|c") shouldBe None
      Table.parse("A|B|C\n---|---|c--") shouldBe None
      Table.parse("---|---|---") shouldBe None
      Table.parse("A|B|C\n---|---|--") shouldBe None
    }

    it("should resolve missing or extra pipes") {
      // A valid table
      val md = Table.parse("A|B\n---|---\na|b").value
      val cleaned = md.build().toString
      cleaned shouldBe
        """| A | B |
          !|---|---|
          !| a | b |
          !""".stripMargin('!')
      md.title shouldBe "A"
      md.colSize shouldBe 2
      md.rowSize shouldBe 2
      Table.parse(cleaned).value shouldBe md

      // Other ways to represent the same table
      for (
        content <- Seq(
          "  A  |  B  \n  ---  |  ---  \n  a  |  b  ",
          "\tA\t|\tB\t\n\t---\t|\t---\t\n\ta\t|\tb\t",
          " A | B \n| --- | --- \n a | b ",
          "| A | B \n| --- | --- \n a | b ",
          " A | B \n| --- | --- \n| a | b ",
          "| A | B |\n| --- | --- \n| a | b |"
        )
      ) {
        Table.parse(content).value shouldBe md
      }
    }

    describe("should update a table") {
      // A valid table
      val md = Table.parse("A|B\n---|---\na|b\nc|d").value

      it("by row index and column index") {
        md.updated(0, 0, "X").build().toString shouldBe
          """| X | B |
            !|---|---|
            !| a | b |
            !| c | d |
            !""".stripMargin('!')
        md.updated(1, 1, "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | X |
            !| c | d |
            !""".stripMargin('!')
        md.updated(0, 2, "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |
            !| X | d |
            !""".stripMargin('!')
      }

      it("by row header and column index") {
        md.updated(0, "A", "X").build().toString shouldBe
          """| X | B |
            !|---|---|
            !| a | b |
            !| c | d |
            !""".stripMargin('!')
        md.updated(1, "a", "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | X |
            !| c | d |
            !""".stripMargin('!')
        md.updated(0, "c", "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |
            !| X | d |
            !""".stripMargin('!')
      }

      it("by row header and column header") {
        md.updated("A", "A", "X").build().toString shouldBe
          """| X | B |
            !|---|---|
            !| a | b |
            !| c | d |
            !""".stripMargin('!')
        md.updated("B", "a", "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | X |
            !| c | d |
            !""".stripMargin('!')
        md.updated("A", "c", "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |
            !| X | d |
            !""".stripMargin('!')
      }

      it("by adding blank columns if necessary, by row index") {
        md.updated(2, 0, "X").build().toString shouldBe
          """| A | B | X |
            !|---|---|---|
            !| a | b |   |
            !| c | d |   |
            !""".stripMargin('!')
        md.updated(3, 0, "X").build().toString shouldBe
          """| A | B |   | X |
            !|---|---|---|---|
            !| a | b |   |   |
            !| c | d |   |   |
            !""".stripMargin('!')
        // When adding to cells that aren't headers, only that row is affected.
        md.updated(2, 1, "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b | X |
            !| c | d |
            !""".stripMargin('!')
        md.updated(10, 2, "X").updated(6, 2, "Y").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |
            !| c | d |  |  |  |  | Y |  |  |  | X |
            !""".stripMargin('!')
      }

      it("by adding blank columns if necessary, by row header") {
        md.updated(2, "A", "X").build().toString shouldBe
          """| A | B | X |
            !|---|---|---|
            !| a | b |   |
            !| c | d |   |
            !""".stripMargin('!')
        md.updated(3, "A", "X").build().toString shouldBe
          """| A | B |   | X |
            !|---|---|---|---|
            !| a | b |   |   |
            !| c | d |   |   |
            !""".stripMargin('!')
        // When adding to cells that aren't headers, only that row is affected.
        md.updated(2, "a", "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b | X |
            !| c | d |
            !""".stripMargin('!')
        md.updated(10, "c", "X").updated(6, "c", "Y").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |
            !| c | d |  |  |  |  | Y |  |  |  | X |
            !""".stripMargin('!')
      }

      it("by adding blank columns if necessary, by row and column header") {
        md.updated("X", "A", "X").build().toString shouldBe
          """| A | B | X |
            !|---|---|---|
            !| a | b |   |
            !| c | d |   |
            !""".stripMargin('!')
        // When adding to cells that aren't headers, the column is added.
        md.updated("X", "a", "x").build().toString shouldBe
          """| A | B | X |
            !|---|---|---|
            !| a | b | x |
            !| c | d |   |
            !""".stripMargin('!')
      }

      it("by adding blank rows if necessary, by index") {
        md.updated(0, 3, "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |
            !| c | d |
            !| X |   |
            !""".stripMargin('!')
        md.updated(1, 5, "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |
            !| c | d |
            !|   |   |
            !|   |   |
            !|   | X |
            !""".stripMargin('!')
      }

      it("by adding blank rows if necessary, by row header and column index") {
        md.updated(0, "X", "x").build().toString shouldBe
          """| A | B |
             !|---|---|
             !| a | b |
             !| c | d |
             !| x |   |
             !""".stripMargin('!')
        md.updated(1, "X", "x").build().toString shouldBe
          """| A | B |
             !|---|---|
             !| a | b |
             !| c | d |
             !| X | x |
             !""".stripMargin('!')
      }

      it("by adding blank rows if necessary, by headers") {
        md.updated("A", "X", "X").build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |
            !| c | d |
            !| X |   |
            !""".stripMargin('!')
        md.updated("B", "X", "x").build().toString shouldBe
          """| A | B |
             !|---|---|
             !| a | b |
             !| c | d |
             !| X | x |
             !""".stripMargin('!')
      }

      it("and delete a column from a nonheader cell, by index") {
        val updated = md.updated(4, 1, "X")
        updated.build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |  |  | X |
            !| c | d |
            !""".stripMargin('!')

        // Remove the updated cell, but only because it didn't extend any columns
        updated.updated(4, 1, "") shouldBe md
        // This is completely ignored
        updated.updated(8, 1, "") shouldBe updated
      }

      it("and delete a column from a nonheader cell, by row header") {
        val updated = md.updated(4, "a", "X")
        updated.build().toString shouldBe
          """| A | B |
            !|---|---|
            !| a | b |  |  | X |
            !| c | d |
            !""".stripMargin('!')

        // Remove the updated cell, but only because it didn't extend any columns
        updated.updated(4, "a", "") shouldBe md
        // This is completely ignored
        updated.updated(8, "a", "") shouldBe updated
      }
    }
  }

  describe("Replacing subelements") {
    val md: Header = Header.parse("""
        |# One
        |# Two
        |# Three
        |""".stripMargin)

    describe("replacing a match one-to-one with another element") {
      it("should replace all matches") {
        md.replaceIn() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "TWO"),
          Header(1, "THREE")
        )
      }
      it("should replace all matches with filtering") {
        md.replaceIn(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(Header(1, "TWO"), Header(1, "THREE"))
      }
      it("should replace the first matching") {
        md.mapFirstIn() {
          case h @ Header(title, 1, _) if title.startsWith("T") =>
            h.copy(title = h.title.toUpperCase)
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "TWO"),
          Header(1, "Three")
        )
        // The equivalent
        md.flatMapFirstIn() {
          case h @ Header(title, 1, _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "TWO"),
          Header(1, "Three")
        )
      }
    }

    describe("removing a match") {
      it("should replace all matches") {
        md.replaceIn() {
          case (Some(Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq.empty
        }.mds shouldBe Seq(Header(1, "One"))
      }
      it("should replace all matches with filtering") {
        // This isn't very useful
        md.replaceIn(filter = true) {
          case (Some(Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq.empty
        }.mds shouldBe Seq.empty
      }
      it("should replace the first") {
        md.flatMapFirstIn() {
          case Header(title, 1, _) if title.startsWith("T") =>
            Seq.empty
        }.mds shouldBe Seq(Header(1, "One"), Header(1, "Three"))
      }
    }

    describe("inserting after a match") {
      it("should replace all matches") {
        md.replaceIn() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h, h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "TWO"),
          Header(1, "Three"),
          Header(1, "THREE")
        )
      }
      it("should replace all matches with filtering") {
        md.replaceIn(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h, h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "Two"),
          Header(1, "TWO"),
          Header(1, "Three"),
          Header(1, "THREE")
        )
      }
      it("should replace the first") {
        md.flatMapFirstIn() {
          case h @ Header(title, 1, _) if title.startsWith("T") =>
            Seq(h, h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "TWO"),
          Header(1, "Three")
        )
      }
    }

    describe("when a match isn't found") {
      it("should do nothing on all matches") {
        md.replaceIn() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe md.mds
      }
      it("should remove all when filtering") {
        md.replaceIn(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq.empty
      }
      it("should do nothing when no first match") {
        md.mapFirstIn() {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            h.copy(title = h.title.toUpperCase)
        }.mds shouldBe md.mds
        md.flatMapFirstIn() {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe md.mds
      }
      it("should help falling back when no first match") {
        md.mapFirstIn(ifNotFound = Seq(Header(1, "Four"))) {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            h.copy(title = h.title.toUpperCase)
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "FOUR")
        )
        md.flatMapFirstIn(ifNotFound = Seq(Header(1, "Four"))) {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "FOUR")
        )
        md.mapFirstIn(replace = true, ifNotFound = Seq(Header(1, "Four"))) {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            h.copy(title = h.title.toUpperCase)
        }.mds shouldBe Seq(Header(1, "FOUR"))
        md.flatMapFirstIn(replace = true, ifNotFound = Seq(Header(1, "Four"))) {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(Header(1, "FOUR"))
        md.mapFirstIn(ifNotFound = Header(1, "Four")) {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            h.copy(title = h.title.toUpperCase)
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "FOUR")
        )
        md.mapFirstIn(ifNotFound = Header(1, "Four") +: md.mds, replace = true) {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            h.copy(title = h.title.toUpperCase)
        }.mds shouldBe Seq(
          Header(1, "FOUR"),
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three")
        )
      }
    }

    describe("when matching on None to append") {
      it("should append on all matches") {
        md.replaceIn() { case (None, _) =>
          Seq(Header(1, "Four"))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "Four")
        )
      }
      it("should remove all but the element when filtering") {
        md.replaceIn(filter = true) { case (None, _) =>
          Seq(Header(1, "Four"))
        }.mds shouldBe Seq(Header(1, "Four"))
      }
    }

    describe("when matching on 0 to prepend") {
      it("should prepend on all matches") {
        md.replaceIn() { case (Some(md), 0) =>
          Seq(Header(1, "Zero"), md)
        }.mds shouldBe Seq(
          Header(1, "Zero"),
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three")
        )
      }
      it("should remove all but the element and the head when filtering") {
        md.replaceIn(filter = true) { case (Some(md), 0) =>
          Seq(Header(1, "Zero"), md)
        }.mds shouldBe Seq(Header(1, "Zero"), Header(1, "One"))
      }
    }

    describe("when prepending to a section header") {
      it("should put the new section after any existing elements") {
        // For a top level header
        Header(0, "A1").prepend("B1") shouldBe Header(0, "A1", Header(1, "B1"))
        Header(0, "A1", Comment("HI")).prepend("B1") shouldBe Header(0, "A1", Comment("HI"), Header(1, "B1"))
        // Skipped header levels are left at the top level, not added to the new section
        Header(0, "A1", Header(3, "HI")).prepend("B1") shouldBe Header(0, "A1", Header(3, "HI"), Header(1, "B1"))

        // For a nth level header
        Header(3, "A1").prepend("B1") shouldBe Header(3, "A1", Header(4, "B1"))
        Header(3, "A1", Comment("HI")).prepend("B1") shouldBe Header(3, "A1", Comment("HI"), Header(4, "B1"))
        Header(3, "A1", Header(3, "HI")).prepend("B1") shouldBe Header(3, "A1", Header(3, "HI"), Header(4, "B1"))
      }

      it("should put the new section before any other headers of the same level") {
        Header(0, "A1", Header(1, "B2")).prepend("B1") shouldBe Header(0, "A1", Header(1, "B1"), Header(1, "B2"))
        Header(0, "A1", Comment("HI"), Header(1, "B2"))
          .prepend("B1") shouldBe Header(0, "A1", Comment("HI"), Header(1, "B1"), Header(1, "B2"))

        // For a nth level header
        Header(3, "A1", Header(4, "B2")).prepend("B1") shouldBe Header(3, "A1", Header(4, "B1"), Header(4, "B2"))
        Header(3, "A1", Comment("HI"), Header(4, "B2"))
          .prepend("B1") shouldBe Header(3, "A1", Comment("HI"), Header(4, "B1"), Header(4, "B2"))
      }
    }

    describe("in a complicated internal match") {

      val md: Header = Header.parse("""
          !One
          !==============================================================================
          !
          !| A1 | A2 |
          !|----|----|
          !| 1  | 2  |
          !
          !| B1 | B2 |
          !|----|----|
          !| 10 | 20 |
          !
          !Two
          !==============================================================================
          !
          !| A1 | A2 |
          !|----|----|
          !| 1  | 2  |
          !
          !| B1 | B2 |
          !|----|----|
          !| 10 | 30 |
          !""".stripMargin('!'))

      it("should update the B1 table in the Two section") {

        // This is a complicated internal replacement: the first replacement finds
        // section Two and the second updates one specific table in the section
        val replaced = md.mapFirstIn() {
          // Matches the Two section and replace the contents inside
          case weekly @ Header(title, 1, _) if title.startsWith("Two") =>
            weekly.mapFirstIn() {
              // Matches the B1 table and updates it with our new table
              case tb @ Table(_, Seq(TableRow(Seq(tableName: String, _*)), _*)) if tableName == "B1" =>
                tb.updated(1, 1, "X")
            }
        }

        replaced.build().toString shouldBe
          """One
            !==============================================================================
            !
            !| A1 | A2 |
            !|----|----|
            !| 1  | 2  |
            !
            !| B1 | B2 |
            !|----|----|
            !| 10 | 20 |
            !
            !Two
            !==============================================================================
            !
            !| A1 | A2 |
            !|----|----|
            !| 1  | 2  |
            !
            !| B1 | B2 |
            !|----|----|
            !| 10 | X  |
            !""".stripMargin('!')
      }

      it("should find the sections") {
        val h1One = md.collectFirstRecursive { case h @ Header(_, 1, _) => h }
        h1One.value.title shouldBe "One"

        val h1Two = md.collectFirstRecursive {
          case h @ Header(_, 1, _) if h.title.startsWith("T") => h
        }
        h1Two.value.title shouldBe "Two"

        val tableB1 = md.collectFirstRecursive {
          case tbl: Table if tbl.title == "B1" => tbl
        }
        tableB1.value.build().toString shouldBe
          """| B1 | B2 |
            !|----|----|
            !| 10 | 20 |
            !""".stripMargin('!')

        val tableB12 = md.collectFirstRecursive {
          case tbl: Table if tbl.mds.exists(_.cells.contains("30")) =>
            tbl
        }
        tableB12.value.build().toString shouldBe
          """| B1 | B2 |
            !|----|----|
            !| 10 | 30 |
            !""".stripMargin('!')
      }

      it("should replace recursively with identity") {
        md.replaceRecursively { case tbl: Table => tbl } shouldBe md
      }

      it("should replace table rows recursively") {
        val replaced = md.replaceRecursively {
          case row: TableRow if row.head == "B1" => TableRow.from("C1", "C2")
        }

        replaced.build().toString shouldBe
          """One
            !==============================================================================
            !
            !| A1 | A2 |
            !|----|----|
            !| 1  | 2  |
            !
            !| C1 | C2 |
            !|----|----|
            !| 10 | 20 |
            !
            !Two
            !==============================================================================
            !
            !| A1 | A2 |
            !|----|----|
            !| 1  | 2  |
            !
            !| C1 | C2 |
            !|----|----|
            !| 10 | 30 |
            !""".stripMargin('!')
      }
    }
  }
}
