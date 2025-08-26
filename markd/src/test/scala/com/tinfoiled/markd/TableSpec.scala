package com.tinfoiled.markd

import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Table]] and [[TableRow]]
  */
class TableSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing a markdown Table") {

    it("should parse lines into cells") {
      Table.parseRow("") shouldBe Seq()
      Table.parseRow("|") shouldBe Seq()
      Table.parseRow("||") shouldBe Seq()
      Table.parseRow("one|two|three") shouldBe Seq("one", "two", "three")
      Table.parseRow("|two|three") shouldBe Seq("", "two", "three")
      Table.parseRow("one||three") shouldBe Seq("one", "", "three")
      Table.parseRow("one|two|") shouldBe Seq("one", "two")
      Table.parseRow("one|two||||") shouldBe Seq("one", "two")
      Table.parseRow(raw"\||two|three") shouldBe Seq(raw"\|", "two", "three")
      Table.parseRow(raw"\||two|three||\|") shouldBe Seq(raw"\|", "two", "three", "", raw"\|")
      Table.parseRow(raw"one\||t\|wo|\|three") shouldBe Seq(raw"one\|", raw"t\|wo", raw"\|three")
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
      md shouldBe Table.from(Seq(Align.LEFT, Align.LEFT), TableRow.from("Id", "Name"), tb1, tb2, tb3)

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
      val md = Markd.parse("""Before
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
      Markd.parse(cleaned) shouldBe md
    }

    it("should detect column alignment") {
      val md = Markd.parse("""
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
      Markd.parse(cleaned) shouldBe md
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
        .parse("""!|   ||
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
}
