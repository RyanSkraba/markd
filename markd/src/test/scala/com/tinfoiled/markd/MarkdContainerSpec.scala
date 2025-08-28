package com.tinfoiled.markd

import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[MarkdContainer]] nodes
  */
class MarkdContainerSpec extends AnyFunSpecLike with Matchers {

  describe("Replacing children") {
    val md: Markd = Markd.parse("""
        |# One
        |# Two
        |# Three
        |""".stripMargin)

    describe("replacing a match one-to-one with another node") {
      it("should replace all matches") {
        md.replaceIn() {
          case (Some(h @ Header(1, title, _*)), _) if title.startsWith("T") => Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(Header(1, "One"), Header(1, "TWO"), Header(1, "THREE"))
      }
      it("should replace all matches with filtering") {
        md.replaceIn(filter = true) {
          case (Some(h @ Header(1, title, _*)), _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(Header(1, "TWO"), Header(1, "THREE"))
      }
      it("should replace the first matching") {
        md.mapFirstIn() {
          case h @ Header(1, title, _*) if title.startsWith("T") => h.copy(title = h.title.toUpperCase)
        }.mds shouldBe Seq(Header(1, "One"), Header(1, "TWO"), Header(1, "Three"))
        // The equivalent
        md.flatMapFirstIn() {
          case h @ Header(1, title, _*) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(Header(1, "One"), Header(1, "TWO"), Header(1, "Three"))
      }
    }

    describe("removing a match") {
      it("should replace all matches") {
        md.replaceIn() { case (Some(Header(1, title, _*)), _) if title.startsWith("T") => Seq.empty }.mds shouldBe Seq(
          Header(1, "One")
        )
      }
      it("should replace all matches with filtering") {
        // This isn't very useful
        md.replaceIn(filter = true) { case (Some(Header(1, title, _*)), _) if title.startsWith("T") => Seq.empty }
          .mds shouldBe Seq.empty
      }
      it("should replace the first") {
        md.flatMapFirstIn() { case Header(1, title, _*) if title.startsWith("T") => Seq.empty }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Three")
        )
      }
    }

    describe("inserting after a match") {
      it("should replace all matches") {
        md.replaceIn() {
          case (Some(h @ Header(1, title, _*)), _) if title.startsWith("T") =>
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
          case (Some(h @ Header(1, title, _*)), _) if title.startsWith("T") =>
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
          case h @ Header(1, title, _*) if title.startsWith("T") => Seq(h, h.copy(title = h.title.toUpperCase))
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
          case (Some(h @ Header(1, title, _*)), _) if title.startsWith("F") => Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe md.mds
      }
      it("should remove all when filtering") {
        md.replaceIn(filter = true) {
          case (Some(h @ Header(1, title, _*)), _) if title.startsWith("F") => Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq.empty
      }
      it("should do nothing when no first match") {
        md.mapFirstIn() {
          case h @ Header(1, title, _*) if title.startsWith("F") => h.copy(title = h.title.toUpperCase)
        }.mds shouldBe md.mds
        md.flatMapFirstIn() {
          case h @ Header(1, title, _*) if title.startsWith("F") => Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe md.mds
      }
      it("should help falling back when no first match") {
        md.mapFirstIn(ifNotFound = Seq(Header(1, "Four"))) {
          case h @ Header(1, title, _*) if title.startsWith("F") => h.copy(title = h.title.toUpperCase)
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "FOUR")
        )
        md.flatMapFirstIn(ifNotFound = Seq(Header(1, "Four"))) {
          case h @ Header(1, title, _*) if title.startsWith("F") => Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "FOUR")
        )
        md.mapFirstIn(replace = true, ifNotFound = Seq(Header(1, "Four"))) {
          case h @ Header(1, title, _*) if title.startsWith("F") => h.copy(title = h.title.toUpperCase)
        }.mds shouldBe Seq(Header(1, "FOUR"))
        md.flatMapFirstIn(replace = true, ifNotFound = Seq(Header(1, "Four"))) {
          case h @ Header(1, title, _*) if title.startsWith("F") => Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(Header(1, "FOUR"))
        md.mapFirstIn(ifNotFound = Header(1, "Four")) {
          case h @ Header(1, title, _*) if title.startsWith("F") => h.copy(title = h.title.toUpperCase)
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "FOUR")
        )
        md.mapFirstIn(ifNotFound = Header(1, "Four") +: md.mds, replace = true) {
          case h @ Header(1, title, _*) if title.startsWith("F") => h.copy(title = h.title.toUpperCase)
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
        md.replaceIn() { case (None, _) => Seq(Header(1, "Four")) }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "Four")
        )
      }
      it("should remove all but the node when filtering") {
        md.replaceIn(filter = true) { case (None, _) => Seq(Header(1, "Four")) }.mds shouldBe Seq(Header(1, "Four"))
      }
    }

    describe("when matching on 0 to prepend") {
      it("should prepend on all matches") {
        md.replaceIn() { case (Some(md), 0) => Seq(Header(1, "Zero"), md) }.mds shouldBe Seq(
          Header(1, "Zero"),
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three")
        )
      }
      it("should remove all but the node and the head when filtering") {
        md.replaceIn(filter = true) { case (Some(md), 0) => Seq(Header(1, "Zero"), md) }.mds shouldBe Seq(
          Header(1, "Zero"),
          Header(1, "One")
        )
      }
    }

    describe("when prepending to a section header") {
      it("should put the new section after any existing nodes") {
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

      val md: Markd = Markd.parse("""
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
          case weekly @ Header(1, title, _*) if title.startsWith("Two") =>
            weekly.mapFirstIn() {
              // Matches the B1 table and updates it with our new table
              case tb: Table if tb.title == "B1" => tb.updated(1, 1, "X")
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
        val h1One = md.collectFirstRecursive { case h @ Header(1, _, _*) => h }
        h1One.value.title shouldBe "One"

        val h1Two = md.collectFirstRecursive { case h @ Header(1, _, _*) if h.title.startsWith("T") => h }
        h1Two.value.title shouldBe "Two"

        val tableB1 = md.collectFirstRecursive { case tbl: Table if tbl.title == "B1" => tbl }
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
        val replaced = md.replaceRecursively { case row: TableRow if row.head == "B1" => TableRow("C1", "C2") }

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

  describe("Scaladoc examples") {

    val table = Table
      .parse("""ID | Name
               !---|----
               !1  | One
               !2  | Two
               !3  | Three
               !""".stripMargin('!'))
      .value

    it("for replaceIn") {
      val replaced = table.replaceIn() {
        // Leave the header row the same (always row 0)
        case (Some(header), 0) => Seq(header)
        // Any rows with "1" in the first cell should be deleted
        case (Some(TableRow("1", _*)), _) => Seq.empty
        // Every other row is doubled
        case (Some(row), _) => Seq(row, row)
        // And we add a new last row.
        case (None, _) => Seq(TableRow("END"))
      }

      replaced.build().toString shouldBe
        """| ID  | Name  |
          !|-----|-------|
          !| 2   | Two   |
          !| 2   | Two   |
          !| 3   | Three |
          !| 3   | Three |
          !| END |       |
          !""".stripMargin('!')
    }
  }
}
