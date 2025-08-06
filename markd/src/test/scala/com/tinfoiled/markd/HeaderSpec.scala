package com.tinfoiled.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Header]]
  */
class HeaderSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing markdown into Header sections") {

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
      h1.mds(2) shouldBe Header(2, "header1b", Paragraph("h1btxt"), Header(3, "header1b1", Paragraph("h1b1txt")))

      h2.mds should have size 4
      h2.mds.head shouldBe Paragraph("h2txt")
      h2.mds(1) shouldBe LinkRef("ref2", "https://www.ref2.com")
      h2.mds(2) shouldBe Header(2, "header2a", Paragraph("h2atxt"))
      h2.mds(3) shouldBe Header(2, "header2b", Paragraph("h2btxt"), LinkRef("ref2b", "https://www.ref2b.com"))
    }
  }
}
