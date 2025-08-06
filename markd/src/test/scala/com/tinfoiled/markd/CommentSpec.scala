package com.tinfoiled.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Comment]]
  */
class CommentSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing a Comment") {
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
}
