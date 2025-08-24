package com.tinfoiled.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Comment]]
  */
class CommentSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing a Comment") {
    it("should find a standalone element") {
      val md = Doc.parse("<!-- Hello world -->")
      md shouldBe Doc(Comment(" Hello world "))

      val cleaned = md.build().toString
      cleaned shouldBe "<!-- Hello world -->\n"
      Doc.parse(cleaned) shouldBe md
    }

    it("should ignore unnecessary whitespace") {
      val md = Doc.parse("\n    \t<!-- Hello\n\tworld -->\n    \t")
      md shouldBe Doc(Comment(" Hello\n\tworld "))

      val cleaned = md.build().toString
      cleaned shouldBe "<!-- Hello\n\tworld -->\n"
      Doc.parse(cleaned) shouldBe md
    }

    it("should separate it carefully from other elements") {
      val md = Doc.parse("Hello\t<!-- Hello\n\tworld -->     world")
      md shouldBe Doc(
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
      Doc.parse(cleaned) shouldBe md
    }
  }
}
