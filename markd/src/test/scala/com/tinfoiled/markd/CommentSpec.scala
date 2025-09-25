package com.tinfoiled.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Comment]] */
class CommentSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing a Comment") {
    it("should find a standalone node") {
      val md = Markd.parse("<!-- Hello world -->")
      md shouldBe Markd(Comment(" Hello world "))

      val cleaned = md.build().toString
      cleaned shouldBe "<!-- Hello world -->\n"
      Markd.parse(cleaned) shouldBe md
    }

    it("should ignore unnecessary whitespace") {
      val md = Markd.parse("\n    \t<!-- Hello\n\tworld -->\n    \t")
      md shouldBe Markd(Comment(" Hello\n\tworld "))

      val cleaned = md.build().toString
      cleaned shouldBe "<!-- Hello\n\tworld -->\n"
      Markd.parse(cleaned) shouldBe md
    }

    it("should separate it carefully from other nodes") {
      val md = Markd.parse("Hello\t<!-- Hello\n\tworld -->     world")
      md shouldBe Markd(
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
      Markd.parse(cleaned) shouldBe md
    }
  }
}
