package com.tinfoiled.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Paragraph]]
  */
class ParagraphSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing a Paragraph") {

    it("should be empty if there aren't any contents") {
      val md = Doc.parse("     \t\n\n")
      md shouldBe Doc()

      val cleaned = md.build().toString
      cleaned shouldBe ""
      Doc.parse(cleaned) shouldBe md
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
          val md = Doc.parse(content)
          md shouldBe Doc(Paragraph(content.trim))

          val cleaned = md.build().toString
          cleaned shouldBe s"${content.trim}\n"
          Doc.parse(cleaned) shouldBe md
        }

        it("creates a simple paragraph in a section") {
          val md = Doc.parse(s"# Main\n$content")
          md shouldBe Doc(Header(1, "Main", Paragraph(content.trim)))

          val cleaned = md.build().toString
          cleaned shouldBe
            s"""Main
               |==============================================================================
               |
               |${content.trim}
               |""".stripMargin
          Doc.parse(cleaned) shouldBe md
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
          val md = Doc.parse(content)
          md shouldBe Doc(Paragraph("Hello"), Paragraph("world"))

          val cleaned = md.build().toString
          cleaned shouldBe
            s"""Hello
               |
               |world
               |""".stripMargin
          Doc.parse(cleaned) shouldBe md
        }

        it("creates paragraphs in a section") {
          val md = Doc.parse(s"# Main\n$content")
          md shouldBe Doc(Header(1, "Main", Paragraph("Hello"), Paragraph("world")))

          val cleaned = md.build().toString
          cleaned shouldBe
            s"""Main
               |==============================================================================
               |
               |Hello
               |
               |world
               |""".stripMargin
          Doc.parse(cleaned) shouldBe md
        }
      }
    }
  }
}
