package com.tinfoiled.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Code]] */
class CodeSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing a Code block") {
    it("should find a standalone node") {
      val md = Markd.parse("""
          |```bash
          |echo Hello world
          |```
          |""".stripMargin)

      md shouldBe Markd(Code("bash", "echo Hello world\n"))

      val cleaned = md.build().toString
      cleaned shouldBe
        """```bash
          |echo Hello world
          |```
          |""".stripMargin
      Markd.parse(cleaned) shouldBe md
    }

    describe("and ignoring markdown inside a code block") {
      it("should ignore a header inside a code block") {
        val md = Markd.parse("""
            |```bash
            |# echo Hello world
            |```
            |""".stripMargin)
        md shouldBe Markd(Code("bash", "# echo Hello world\n"))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```bash
            |# echo Hello world
            |```
            |""".stripMargin
        Markd.parse(cleaned) shouldBe md
      }

      it("should ignore a comment inside a code block") {
        val md = Markd.parse("""
            |```bash
            |    <!--   comment  -->
            |```
            |""".stripMargin)
        md shouldBe Markd(Code("bash", "    <!--   comment  -->\n"))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```bash
            |    <!--   comment  -->
            |```
            |""".stripMargin
        Markd.parse(cleaned) shouldBe md
      }
    }

    it("should ignore unnecessary whitespace") {
      val md = Markd.parse("""
          |```bash.....
          |.....echo Hello world.....
          |```.....
        """.stripMargin.replace(".", " "))
      md shouldBe Markd(Code("bash", "     echo Hello world     \n"))

      val cleaned = md.build().toString
      cleaned shouldBe
        """```bash
          |.....echo Hello world.....
          |```
          |""".stripMargin.replace(".", " ")
      Markd.parse(cleaned) shouldBe md
    }

    it("should ignore a code block with bad whitespace") {
      val md = Markd.parse("""
          |   ```bash
          |echo Hello world
          |```
        """.stripMargin.replace(".", " "))
      // TODO: What do we expect here?  This is probably not what we want and cleaning breaks.
      // md shouldBe Markd(Paragraph("   ```bash\necho Hello world\n```"))

      val cleaned = md.build().toString
      cleaned shouldBe
        """``bash
          |echo Hello world
          |
          |``
          |""".stripMargin.replace(".", " ")
      Markd.parse(cleaned) shouldBe md
    }

    // TODO: This doesn't quite work.
    ignore("should ignore an internal code block") {
      val md = Markd.parse("""
          |echo ```Hello``` world
          |""".stripMargin)
      md shouldBe Markd(Paragraph("echo ```Hello``` world"))

      val cleaned = md.build().toString
      cleaned shouldBe """echo ```Hello``` world"""
      Markd.parse(cleaned) shouldBe md
    }

    it("should prettify a JSON code block") {
      val md = Markd.parse("""
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
      Markd.parse(cleaned).build().toString shouldBe cleaned
    }

    it("should not prettify a JSON code block with invalid JSON") {
      val md = Markd.parse("""
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
      Markd.parse(cleaned) shouldBe md
    }

    it("should prettify a jsonline code block") {
      val md = Markd.parse("""
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
      Markd.parse(cleaned).build().toString shouldBe cleaned
    }

    it("should prettify only the valid lines in jsonline or json line code blocks") {
      val md = Markd.parse("""
          |```jsonline
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
      Markd.parse(cleaned).build().toString shouldBe cleaned
    }
  }
}
