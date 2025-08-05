package com.tinfoiled.markd

import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Code]]
  */
class CodeSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing a Code block") {
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
}
