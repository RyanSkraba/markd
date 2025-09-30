package com.tinfoiled.markd.ql

import com.tinfoiled.markd._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[MarkdQL]] */
class MarkdQLSpec extends AnyFunSpecLike with Matchers {

  val Basic: Markd = Markd.parse("""# A
      !## B
      !Hello AB
      !### C
      !Hello ABC
      !### C2
      !Hello ABC2
      !## B2
      !Hello AB2
      !""".stripMargin('!'))

  describe("A Basic MarkdQL query") {

    def para(in: String) = List(Paragraph(in))

    it("should return itself with '.'") { MarkdQL.query(".", Basic) shouldBe List(Basic) }
    it("should return its children with '.[*]'") { MarkdQL.query(".[*]", Basic) shouldBe Basic.mds }
    it("should return its children with '[*]'") { MarkdQL.query("[*]", Basic) shouldBe Basic.mds }

    it("should find a paragraph 'A.B.C[*]'") { MarkdQL.query("A.B.C[*]", Basic) shouldBe para("Hello ABC") }
    it("should find a paragraph 'A.B.C2[*]'") { MarkdQL.query("A.B.C2[*]", Basic) shouldBe para("Hello ABC2") }

    for (unmatched <- Seq("X", ".X", "X[*]", ".X[*]", ".A.X", ".A.B.X"))
      it(s"should return empty on unmatched path: '$unmatched'") {
        MarkdQL.query(unmatched, Basic) shouldBe empty
      }

    it("do other queries") {
      Markd(MarkdQL.query("A[*]", Basic): _*).build().toString shouldBe
        """B
          |------------------------------------------------------------------------------
          |
          |Hello AB
          |
          |### C
          |
          |Hello ABC
          |
          |### C2
          |
          |Hello ABC2
          |
          |B2
          |------------------------------------------------------------------------------
          |
          |Hello AB2
          |""".stripMargin

      Markd(MarkdQL.query("A.B[*]", Basic): _*).build().toString shouldBe
        """Hello AB
          |
          |### C
          |
          |Hello ABC
          |
          |### C2
          |
          |Hello ABC2
          |""".stripMargin
    }

  }
}
