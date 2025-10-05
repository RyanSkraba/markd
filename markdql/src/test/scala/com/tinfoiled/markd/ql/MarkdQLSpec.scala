package com.tinfoiled.markd.ql

import com.tinfoiled.markd._
import com.tinfoiled.markd.ql.MarkdQL.query
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

    it("should return itself with '.'") { query(".", Basic) shouldBe List(Basic) }
    it("should return its children with '.[*]'") { query(".[*]", Basic) shouldBe Basic.mds }
    it("should return its children with '[*]'") { query("[*]", Basic) shouldBe Basic.mds }

    it("should find a paragraph 'A.B.C[*]'") { query("A.B.C[*]", Basic) shouldBe para("Hello ABC") }
    it("should find a paragraph 'A.B.C2[*]'") { query("A.B.C2[*]", Basic) shouldBe para("Hello ABC2") }

    it("should find a paragraph 'A..C[*]'") { query("A..C[*]", Basic) shouldBe para("Hello ABC") }
    it("should find a paragraph '..C[*]'") { query("..C[*]", Basic) shouldBe para("Hello ABC") }

    for (unmatched <- Seq("X", ".X", "X[*]", ".X[*]", ".A.X", ".A.B.X"))
      it(s"should return empty on unmatched path: '$unmatched'") {
        query(unmatched, Basic) shouldBe empty
      }

    it("do other queries") {
      Markd(query("A[*]", Basic): _*).build().toString shouldBe
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

      Markd(query("A.B[*]", Basic): _*).build().toString shouldBe
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

  describe("When given bad queries") {
    for (badQL <- Seq("A...C"))
      it(s"should throw an error on: $badQL") {
        intercept[RuntimeException](query(badQL, Basic)).getMessage shouldBe s"Unrecognized query: $badQL"
      }
  }

}
