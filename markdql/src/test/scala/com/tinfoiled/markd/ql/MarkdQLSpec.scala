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
      !# A2
      !|To Do | Description
      !|------|---------
      !|R1    | D1
      !|R2    | D2
      !|R3    | D3
      !|R4    | D4
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

    it("should find all contents 'A[*]'") {
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
    }

    it("should find all contents 'A.B[*]'") {
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

    for (tableQuery <- Seq("A2.!To Do", "..!To Do"))
      it(s"should find a table: '${tableQuery}'") {
        Markd(query(tableQuery, Basic): _*).build().toString shouldBe
          """| To Do | Description |
          !|-------|-------------|
          !| R1    | D1          |
          !| R2    | D2          |
          !| R3    | D3          |
          !| R4    | D4          |
          !""".stripMargin('!')
      }

    it("should find a table value: '..!To Do[Description,R2]") {
      query("..!To Do[Description,R2]", Basic) shouldBe List(Paragraph("D2"))
    }

    it("should not find a table value: '..!To Do[X,R2]") {
      // TODO: Should this distinguish between empty and not found?
      query("..!To Do[X,R2]", Basic) shouldBe List(Paragraph(""))
    }

    it("should not find a table value: '..!To Do[Description,X]") {
      // TODO: Should this distinguish between empty and not found?
      query("..!To Do[Description,X]", Basic) shouldBe List(Paragraph(""))
    }
  }

  // A[*][*]
  // ..[*]
  // ..C..[*]
  // ..!To Do[rowonly]
  describe("When given bad queries") {
    for (badQL <- Seq("A...C"))
      it(s"should throw an error on: $badQL") {
        intercept[RuntimeException](query(badQL, Basic)).getMessage shouldBe s"Unrecognized query: $badQL"
      }
  }
}
