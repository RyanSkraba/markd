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
      !# A2
      !|To Do | Description
      !|------|---------
      !|R1    | D1
      !|R2    | D2
      !|R3    | D3
      !|R4    | D4
      !# Find me [it's "complicated"]
      !## Simple
      !Found
      !## Table
      !| "Price" | In[
      !|------|---------
      !|\]\\  | Cell
      !""".stripMargin('!'))

  /** Successful internal parsing test. This is helpful especially for debugging, but uses the exposed internals in
    * MarkdQL.
    * @param param
    *   The query string and the expected separator, token, index and rest
    */
  def itShouldParse(param: (String, (String, String, String, String))): Unit = {
    val (query, expected) = param
    it(s"should successfully parse: $query") {
      val q = MarkdQL.Query(query)
      (q.separator, q.token, q.index, q.rest) shouldBe expected
    }
  }

  /** Tests for failed internal parsing.
    * @param query
    *   The invalid query string
    */
  def itShouldFailToParse(query: String): Unit = {
    it(s"should fail to parse: $query") {
      intercept[RuntimeException](MarkdQL.query(query, Basic)).getMessage shouldBe s"Unrecognized query: $query"
    }
  }

  /** Tests for a successful query.
    * @param param
    *   The query string and the incoming node, and the expected result nodes
    */
  def itShouldQuery(param: ((String, MarkdNode), Seq[MarkdNode])): Unit = {
    val ((query, md), expected) = param
    val qualifier = expected match {
      case Seq()               => " (no match)"
      case Seq(Paragraph(txt)) => s" (found '$txt')"
      case _                   => ""
    }

    it(s"should query: $query$qualifier") { MarkdQL.query(query, md) shouldBe expected }
  }

  /** Tests for a successful query with one result.
    * @param param
    *   The query string and the incoming node, and a single expected result node
    */
  def itShouldQuery1(param: ((String, MarkdNode), MarkdNode)): Unit = itShouldQuery(param._1 -> Seq(param._2))

  /** Tests for a successful query with one paragraph result.
    * @param param
    *   The query string and the incoming node, and a single expected string
    */
  def itShouldQueryTxt(param: ((String, MarkdNode), String)): Unit = itShouldQuery(
    param._1 -> Seq(Paragraph(param._2))
  )

  /** Tests for a successful query with no result.
    * @param param
    *   The query string and the incoming node
    */
  def itShouldQueryEmpty(query: String, md: MarkdNode): Unit = itShouldQuery((query, md) -> Seq.empty)

  describe("A Basic MarkdQL query") {

    itShouldQuery1((".", Basic) -> Basic)
    itShouldQuery((".[*]", Basic) -> Basic.mds)
    itShouldQuery(("[*]", Basic) -> Basic.mds)

    itShouldQueryTxt(("A.B.C[*]", Basic) -> "Hello ABC")
    itShouldQueryTxt(("A.B.C2[*]", Basic) -> "Hello ABC2")

    itShouldQueryTxt(("A..C[*]", Basic) -> "Hello ABC")
    itShouldQueryTxt(("..C2[*]", Basic) -> "Hello ABC2")

    itShouldQueryTxt(("..!To Do[Description,R2]", Basic) -> "D2")

    itShouldQueryEmpty("..!To Do[X,R2]", Basic)
    itShouldQueryEmpty("..!To Do[Description,X]", Basic)

    itShouldQueryTxt(("""."Find me [it's \"complicated\"]".Simple[*]""", Basic) -> "Found")
    itShouldQueryTxt((""".."!\"Price\""["In[,\\\]\\\\"]""", Basic) -> "Cell")

    for (unmatched <- Seq("X", ".X", "X[*]", ".X[*]", ".A.X", ".A.B.X"))
      itShouldQueryEmpty(unmatched, Basic)

    it("should find all contents 'A[*]'") {
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
    }

    it("should find all contents 'A.B[*]'") {
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

    for (tableQuery <- Seq("A2.!To Do", "..!To Do"))
      it(s"should find a table: '${tableQuery}'") {
        Markd(MarkdQL.query(tableQuery, Basic): _*).build().toString shouldBe
          """| To Do | Description |
          !|-------|-------------|
          !| R1    | D1          |
          !| R2    | D2          |
          !| R3    | D3          |
          !| R4    | D4          |
          !""".stripMargin('!')
      }
  }

  describe("Internal MarkdQL query parsing") {
    itShouldParse("." -> ("", "", "", ""))
    itShouldParse(".[*]" -> (".", "", "*", ""))
    itShouldParse(".A[*]" -> (".", "A", "*", ""))
    itShouldParse("[*]" -> ("", "", "*", ""))
    itShouldParse("[0]" -> ("", "", "0", ""))
    itShouldParse(".[stuff]" -> (".", "", "stuff", ""))
    itShouldParse("..B[0]" -> ("..", "B", "0", ""))
    itShouldParse("abc" -> ("", "abc", "", ""))
    itShouldParse(""".."abc".rest""" -> ("..", "abc", "", ".rest"))
    itShouldParse(""".."ab..c".rest""" -> ("..", "ab..c", "", ".rest"))
    itShouldParse(""".."!abc".rest""" -> ("..", "!abc", "", ".rest"))
    itShouldParse(""".."!ab..c".rest""" -> ("..", "!ab..c", "", ".rest"))
    itShouldFailToParse("...C")
    itShouldFailToParse("A...C")
    // itShouldFailToParse("A[*][*]")
    // itShouldFailToParse("..[*]")
    // itShouldFailToParse("..C..[*]")
    // itShouldFailToParse("..!To Do[rowonly]")

  }
}
