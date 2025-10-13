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

  /** Tests for a successful query. */
  def itShouldQuery(query: String, md: (MarkdNode, Seq[MarkdNode]), msg: String = ""): Unit = {
    val qualifier = md._2 match {
      case Seq()               => " (no match)"
      case Seq(Paragraph(txt)) => s" (found '$txt')"
      case _                   => ""
    }

    it(if (msg.isBlank) s"should query: $query$qualifier" else msg) { MarkdQL.query(query, md._1) shouldBe md._2 }
  }

  /** Tests for a successful query with one result. */
  def itShouldQuery1(query: String, md: (MarkdNode, MarkdNode), msg: String = ""): Unit =
    itShouldQuery(query, md._1 -> Seq(md._2), msg)

  /** Tests for a successful query with one paragraph result. */
  def itShouldQueryTxt(query: String, md: (MarkdNode, String), msg: String = ""): Unit =
    itShouldQuery(query, md._1 -> Seq(Paragraph(md._2)), msg)

  /** Tests for a successful query with no result.
    * @param param
    *   The query string and the incoming node
    */
  def itShouldQueryEmpty(query: String, md: MarkdNode): Unit = itShouldQuery(query, md -> Seq.empty)

  describe("When querying directly on the input") {
    itShouldQuery1(".", Basic -> Basic)
    itShouldQuery(".[*]", Basic -> Basic.mds)
    itShouldQuery("[*]", Basic -> Basic.mds)
  }

  describe("When querying inside subheaders ") {
    itShouldQuery1("A.B.C", Basic -> Header(3, "C", Paragraph("Hello ABC")))
    itShouldQueryTxt("A.B.C[*]", Basic -> "Hello ABC")
    itShouldQueryTxt("A.B.C2[*]", Basic -> "Hello ABC2")

    itShouldQuery1("A..C", Basic -> Header(3, "C", Paragraph("Hello ABC")))
    itShouldQueryTxt("A..C[*]", Basic -> "Hello ABC")
    itShouldQuery1("..C2", Basic -> Header(3, "C2", Paragraph("Hello ABC2")))
    itShouldQueryTxt("..C2[*]", Basic -> "Hello ABC2")
    itShouldQuery(
      "..B[*]",
      Basic -> List(
        Paragraph("Hello AB"),
        Header(3, "C", Paragraph("Hello ABC")),
        Header(3, "C2", Paragraph("Hello ABC2"))
      )
    )
  }

  describe("When querying a table") {
    itShouldQueryTxt("..!To Do[Description,R2]", Basic -> "D2")
    itShouldQueryEmpty("..!To Do[X,R2]", Basic)
    itShouldQueryEmpty("..!To Do[Description,X]", Basic)

    for (tableQuery <- Seq("A2.!To Do", "A2[*]", "..!To Do"))
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

  describe("When quoting a query token") {
    itShouldQueryTxt("""."Find me [it's \"complicated\"]".Simple[*]""", Basic -> "Found")
    itShouldQueryTxt(""".."!\"Price\""["In[,\\\]\\\\"]""", Basic -> "Cell")
  }

  describe("When a query doesn't match") {

    for (unmatched <- Seq("X", ".X", "X[*]", ".X[*]", ".A.X", ".A.B.X"))
      itShouldQueryEmpty(unmatched, Basic)

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
