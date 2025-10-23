package com.tinfoiled.markd.ql

import com.tinfoiled.markd
import com.tinfoiled.markd._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[MarkdQL]] */
class MarkdQLSpec extends AnyFunSpecLike with Matchers {

  /** A basic queryable markdown */
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

  /** Top-level tables */
  val Table: Markd = Markd.parse("""
      !|Key | Value
      !|----|---
      !|K1  | V1
      !|K2  | V2
      !
      !|KeyX | Value
      !|-----|---
      !|Kx1  | Vx1
      !|Kx2  | Vx2
      !""".stripMargin('!'))

  /** Successful internal parsing test. This is helpful especially for debugging, but uses the exposed internals in
    * MarkdQL.
    * @param param
    *   The query string and the expected separator, token, index and rest
    */
  def itShouldParse(param: (String, (String, String, String, String))): Unit = {
    val (query, expected) = param
    it(s"should successfully parse: $query") {
      val q = MarkdQL.Query(rest = query).next
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
      case Seq()               => " (finding no match)"
      case Seq(Paragraph(txt)) => s" (finding '$txt')"
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

  /** Tests for a successful query with no result. */
  def itShouldQueryEmpty(query: String, md: MarkdNode): Unit = itShouldQuery(query, md -> Seq.empty)

  describe("When querying directly on the input") {
    itShouldQuery1(".", Basic -> Basic)
    itShouldQuery(".[*]", Basic -> Basic.mds)
    itShouldQuery("[*]", Basic -> Basic.mds)
    itShouldQuery1("[0]", Basic -> Basic.mds.head)
  }

  describe("When querying inside subheaders ") {
    itShouldQuery1("A.B.C", Basic -> Header(3, "C", Paragraph("Hello ABC")))
    itShouldQueryTxt("A.B.C[*]", Basic -> "Hello ABC")
    itShouldQueryTxt("A.B.C[0]", Basic -> "Hello ABC")
    itShouldQueryTxt("A.B.C2[*]", Basic -> "Hello ABC2")
    itShouldQueryTxt("A.B.C2[0]", Basic -> "Hello ABC2")

    itShouldQuery1("A..C", Basic -> Header(3, "C", Paragraph("Hello ABC")))
    itShouldQueryTxt("A..C[*]", Basic -> "Hello ABC")
    itShouldQueryTxt("A..C[0]", Basic -> "Hello ABC")
    itShouldQuery1("..C2", Basic -> Header(3, "C2", Paragraph("Hello ABC2")))
    itShouldQueryTxt("..C2[*]", Basic -> "Hello ABC2")
    itShouldQueryTxt("..C2[0]", Basic -> "Hello ABC2")
    itShouldQuery(
      "..B[*]",
      Basic -> List(
        Paragraph("Hello AB"),
        Header(3, "C", Paragraph("Hello ABC")),
        Header(3, "C2", Paragraph("Hello ABC2"))
      )
    )
    itShouldQueryTxt("..B[0]", Basic -> "Hello AB")
    itShouldQuery1("..B[1]", Basic -> Header(3, "C", Paragraph("Hello ABC")))
    itShouldQuery1("..B[2]", Basic -> Header(3, "C2", Paragraph("Hello ABC2")))
    itShouldQueryEmpty("..B[3]", Basic)

    // TODO: Countdown
//    itShouldQueryTxt("..B[-3]", Basic -> "Hello AB")
//    itShouldQuery1("..B[-2]", Basic -> Header(3, "C", Paragraph("Hello ABC")))
//    itShouldQuery1("..B[-1]", Basic -> Header(3, "C2", Paragraph("Hello ABC2")))
//    itShouldQueryEmpty("..B[-4]", Basic)
  }

  describe("When querying a table") {
    itShouldQueryTxt("..!To Do[Description,R2]", Basic -> "D2")
    itShouldQueryEmpty("..!To Do[X,R2]", Basic)
    itShouldQueryEmpty("..!To Do[Description,X]", Basic)

    itShouldQuery1("!Key", Table -> markd.Table(2, "Key", "Value", "K1", "V1", "K2", "V2"))
    itShouldQuery1("!Key[0]", Table -> TableRow("Key", "Value"))
    itShouldQuery1("!Key[1]", Table -> TableRow("K1", "V1"))
    itShouldQuery1("!Key[2]", Table -> TableRow("K2", "V2"))
    itShouldQueryEmpty("!Key[3]", Table)

    // TODO: Query cells
    // itShouldQueryTxt("!Key[0][0]", Table ->  "Key")

    for (tableQuery <- Seq("!Key[Value,K2]", ".!Key[Value,K2]", "..!Key[Value,K2]"))
      itShouldQueryTxt(tableQuery, Table -> "V2")

    for (tableQuery <- Seq("A2.!To Do", "A2!To Do", "A2[*]", "..!To Do"))
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
    itShouldQueryTxt("""..!"\"Price\""["In[,\\\]\\\\"]""", Basic -> "Cell")
  }

  describe("When a query doesn't match") {
    for (unmatched <- Seq("X", ".X", "X[*]", ".X[*]", ".A.X", ".A.B.X"))
      itShouldQueryEmpty(unmatched, Basic)
  }

  /** These tests test the internal query parsing as a check for debugging changes */
  describe("Internal MarkdQL query parsing") {
    itShouldParse("" -> ("", "", "", ""))
    itShouldParse("." -> ("", "", "", ""))
    itShouldParse(".[*]" -> ("", "", "*", ""))
    itShouldParse(".A[*]" -> ("", "A", "*", ""))
    itShouldParse("[*]" -> ("", "", "*", ""))
    itShouldParse("[0]" -> ("", "", "0", ""))
    itShouldParse("[0][1]" -> ("", "", "0", "[1]"))
    itShouldParse(".[stuff]" -> ("", "", "stuff", ""))
    itShouldParse("..B[0]" -> ("..", "B", "0", ""))
    itShouldParse("abc" -> ("", "abc", "", ""))
    itShouldParse(""".."abc".rest""" -> ("..", "abc", "", ".rest"))
    itShouldParse(""".."ab..c".rest""" -> ("..", "ab..c", "", ".rest"))
    itShouldParse(""".."!abc".rest""" -> ("..", "!abc", "", ".rest"))
    itShouldParse(""".."!ab..c".rest""" -> ("..", "!ab..c", "", ".rest"))

    describe("when given an invalid query") {
      itShouldFailToParse("...C")
      itShouldFailToParse("A...C")
      itShouldFailToParse("..C[")
      // itShouldFailToParse("A[*][*]")
      // itShouldFailToParse("..[*]")
      // itShouldFailToParse("..C..[*]")
      // itShouldFailToParse("..!To Do[rowonly]")
    }

    describe("when querying tables") {
      itShouldParse("""!A.rest""" -> ("!", "A", "", ".rest"))
      itShouldParse(""".!A.rest""" -> ("!", "A", "", ".rest"))
      itShouldParse("""..!A.rest""" -> ("..!", "A", "", ".rest"))
      itShouldParse("""!"!A!\"\\\x.x[".rest""" -> ("!", """!A!"\x.x[""", "", ".rest"))
      itShouldParse(""".!"!A!\"\\\x.x[".rest""" -> ("!", """!A!"\x.x[""", "", ".rest"))
      itShouldParse("""..!"!A!\"\\\x.x[".rest""" -> ("..!", """!A!"\x.x[""", "", ".rest"))

      // TODO: What should an empty token to?  Find an empty title?
      itShouldParse("![*]" -> ("!", "", "*", ""))
      itShouldFailToParse("!")
      itShouldFailToParse(".!")
      itShouldFailToParse("..!")
    }
  }
}
