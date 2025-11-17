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
      !
      !# Find me [it's "complicated"]
      !## Simple
      !Found
      !## Table
      !| "Price" | In[
      !|------|---------
      !|\]\\  | Cell
      !
      !# Regex
      !Examples to use for trying regexes
      !## X
      !## XX
      !## XZZZZ
      !## Tables
      !
      !X|Found X
      !---|---
      !
      !XX|Found XX
      !---|---
      !
      !XZZZZ|Found XZZZZ
      !---|---
      !
      !# Code blocks
      !
      !```json
      !"one"
      !```
      !
      !```
      !Hello world!
      !```
      !
      !```bash
      !ls
      !```
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

  /** Markdown with different types */
  val DifferentTypes: Markd = Markd.parse("""
      !# A Markdown document with various nodes
      !paragraph 1
      !```
      !code block 1
      !```
      !```json
      !"code block 2"
      !```
      !| Table | Column|
      !|---|---|
      !| A1 |B1|
      !|A2|B2|
      !```json
      !"code block 3"
      !```
      !paragraph 2
      !## header2 1
      !## header2 2
      !""".stripMargin('!'))

  /** Successful internal parsing test. This is helpful especially for debugging, but uses the exposed internals in
    * MarkdQL.
    * @param param
    *   The query string and the expected query properties including separator, token, index and rest
    * @param regex
    *   The expected regex property
    * @param recursive
    *   The expected regex property
    */
  def itShouldParse(
      param: (String, (String, String, String, String)),
      regex: Boolean = false,
      recursive: Boolean = false
  ): Unit = {
    val (query, expected) = param
    it(s"should successfully parse: $query") {
      val q = MarkdQL.Query(rest = query).next
      (q.regex, q.recursive) -> (q.separator, q.token, q.index, q.rest) shouldBe (regex, recursive) -> expected
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

    // Some goofiness with quotes and separators
    itShouldQuery1(""""A"."B"."C"""", Basic -> Header(3, "C", Paragraph("Hello ABC")))
    itShouldQuery1(""""A""B""C"""", Basic -> Header(3, "C", Paragraph("Hello ABC")))

    // Nesting indices to find leaf nodes is possible, but not very much fun!
    itShouldQueryTxt("A.B.[0]", Basic -> "Hello AB")
    itShouldQueryTxt("A.B[0]", Basic -> "Hello AB")
    itShouldQueryTxt("[0][0][0]", Basic -> "Hello AB")
    itShouldQueryTxt("A[0][0]", Basic -> "Hello AB")
    itShouldQueryTxt("[0]B[0]", Basic -> "Hello AB")
    itShouldQueryTxt("[0].B[0]", Basic -> "Hello AB")
    itShouldQueryTxt("[0].B.[0]", Basic -> "Hello AB")

    describe("and querying with regex") {
      for (prefix <- Seq("Regex.", "..")) {
        itShouldQuery1(s"$prefix/X/", Basic -> Header(2, "X"))
        itShouldQuery1(s"$prefix/X./", Basic -> Header(2, "XX"))
        itShouldQuery1(s"$prefix/X.*/", Basic -> Header(2, "X"))
        itShouldQueryEmpty(s"$prefix/XZ/", Basic)
        itShouldQueryEmpty(s"$prefix/ZZ/", Basic)
        itShouldQuery1(s"$prefix/XZ+/", Basic -> Header(2, "XZZZZ"))
        itShouldQuery1(s"$prefix/^XZ+$$/", Basic -> Header(2, "XZZZZ"))
        itShouldQuery1(s"$prefix/.*ZZ.*/", Basic -> Header(2, "XZZZZ"))
      }
    }

    describe("and querying by index") {
      itShouldQueryTxt("[0][0][1][0][0]", Basic -> "Hello ABC")
      itShouldQueryTxt("[0][0][1][0][*]", Basic -> "Hello ABC")
      itShouldQueryTxt("[0][0][2][0][0]", Basic -> "Hello ABC2")
      itShouldQueryTxt("[0][0][2][0][*]", Basic -> "Hello ABC2")
      itShouldQueryTxt("A[0]C[0]", Basic -> "Hello ABC")
      itShouldQueryTxt("A[0]C2[0]", Basic -> "Hello ABC2")
      itShouldQueryTxt("A[0]C2[*]", Basic -> "Hello ABC2")
      itShouldQueryTxt("[0][1][0]", Basic -> "Hello AB2")
      itShouldQueryTxt("[2][0][0]", Basic -> "Found")
    }

    // TODO: This should probably be an error or return empty
    itShouldQueryTxt("A[0]C2[0][0][0][0][0][0][0][0][0][0]", Basic -> "Hello ABC2")

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

    // With negative indices, count from the end
    itShouldQueryTxt("..B[-3]", Basic -> "Hello AB")
    itShouldQuery1("..B[-2]", Basic -> Header(3, "C", Paragraph("Hello ABC")))
    itShouldQuery1("..B[-1]", Basic -> Header(3, "C2", Paragraph("Hello ABC2")))
    itShouldQueryEmpty("..B[-4]", Basic)
  }

  describe("When querying a code block") {
    itShouldQuery1("Code blocks`bash", Basic -> Code("bash", "ls\n"))
    itShouldQuery1("Code blocks.`bash", Basic -> Code("bash", "ls\n"))
    itShouldQuery1("Code blocks..`bash", Basic -> Code("bash", "ls\n"))
    itShouldQuery1("Code blocks`json", Basic -> Code("json", "\"one\"\n"))
    itShouldQuery1("Code blocks`/.*sh/", Basic -> Code("bash", "ls\n"))
    itShouldQuery1("Code blocks`/.*/", Basic -> Code("json", "\"one\"\n"))
    itShouldQuery1("Code blocks`\"\"", Basic -> Code("", "Hello world!\n"))
  }

  describe("When querying a table") {
    itShouldQueryTxt("..|To Do[Description,R2]", Basic -> "D2")
    itShouldQueryEmpty("..|To Do[X,R2]", Basic)
    itShouldQueryEmpty("..|To Do[Description,X]", Basic)

    describe("and querying with regex") {
      for (prefix <- Seq("Regex.Tables", "Regex.Tables.", "Regex..", "..")) {
        itShouldQueryTxt(s"$prefix|/X/[0][1]", Basic -> "Found X")
        itShouldQueryTxt(s"$prefix|/X./[0][1]", Basic -> "Found XX")
        itShouldQueryTxt(s"$prefix|/X.*/[0][1]", Basic -> "Found X")
        itShouldQueryEmpty(s"$prefix|/XZ/[0][1]", Basic)
        itShouldQueryEmpty(s"$prefix|/ZZ/[0][1]", Basic)
        itShouldQueryTxt(s"$prefix|/XZ+/[0][1]", Basic -> "Found XZZZZ")
        itShouldQueryTxt(s"$prefix|/^XZ+$$/[0][1]", Basic -> "Found XZZZZ")
        itShouldQueryTxt(s"$prefix|/.*ZZ.*/[0][1]", Basic -> "Found XZZZZ")
      }
    }

    itShouldQuery1("|Key", Table -> markd.Table(2, "Key", "Value", "K1", "V1", "K2", "V2"))
    itShouldQuery1("|Key[0]", Table -> TableRow("Key", "Value"))
    itShouldQuery1("|Key[1]", Table -> TableRow("K1", "V1"))
    itShouldQuery1("|Key[2]", Table -> TableRow("K2", "V2"))
    itShouldQueryEmpty("|Key[3]", Table)
    itShouldQuery1("|Key[-3]", Table -> TableRow("Key", "Value"))
    itShouldQuery1("|Key[-2]", Table -> TableRow("K1", "V1"))
    itShouldQuery1("|Key[-1]", Table -> TableRow("K2", "V2"))
    itShouldQueryEmpty("|Key[-4]", Table)

    describe("when querying cells") {
      itShouldQueryTxt("|Key[0][0]", Table -> "Key")
      itShouldQueryTxt("|Key[0][1]", Table -> "Value")
      itShouldQueryTxt("|Key[0][-2]", Table -> "Key")
      itShouldQueryTxt("|Key[0][-1]", Table -> "Value")
      itShouldQueryTxt("|Key[1][0]", Table -> "K1")
      itShouldQueryTxt("|Key[1][1]", Table -> "V1")
      itShouldQueryTxt("|Key[1][-2]", Table -> "K1")
      itShouldQueryTxt("|Key[1][-1]", Table -> "V1")
      itShouldQueryTxt("|Key[-1][0]", Table -> "K2")
      itShouldQueryTxt("|Key[-1][1]", Table -> "V2")
      itShouldQueryTxt("|Key[-1][-2]", Table -> "K2")
      itShouldQueryTxt("|Key[-1][-1]", Table -> "V2")

      for (row <- -4 to 3; column <- -3 to 2 if row <= -4 || row >= 3 || column <= -4 || column >= 2)
        itShouldQueryEmpty(s"|Key[$row][$column]", Table)
    }

    for (tableQuery <- Seq("|Key[Value,K2]", ".|Key[Value,K2]", "..|Key[Value,K2]"))
      itShouldQueryTxt(tableQuery, Table -> "V2")

    for (tableQuery <- Seq("A2.|To Do", "A2|To Do", "A2[*]", "..|To Do"))
      it(s"should find a table: '$tableQuery'") {
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

  describe("When filtering on node types") {
    // TODO: This is probably not what we really want
    itShouldQuery(
      """.[0][*][code]""",
      DifferentTypes -> Seq(
        Code("", "code block 1\n"),
        Code("json", "\"code block 2\"\n"),
        Code("json", "\"code block 3\"\n")
      )
    )
    // TODO itShouldQuery(""".[0][code]""", ...
    // TODO itShouldQuery("""..[code]""", ...
    // TODO itShouldQuery("""..[code`json]""", ...
  }

  describe("When quoting a query token") {
    itShouldQueryTxt("""."Find me [it's \"complicated\"]".Simple[*]""", Basic -> "Found")
    itShouldQueryTxt("""..|"\"Price\""["In[,\\\]\\\\"]""", Basic -> "Cell")
  }

  describe("When a query doesn't match") {
    for (unmatched <- Seq("X", ".X", "X[*]", ".X[*]", ".A.X", ".A.B.X"))
      itShouldQueryEmpty(unmatched, Basic)
  }

  /** These tests test the internal query parsing as a check for debugging changes */
  describe("Internal MarkdQL query parsing") {
    itShouldParse("" -> ("", "", "", ""))
    itShouldParse("\"\"" -> ("", "", "", ""))
    itShouldParse(".\"\"[\"\"]" -> ("", "", "", ""))
    itShouldParse("." -> ("", "", "", ""))
    itShouldParse(".[*]" -> ("", "", "*", ""))
    itShouldParse(".A[*]" -> ("", "A", "*", ""))
    itShouldParse("[*]" -> ("", "", "*", ""))
    itShouldParse("[0]" -> ("", "", "0", ""))
    itShouldParse("[0][1]" -> ("", "", "0", "[1]"))
    itShouldParse(".[stuff]" -> ("", "", "stuff", ""))
    itShouldParse("..B[0]" -> ("", "B", "0", ""), recursive = true)
    itShouldParse("abc" -> ("", "abc", "", ""))
    itShouldParse("a\"bc" -> ("", "a\"bc", "", ""))
    itShouldParse("\"abc\"" -> ("", "abc", "", ""))
    itShouldParse(""".."abc".rest""" -> ("", "abc", "", ".rest"), recursive = true)
    itShouldParse(""".."ab..c".rest""" -> ("", "ab..c", "", ".rest"), recursive = true)
    itShouldParse(""".."|abc".rest""" -> ("", "|abc", "", ".rest"), recursive = true)
    itShouldParse(""".."|ab..c".rest""" -> ("", "|ab..c", "", ".rest"), recursive = true)
    itShouldParse("""..`"abc`"[0].rest""" -> ("`", "abc`", "0", ".rest"), recursive = true)

    describe("when finding regex matches") {
      itShouldParse("/abc/" -> ("", "abc", "", ""), regex = true)
      itShouldParse("/abc//def/" -> ("", "abc", "", "/def/"), regex = true)
      itShouldParse("/abc/[/def/]/ghi/" -> ("", "abc", "/def/", "/ghi/"), regex = true)
      itShouldParse("../abc/" -> ("", "abc", "", ""), regex = true, recursive = true)
      itShouldParse("../a\"bc/" -> ("", "a\"bc", "", ""), regex = true, recursive = true)
      itShouldParse("/a\\/bc/" -> ("", "a/bc", "", ""), regex = true)
    }

    describe("when given an invalid query") {
      itShouldFailToParse("...C")
      itShouldFailToParse("A...C")
      itShouldFailToParse("..C[")
      // itShouldFailToParse("A[*][*]")
      // itShouldFailToParse("..[*]")
      // itShouldFailToParse("..C..[*]")
      // itShouldFailToParse("..|To Do[rowonly]")
    }

    describe("when querying tables") {
      itShouldParse("""|A.rest""" -> ("|", "A", "", ".rest"))
      itShouldParse(""".|A.rest""" -> ("|", "A", "", ".rest"))
      itShouldParse("""..|A.rest""" -> ("|", "A", "", ".rest"), recursive = true)
      itShouldParse("""|"|A|\"\\\x.x[".rest""" -> ("|", """|A|"\x.x[""", "", ".rest"))
      itShouldParse(""".|"|A|\"\\\x.x[".rest""" -> ("|", """|A|"\x.x[""", "", ".rest"))
      itShouldParse("""..|"|A|\"\\\x.x[".rest""" -> ("|", """|A|"\x.x[""", "", ".rest"), recursive = true)

      // TODO: What should an empty token to?  Find an empty title?
      itShouldParse("|[*]" -> ("|", "", "*", ""))
      itShouldFailToParse("|")
      itShouldFailToParse(".|")
      itShouldFailToParse("..|")
    }
  }
}
