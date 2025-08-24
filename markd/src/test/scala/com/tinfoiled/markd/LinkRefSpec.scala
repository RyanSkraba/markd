package com.tinfoiled.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[LinkRef]]
  */
class LinkRefSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing LinkRefs") {
    val linkrefs = """
        |[ref-bare]:
        |[dup]: dup
        |[dup]: dup "dup"
        |[url]: url
        |[url-prews]: url-prews
        |[url-postws]: url-postws
        |[title]: "title"
        |[title-prews]: "title-prews"
        |[title-postws]: "title-postws"
        |[title-empty]:
        |[title-empty-prews]:
        |[title-empty-postws]:
        |[all]: all "all"
        |[all-prews]: all-prews "all-prews"
        |[dup]: dup "lastdup"
        |[all-midws]: all-midws "all-midws"
        |[all-postws]: all-postws "all-postws"
        |[all-empty-title]: all-empty-title
        |""".stripMargin.replace(".", " ")

    it("and sort, clean and deduplicate by default") {
      val md = Doc.parse(linkrefs)
      val cleaned = md.build().toString
      cleaned shouldBe
        """[all]: all "all"
          |[all-empty-title]: all-empty-title
          |[all-midws]: all-midws "all-midws"
          |[all-postws]: all-postws "all-postws"
          |[all-prews]: all-prews "all-prews"
          |[dup]: dup "lastdup"
          |[ref-bare]:
          |[title]: "title"
          |[title-empty]:
          |[title-empty-postws]:
          |[title-empty-prews]:
          |[title-postws]: "title-postws"
          |[title-prews]: "title-prews"
          |[url]: url
          |[url-postws]: url-postws
          |[url-prews]: url-prews
          |""".stripMargin
      Doc.parse(cleaned) shouldBe md
    }

    it("should escape and unescape link titles correctly") {
      val linkRefTitles =
        """[a]: url "title"
          |[b]: url "postws  "
          |[c]: url "  prews"
          |[d]: url "Quo\\th \""
          |[e]: "title"
          |[f]: "postws  "
          |[g]: "  prews"
          |[h]: "Quo\\th \""
          |""".stripMargin
      // The round trip shouldn't change the text at all
      val md = Doc.parse(linkRefTitles)
      val cleaned = md.build().toString
      cleaned shouldBe linkRefTitles
      Doc.parse(cleaned) shouldBe md

      // But shouldParse
      md.mds should have size 8
      md.mds.head shouldBe LinkRef("a", "url", "title")
      md.mds(1) shouldBe LinkRef("b", "url", "postws  ")
      md.mds(2) shouldBe LinkRef("c", "url", "  prews")
      md.mds(3) shouldBe LinkRef("d", "url", "Quo\\th \"")
      md.mds(4) shouldBe LinkRef("e", None, Some("title"))
      md.mds(5) shouldBe LinkRef("f", None, Some("postws  "))
      md.mds(6) shouldBe LinkRef("g", None, Some("  prews"))
      md.mds(7) shouldBe LinkRef("h", None, Some("Quo\\th \""))
    }

    it("but allow leaving unsorted and undeduplicated") {
      val md = Doc.parse(linkrefs, cfg = new ParserCfg(sortLinkRefs = false))

      val cleaned = md.build().toString
      cleaned shouldBe
        """[ref-bare]:
          |[dup]: dup
          |[dup]: dup "dup"
          |[url]: url
          |[url-prews]: url-prews
          |[url-postws]: url-postws
          |[title]: "title"
          |[title-prews]: "title-prews"
          |[title-postws]: "title-postws"
          |[title-empty]:
          |[title-empty-prews]:
          |[title-empty-postws]:
          |[all]: all "all"
          |[all-prews]: all-prews "all-prews"
          |[dup]: dup "lastdup"
          |[all-midws]: all-midws "all-midws"
          |[all-postws]: all-postws "all-postws"
          |[all-empty-title]: all-empty-title
          |""".stripMargin

      Doc.parse(cleaned, cfg = new ParserCfg(sortLinkRefs = false)) shouldBe md
      md.mds should have size 18
      md.mds.head shouldBe LinkRef("ref-bare", None, None)
      md.mds(1) shouldBe LinkRef("dup", "dup")
      md.mds(2) shouldBe LinkRef("dup", "dup", "dup")
      md.mds(3) shouldBe LinkRef("url", "url")
      md.mds(4) shouldBe LinkRef("url-prews", "url-prews")
      md.mds(5) shouldBe LinkRef("url-postws", "url-postws")
      md.mds(6) shouldBe LinkRef("title", None, Some("title"))
      md.mds(7) shouldBe LinkRef("title-prews", None, Some("title-prews"))
      md.mds(8) shouldBe LinkRef("title-postws", None, Some("title-postws"))
      md.mds(9) shouldBe LinkRef("title-empty", None, None)
      md.mds(10) shouldBe LinkRef("title-empty-prews", None, None)
      md.mds(11) shouldBe LinkRef("title-empty-postws", None, None)
      md.mds(12) shouldBe LinkRef("all", "all", "all")
      md.mds(13) shouldBe LinkRef("all-prews", "all-prews", "all-prews")
      md.mds(14) shouldBe LinkRef("dup", "dup", "lastdup")
      md.mds(15) shouldBe LinkRef("all-midws", "all-midws", "all-midws")
      md.mds(16) shouldBe LinkRef("all-postws", "all-postws", "all-postws")
      md.mds(17) shouldBe LinkRef("all-empty-title", "all-empty-title")
    }

    it("should ignore non-linkref") {
      val md = Doc.parse("""
        |[url]: url
        | [space-before]: Leading space?  Not a link ref
        |""".stripMargin.replace(".", " "))

      val cleaned = md.build().toString
      cleaned shouldBe
        """[space-before]: Leading space?  Not a link ref
        |
        |[url]: url
        |""".stripMargin
      // TODO: The round-trip is still broken because of cleaning up whitespace.
      // Doc.parse(cleaned) shouldBe md

      md.mds should have size 2
      md.mds.head shouldBe Paragraph("[space-before]: Leading space?  Not a link ref")
      md.mds(1) shouldBe LinkRef("url", "url")
    }
  }
}
