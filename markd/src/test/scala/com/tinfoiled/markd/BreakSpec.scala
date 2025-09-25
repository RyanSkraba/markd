package com.tinfoiled.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Break]] */
class BreakSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing a Break block") {
    ignore("should find a standalone node") {
      val md = Markd.parse("***")
      md shouldBe Markd(Break)
    }
  }
}
