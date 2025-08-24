package com.tinfoiled.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Break]]
  */
class BreakSpec extends AnyFunSpecLike with Matchers {
  describe("Parsing a Break block") {
    ignore("should find a standalone element") {
      val md = Doc.parse("***")
      md shouldBe Doc(Break)
    }
  }
}
