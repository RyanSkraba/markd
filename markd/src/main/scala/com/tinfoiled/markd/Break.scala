package com.tinfoiled.markd

/** A thematic break between paragraphs in a document.
  *
  * TODO: Implementation
  *
  * {{{
  * ***
  * }}}
  *
  * @see
  *   [[https://spec.commonmark.org/current/#thematic-breaks]]
  */
case object Break extends Markd {
  override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
    sb ++= "***\n"
  }
}
