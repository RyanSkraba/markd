package com.tinfoiled.markd

/** A thematic break between paragraphs in a document.
  *
  * {{{
  * ***
  * }}}
  *
  * @see
  *   [[https://spec.commonmark.org/0.31.2/#thematic-breaks]]
  */
case object Break extends Markd {
  override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
    sb ++= "***\n"
  }
}
