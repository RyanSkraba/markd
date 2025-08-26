package com.tinfoiled.markd

/** Contents outside of the markdown processing.
  *
  * {{{
  *   <!-- comment -->
  * }}}
  *
  * @param content
  *   the contents of the comment.
  */
case class Comment(content: String) extends MarkdNode {
  override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
    sb ++= "<!--" ++= content ++= "-->\n"
  }
}
