package com.tinfoiled.markd

/** A simple text paragraph of Markdown, containing any text content.
  *
  * @param content
  *   the text contents for the paragraph.
  */
case class Paragraph(content: String) extends Markd {
  override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
    sb ++= content.trim() ++= "\n"
  }

  /** Transforms this paragraph into another more specific [[Markd]] type if possible.
    */
  def refine(): Markd = Table.parse(content).getOrElse(this)

}
