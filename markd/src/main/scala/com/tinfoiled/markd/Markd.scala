package com.tinfoiled.markd

/** Any markdown element. */
trait Markd {

  /** Write some whitespace before this element.
    *
    * @param sb
    *   The builder to write to.
    * @param prev
    *   The element before this element (if any).
    * @param cfg
    *   A formatting configuration to help configure the output.
    * @return
    *   The builder passed in.
    */
  def buildPreSpace(
      sb: StringBuilder = new StringBuilder(),
      prev: Option[Markd] = None,
      cfg: FormatCfg = FormatCfg.Default
  ): StringBuilder = if (prev.isDefined) sb ++= "\n" else sb

  /** Write this element to the builder.
    *
    * @param sb
    *   The builder to write to.
    * @param cfg
    *   A formatting configuration to help configure the output.
    * @return
    *   The builder passed in.
    */
  def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = sb
}
