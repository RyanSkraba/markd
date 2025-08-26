package com.tinfoiled.markd

/** Any markdown element, treated as a node in a tree.  Non-leaf nodes also implement [[MarkdContainer]]. */
trait MarkdNode {

  /** Write some whitespace before this node.
    *
    * @param sb
    *   The builder to write to.
    * @param prev
    *   The node before this node (if any).
    * @param cfg
    *   A formatting configuration to help configure the output.
    * @return
    *   The builder passed in.
    */
  def buildPreSpace(
      sb: StringBuilder = new StringBuilder(),
      prev: Option[MarkdNode] = None,
      cfg: FormatCfg = FormatCfg.Default
  ): StringBuilder = if (prev.isDefined) sb ++= "\n" else sb

  /** Write this node to the builder.
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
