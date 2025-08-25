package com.tinfoiled.markd

import scala.util.matching.Regex

/** Markdown header or section.
  *
  * {{{
  * # Header 1
  *
  * Header 2
  * --------
  *
  * ### Header 3
  * }}}
  *
  * @param level
  *   The level (from 1 to 9). A level of 0 can be used to represent an entire document.
  * @param title
  *   The title of the section
  * @param mds
  *   The internal subsections and parsed [[Markd]] elements.
  */
case class Header(level: Int, title: String, mds: Markd*) extends MultiMarkd[Markd] {

  type Self = Header

  def copy(level: Int = level, title: String = title, mds: Seq[Markd] = mds): Header = Header(level, title, mds: _*)

  override def copyMds(newMds: Seq[Markd]): Self = copy(mds = newMds)

  /** Helper method to simplify prepending a sublevel header at the top of this section. A new subsection that is one
    * level below this one will be added, after the content of this section but before any subsections. Appending is
    * much simpler via copyMds.
    *
    * @param innerTitle
    *   The title of the subsection to prepend
    * @param innerMds
    *   The contents of the subsection.
    * @return
    *   This header with the new subsection prepended to it.
    */
  def prepend(innerTitle: String, innerMds: Markd*): Header = {
    val toPrepend = Header(level + 1, innerTitle, innerMds: _*)
    flatMapFirstIn(ifNotFound = mds :+ toPrepend, replace = true) {
      case h @ Header(lvl, _, _*) if lvl == toPrepend.level && toPrepend != h => Seq(toPrepend, h)
      case h @ Header(lvl, _, _*) if lvl == toPrepend.level                   => Seq(h)
    }
  }

  override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
    level match {
      case 0 => // No title section for a document.
      case 1 => sb ++= title ++= "\n" ++= "=" * 78 ++= "\n"
      case 2 => sb ++= title ++= "\n" ++= "-" * 78 ++= "\n"
      case _ => sb ++= "#" * level ++= " " ++= title ++= "\n"
    }
    buildSub(sb, if (level == 0) None else Some(this), cfg)
  }
}
