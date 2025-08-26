package com.tinfoiled.markd

import scala.util.matching.Regex

/** A link reference.
  *
  * {{{
  * [ref]: https://link.url "Optional description"
  * }}}
  *
  * @param ref
  *   the markdown tag used to reference the link
  * @param url
  *   the url that is being linked to
  * @param title
  *   optionally a title or description of the link for hover text
  */
case class LinkRef(ref: String, url: Option[String] = None, title: Option[String] = None) extends MarkdNode {

  /** Don't space between LinkRefs */
  override def buildPreSpace(
      sb: StringBuilder = new StringBuilder(),
      prev: Option[MarkdNode] = None,
      cfg: FormatCfg = FormatCfg.Default
  ): StringBuilder = prev match {
    case Some(LinkRef(_, _, _)) => sb
    case _                      => super.buildPreSpace(sb, prev, cfg)
  }

  override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
    sb ++= "[" ++= ref ++= "]:"
    url.filterNot(_.isBlank).map(sb ++= " " ++= _)
    title.filterNot(_.isBlank).map(sb ++= " \"" ++= LinkRef.escape(_) += '"')
    sb ++= "\n"
  }
}

object LinkRef {

  /** Regex used to find link references. */
  private val LinkRegex: Regex =
    raw"""(?x)
          ^
          \[(?<ref>[^]]+)]:
          \s*(?<url>[^"].*?)?
          (\s*"(?<title>.*?)")?
          \s*
          $$
          """.r

  def apply(ref: String, url: String): LinkRef = LinkRef(ref, Some(url), None)

  def apply(ref: String, url: String, title: String): LinkRef = LinkRef(ref, Some(url), Some(title))

  def parse(content: String): Option[LinkRef] = LinkRef.LinkRegex
    .findFirstMatchIn(content)
    .map(m =>
      LinkRef(
        m.group("ref"),
        Option(m.group("url")).filter(!_.isBlank).map(_.trim),
        Option(m.group("title")).map(LinkRef.unescape).filter(!_.isBlank)
      )
    )

  private def escape(in: String): String = in.replace("\\", "\\\\").replace("\"", "\\\"")

  private def unescape(in: String): String = in.replace("\\\\", "\\").replace("\\\"", "\"")
}
