package com.tinfoiled.markd

import scala.util.matching.Regex

/** An "invisible" top-level markdown node that only serves as a container for the others. This is useful if there is
  * text before the Header(1) or if there are multiple Header(1) in a page.
  *
  * @param mds
  *   The internal subsections and parsed [[MarkdNode]] nodes.
  */
case class Markd(mds: MarkdNode*) extends MarkdContainer[MarkdNode] {

  type Self = Markd

  override def copyMds(newMds: Seq[MarkdNode]): Self = Markd(newMds: _*)

  override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
    buildSub(sb, None, cfg)
  }
}

object Markd {

  /** Splits text into strings ready to be placed into [[Comment]], [[Code]], [[LinkRef]] and [[Paragraph]] instances.
    */
  private[this] val Pass1Regex: Regex =
    raw"""(?x)(?s)
            ( <!--(.*?)-->                                     # Comment
            | (?<=(^|\n))```([^\n]*?)\s*\n(.*?)```\s*(\n|$$)   # Code
            | (?<=(^|\n))(\[[^]]+]:[^\n]*)                     # LinkRef
            | .*?(?=$$|<!--|```|\n\[[^]]+]:|\n\s*\n)           # All other text
            )
         """.r

  /** Regex used to split header section. */
  private val HeaderRegex: Regex =
    raw"""(?x)
          (?=(^|\n)                           # Lookahead
            (
              (?<mlTitle>[^\n]+)\n            # Multiline header
              (?<mlLevel>===+|---+)
            |
              (?<slLevel>\#{1,9})\s+(?<slTitle>[^\n]+)  # or single line header
            )
            (\n|$$))
         """.r

  /** Extract the level and title from a matching header. */
  private[this] def extractHeader(m: Regex.Match): Header = {
    if (Option(m.group("mlTitle")).isDefined)
      Header(if (m.group("mlLevel").startsWith("=")) 1 else 2, m.group("mlTitle"))
    else
      Header(m.group("slLevel").length, m.group("slTitle"))
  }

  /** Splits the content into sections, as a tree of headers. */
  def parse(content: String, cfg: ParserCfg = new ParserCfg()): Markd = {
    // The first pass splits everything into code, comments, links and paragraphs
    val pass1: Iterator[MarkdNode] = Pass1Regex
      .findAllMatchIn(content)
      .flatMap {
        case Pass1Regex(_, _, _, code_type, code, _*) if code != null        => Option(Code(code_type, code))
        case Pass1Regex(_, comment, _*) if comment != null                   => Option(Comment(comment))
        case Pass1Regex(_, _, _, _, _, _, _, linkRef, _*) if linkRef != null => LinkRef.parse(linkRef)
        case Pass1Regex(all, _*) if !all.isBlank                             => Option(Paragraph(all))
        case _                                                               => None
      }

    // The second pass splits Headers out of the paragraphs
    val pass2: Iterator[MarkdNode] = pass1.flatMap {
      case Paragraph(content) =>
        HeaderRegex
          .split(content)
          .flatMap { text =>
            HeaderRegex.findPrefixMatchOf(s"$text\n") match {
              case None if text.nonEmpty => Some(Paragraph(text.trim))
              case Some(m: Regex.Match) =>
                val h = extractHeader(m)
                // The contents come after the last match in the regex.
                val lastMatchedGroup =
                  1 + m.subgroups.lastIndexWhere(_ != null)
                val headerContents = m.after(lastMatchedGroup).toString
                if (headerContents.isEmpty) Some(h)
                else Seq(h, Paragraph(headerContents.trim))
              case _ => None
            }
          }
      case other: MarkdNode => Option(other)
    }

    // A third pass allows a Paragraph to "refine" itself to another type.
    val pass3: Iterator[MarkdNode] = pass2.map {
      case p: Paragraph => p.refine()
      case other        => other
    }

    // Apply a recursive function that makes the flat list into a tree.
    def treeify(node: Header, flat: Seq[MarkdNode]): (Header, Seq[MarkdNode]) =
      flat.headOption match {
        // If the next node in the list is a sub-section (i.e. greater level)
        case Some(next: Header) if next.level > node.level =>
          // then the sub-section should be treeified, using as many nodes as necessary from
          // the list.
          val (subsection, flatRemainder) = treeify(next, flat.tail)
          // Add the subsection to this node, and continue to treeify this node with the rest.
          treeify(node.copyMds(node.mds :+ subsection), flatRemainder)
        // If the next node in the list is a section of the same or lower level,
        // then just return, and it can be added to the current node's parent.
        case Some(_: Header) => (node, flat)
        // If the next node in the list is any other Markd, then just add it to this node.
        case Some(next) => treeify(node.copyMds(node.mds :+ next), flat.tail)
        // Otherwise processing is complete.
        case _ => (node, Seq.empty)
      }
    val pass4: Header = treeify(Header(0, ""), pass3.toSeq)._1

    // Organize all of the nodes inside the tree.
    def organizeHeaderContents(node: Header): Header = {
      val (others, linkRefs, headers) = node.mds
        .foldRight((List.empty[MarkdNode], List.empty[LinkRef], List.empty[Header])) { case (md, (xs1, xs2, xs3)) =>
          md match {
            case multimd: Header  => (xs1, xs2, organizeHeaderContents(multimd) :: xs3)
            case linkRef: LinkRef => (xs1, linkRef :: xs2, xs3)
            case _                => (md :: xs1, xs2, xs3)
          }
        }
      // The right order is all nodes, followed by linkRefs, followed by subheaders.
      node.copyMds(others ++ cfg.linkCleaner(linkRefs) ++ headers)
    }
    Markd(organizeHeaderContents(pass4).mds: _*)
  }
}
