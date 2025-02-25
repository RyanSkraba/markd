package com.tinfoiled

import play.api.libs.json.Json

import scala.util.Try
import scala.util.matching.Regex

/** Markd is a model for simple markdown files. It can be used to parse, modify and write markdown text.
  *
  * The model is simple and includes many (but not all) features of markdown.
  *
  * You can clean a markdown file by parsing it into a model then writing it out again.
  *
  * {{{
  * files
  *   .foreach(f => {
  *     val md = Header.parse(f.slurp())
  *     f.writeAll(md.build().toString)
  *   })
  * }}}
  *
  * @see
  *   [[https://en.wikipedia.org/wiki/Markdown]]
  */
package object markd {

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

  /** Contents outside of the markdown processing.
    *
    * {{{
    *   <!-- comment -->
    * }}}
    *
    * @param content
    *   the contents of the comment.
    */
  case class Comment(content: String) extends Markd {
    override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
      sb ++= "<!--" ++= content ++= "-->\n"
    }
  }

  /** A fenced code block.
    *
    * {{{
    * ```bash
    * echo Hello world!
    * ```
    * }}}
    *
    * @param content
    *   the contents of the comment.
    */
  case class Code(code_type: String, content: String) extends Markd {

    lazy val builtContent: String = (code_type, content) match {
      case ("json", json) =>
        Try(Json.prettyPrint(Json.parse(json)) + "\n").getOrElse(json)
      case ("jsonline" | "jsonlines" | "json line" | "json lines", jsonline) =>
        jsonline
          .split("\n")
          .map { json => Try(Json.stringify(Json.parse(json))).getOrElse(json) }
          .mkString("", "\n", "\n")
      case _ => content
    }

    override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
      sb ++= "```" ++= code_type ++= "\n" ++= builtContent ++= "```\n"
    }
  }

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
  case class LinkRef(ref: String, url: Option[String] = None, title: Option[String] = None) extends Markd {

    /** Don't space between LinkRefs */
    override def buildPreSpace(
        sb: StringBuilder = new StringBuilder(),
        prev: Option[Markd] = None,
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
    val LinkRegex: Regex =
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

    def escape(in: String): String = in.replace("\\", "\\\\").replace("\"", "\\\"")

    def unescape(in: String): String = in.replace("\\\\", "\\").replace("\\\"", "\"")
  }

  /** An element that can contain other elements. */
  trait MultiMarkd[T <: Markd] extends Markd {

    type Self <: MultiMarkd[T]

    /** The subelements of this element. */
    def mds: Seq[T]

    /** Write this element to the builder.
      *
      * @param sb
      *   The builder to write to.
      * @param prev
      *   If known, the previous element written to the builder. This can be used to adjust spacing.
      * @return
      *   The builder passed in.
      */
    def buildSub(
        sb: StringBuilder = new StringBuilder(),
        prev: Option[T] = None,
        cfg: FormatCfg = FormatCfg.Default
    ): StringBuilder = {
      if (mds.nonEmpty) {
        mds.headOption.map { head =>
          head.buildPreSpace(sb, prev, cfg)
          head.build(sb, cfg)
        }
        for (md: Seq[Markd] <- mds.sliding(2) if md.size == 2) {
          md.last.buildPreSpace(sb, Some(md.head), cfg)
          md.last.build(sb, cfg)
        }
      }
      sb
    }

    /** Create a copy of the element with the new subelements.
      * @param newMds
      *   The subelements to replace the existing ones in the copy.
      */
    def copyMds(newMds: Seq[T]): Self

    /** Create a copy of the list of subelements, replacing some as necessary.
      *
      * A partial function matches and replaces Markd subelements. If the partial function is defined for one of the
      * subelements, it supplies the list of replacements. It matches on the element (or None to match the end of the
      * list) and its index.
      *
      * @param filter
      *   True if non-matching subelements should be removed, false to leave non-matching elements unchanged.
      * @param pf
      *   A partial function to replace markd elements.
      * @return
      *   A copy of this [[MultiMarkd]] with the replaced subelements
      */
    def replaceIn(filter: Boolean = false)(pf: PartialFunction[(Option[T], Int), Seq[T]]): Self = {
      // Elements undefined by the partial function should either be filtered from the results
      // or passed through without modification.
      val unmatched: PartialFunction[(Option[T], Int), Seq[T]] =
        if (filter) { case _ => Seq() }
        else { case (md, _) => md.toSeq }

      // Map the sub elements with the function, using None for the end.
      copyMds(
        (mds.map { Option(_) }.zipWithIndex :+ (None, mds.size))
          .flatMap(pf orElse unmatched)
      )
    }

    /** Copies this element, but flatMapping the first matching subelement to new values.
      *
      * A partial function matches and replaces Markd subelements. If the partial function is defined for one of the
      * subelements, it supplies the list of replacements.
      *
      * @param ifNotFound
      *   If nothing is matched, add these elements to the end of the list and try again. This permits insert-and-update
      *   replacements in the children.
      * @param replace
      *   If true, when falling back on ifNotFound, then replace all of the children instead of appending the new
      *   elements when trying again. This can be used to control the modification of the new elements, such as
      *   prepending or filtering.
      * @param pf
      *   A partial function to replace markd elements.
      * @return
      *   A copy of this [[MultiMarkd]] with the replaced subelements
      */
    def flatMapFirstIn(ifNotFound: => Seq[T] = Seq.empty, replace: Boolean = false)(
        pf: PartialFunction[T, Seq[T]]
    ): Self = {
      copyMds(
        Option(mds.indexWhere(pf.isDefinedAt))
          .filter(_ != -1)
          .map((_, mds))
          .orElse {
            val ifNotFoundReplacement = if (replace) ifNotFound else mds ++ ifNotFound
            // First fallback, use the ifNotFound instead.
            Option(ifNotFoundReplacement.indexWhere(pf.isDefinedAt))
              .filter(_ != -1)
              .map((_, ifNotFoundReplacement))
          }
          .map { case (idx, mds) => mds.patch(idx, pf(mds(idx)), 1) }
          .getOrElse(mds)
      )
    }

    /** Copies this element, but mapping the first matching subelement to a new value.
      *
      * A partial function matches and replaces Markd subelements. If the partial function is defined for one of the
      * subelements, it supplies the replacements.
      *
      * @param ifNotFound
      *   If nothing is matched, add these elements to the end of the list and try again. This permits insert-and-update
      *   replacements in the children.
      * @param replace
      *   If true, when falling back on ifNotFound, then replace all of the children instead of appending the new
      *   elements when trying again. This can be used to control the modification of the new elements, such as
      *   prepending or filtering.
      * @param pf
      *   A partial function to replace markd elements.
      * @return
      *   A copy of this [[MultiMarkd]] with the replaced subelements
      */
    def mapFirstIn(ifNotFound: => Seq[T] = Seq.empty, replace: Boolean = false)(pf: PartialFunction[T, T]): Self =
      flatMapFirstIn(ifNotFound = ifNotFound, replace = replace)(pf.andThen(Seq(_)))

    /** Copies this element, but mapping the first matching subelement to a new value.
      *
      * A partial function matches and replaces Markd subelements. If the partial function is defined for one of the
      * subelements, it supplies the replacements.
      *
      * @param ifNotFound
      *   If nothing is matched, add this element to the end of the list and try again. This permits insert-and-update
      *   replacements in the children.
      * @param pf
      *   A partial function to replace markd elements.
      * @return
      *   A copy of this [[MultiMarkd]] with the replaced subelements
      */
    def mapFirstIn(ifNotFound: => T)(pf: PartialFunction[T, T]): Self =
      mapFirstIn(ifNotFound = Seq(ifNotFound))(pf)

    /** Finds the first [[Markd]] element recursively in this element for which the given partial function is defined,
      * and applies the partial function to it.
      *
      * @param pf
      *   the partial function
      * @return
      *   an option value containing pf applied to the first value for which it is defined, or `None` if none exists.
      */
    def collectFirstRecursive[B](pf: PartialFunction[Markd, B]): Option[B] =
      if (pf.isDefinedAt(this)) Some(pf(this))
      else
        mds
          .to(LazyList)
          .map {
            case md if pf.isDefinedAt(md) => Some(pf(md))
            case md: MultiMarkd[_]        => md.collectFirstRecursive(pf)
            case _                        => None
          }
          .find(_.isDefined)
          .flatten

    def replaceRecursively(pf: PartialFunction[Markd, Markd]): Self = {
      replaceIn() {
        case (Some(md), _) if pf.isDefinedAt(md) => Seq(pf.apply(md).asInstanceOf[T])
        case (Some(md: MultiMarkd[_]), _)        => Seq(md.replaceRecursively(pf).asInstanceOf[T])
        case (Some(md), _)                       => Seq(md)
        case (None, _)                           => Seq.empty
      }
    }
  }

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
  case class Header(title: String, level: Int, mds: Seq[Markd]) extends MultiMarkd[Markd] {

    type Self = Header

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
      val toPrepend = Header(innerTitle, level + 1, innerMds)
      flatMapFirstIn(ifNotFound = mds :+ toPrepend, replace = true) {
        case h @ Header(_, lvl, _) if lvl == toPrepend.level && toPrepend != h => Seq(toPrepend, h)
        case h @ Header(_, lvl, _) if lvl == toPrepend.level                   => Seq(h)
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

  object Header {

    /** Splits text into strings ready to be placed into [[Comment]], [[Code]], [[LinkRef]] and [[Paragraph]] instances.
      */
    private[this] val Pass1Regex: Regex =
      raw"""(?x)(?s)
            ( <!--(.*?)-->                                     # Comment
            | (?<=(^|\n))```([^\n]*?)\s*\n(.*?)```\s*(\n|$$)   # Code
            | (?<=(^|\n))(\[[^\]]+\]:[^\n]*)                   # LinkRef
            | .*?(?=$$|<!--|```|\n\[[^\]]+\]:|\n\s*\n)         # All other text
            )
         """.r

    /** Regex used to split header section. */
    val HeaderRegex: Regex =
      raw"""(?x)
          (?=(^|\n)                           # Lookahead
            (
              (?<titleml>[^\n]+)\n            # Multiline header
              (===+|---+)
            |
              (\#{1,9})\s+(?<titlesl>[^\n]+)  # or single line header
            )
            (\n|$$))
         """.r(
        "",
        "",
        "title_ml",
        "level_ml",
        "level_sl",
        "title_sl"
      )

    def apply(level: Int, title: String, sub: Markd*): Header = Header(title, level, sub)

    /** Extract the level and title from a matching header. */
    private[this] def extractHeader(m: Regex.Match): Header = {
      if (Option(m.group("title_ml")).isDefined)
        Header(
          if (m.group("level_ml").startsWith("=")) 1 else 2,
          m.group("title_ml")
        )
      else
        Header(m.group("level_sl").length, m.group("title_sl"))
    }

    /** Splits the content into sections, as a tree of headers. */
    def parse(content: String, cfg: ParserCfg = new ParserCfg()): Header = {
      // The first pass splits everything into code, comments, links and paragraphs
      val pass1: Iterator[Markd] = Pass1Regex
        .findAllMatchIn(content)
        .flatMap {
          case Pass1Regex(_, _, _, code_type, code, _*) if code != null        => Option(Code(code_type, code))
          case Pass1Regex(_, comment, _*) if comment != null                   => Option(Comment(comment))
          case Pass1Regex(_, _, _, _, _, _, _, linkRef, _*) if linkRef != null => LinkRef.parse(linkRef)
          case Pass1Regex(all, _*) if !all.isBlank                             => Option(Paragraph(all))
          case _                                                               => None
        }

      // The second pass splits Headers out of the paragraphs
      val pass2: Iterator[Markd] = pass1.flatMap {
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
        case other: Markd => Option(other)
      }

      // A third pass allows a Paragraph to "refine" itself to another type.
      val pass3: Iterator[Markd] = pass2.map {
        case p: Paragraph => p.refine()
        case other        => other
      }

      // Apply a recursive function that makes the flat list into a tree.
      def treeify(node: Header, flat: Seq[Markd]): (Header, Seq[Markd]) =
        flat.headOption match {
          // If the next element in the list is a sub-section (i.e. greater level)
          case Some(next: Header) if next.level > node.level =>
            // then the sub-section should be treeified, using as many elements as necessary from
            // the list.
            val (subsection, flatRemainder) = treeify(next, flat.tail)
            // Add the subsection to this node, and continue to treeify this node with the rest.
            treeify(node.copy(mds = node.mds :+ subsection), flatRemainder)
          // If the next element in the list is a section of the same or lower level,
          // then just return, and it can be added to the current node's parent.
          case Some(_: Header) => (node, flat)
          // If the next element in the list is any other Markd, then just add it to this node.
          case Some(next) => treeify(node.copy(mds = node.mds :+ next), flat.tail)
          // Otherwise processing is complete.
          case _ => (node, Seq.empty)
        }
      val pass4: Header = treeify(Header(0, ""), pass3.toSeq)._1

      // Organize all of the nodes inside the tree.
      def organizeHeaderContents(node: Header): Header = {
        val (others, linkRefs, headers) = node.mds
          .foldRight((List.empty[Markd], List.empty[LinkRef], List.empty[Header])) { case (md, (xs1, xs2, xs3)) =>
            md match {
              case header: Header =>
                (xs1, xs2, organizeHeaderContents(header) :: xs3)
              case linkRef: LinkRef => (xs1, linkRef :: xs2, xs3)
              case _                => (md :: xs1, xs2, xs3)
            }
          }
        // The right order is all elements, followed by linkRefs, followed by subheaders.
        node.copy(mds = others ++ cfg.linkCleaner(linkRefs) ++ headers)
      }
      organizeHeaderContents(pass4)
    }
  }

  /** Alignment in a Table. */
  object Align extends Enumeration {
    type Align = Value
    val LEFT, CENTER, RIGHT = Value
  }

  import Align._

  /** Markdown table.
    *
    * {{{
    * | Col1     |      Col2     |  Col3 |
    * |----------|:-------------:|------:|
    * | col 1 is |  left-aligned | $1600 |
    * | col 2 is |    centered   |   $12 |
    * | col 3 is | right-aligned |    $1 |
    * }}}
    *
    * @param aligns
    *   The alignment for each column.
    * @param mds
    *   The table rows, including the column headers (as the first row) and cell values (all subsequent rows).
    */
  case class Table(aligns: Seq[Align], mds: Seq[TableRow]) extends MultiMarkd[TableRow] {

    type Self = Table

    /** A simple definition of the title cell is the top left cell value. */
    lazy val title: String = mds.headOption.map(_.head).getOrElse("")

    /** The number of columns that this table has. A row can have more than this number of columns, but the extra cells
      * will be appended, not aligned.
      */
    lazy val colSize: Int = aligns.length

    /** The number of rows that this table has, including the header row. */
    lazy val rowSize: Int = mds.length

    /** The maximum cell string length for each column, not including margins */
    lazy val widths: Seq[Int] = Seq.tabulate(colSize) { i => Math.max(1, mds.map(_(i).length).max) }

    /** @param row
      *   The index of the row to get from the table, noting that zero is the header row.
      * @return
      *   The row, or an empty row if the index is out of bounds.
      */
    def apply(row: Int): TableRow = mds.applyOrElse(row, (_: Int) => TableRow.from())

    /** @param rowHead
      *   The row to get from the table by matching the first cell, including the header row.
      * @return
      *   The row, or an empty row if a matching row can't be found.
      */
    def apply(rowHead: String): TableRow = apply(mds.indexWhere(_.head == rowHead))

    /** @param column
      *   The index of the column to get from the table
      * @param row
      *   The index of the row to get from the table, noting that zero is the header row.
      * @return
      *   The cell value, or an empty string if any of the indexes are out of bounds.
      */
    def apply(column: Int, row: Int): String = apply(row).apply(column)

    /** @param column
      *   The index of the column to get from the table
      * @param rowHead
      *   The row to get from the table by matching the first cell, including the header row.
      * @return
      *   The cell value, or an empty string if the column is out of bounds or the row header can't be found
      */
    def apply(column: Int, rowHead: String): String = apply(rowHead).apply(column)

    /** @param columnHead
      *   The row to get from the table by matching the first cell in the header.
      * @param rowHead
      *   The row to get from the table by matching the first cell, including the header row.
      * @return
      *   The cell value, or an empty string if the column is out of bounds or the row header can't be found
      */
    def apply(columnHead: String, rowHead: String): String = {
      val colIndex = mds.head.cells.indexOf(columnHead)
      if (colIndex == -1) ""
      else apply(rowHead).apply(colIndex)
    }

    override def build(sb: StringBuilder = new StringBuilder(), cfg: FormatCfg = FormatCfg.Default): StringBuilder = {
      // The column header line
      mds.head.buildRow(aligns, widths, sb, cfg)

      // The separator row
      sb ++= (for ((a, i) <- aligns.zipWithIndex)
        yield {
          val sb2 = new StringBuilder("-" * (widths(i) + 2))
          if (a == Align.CENTER || a == Align.RIGHT)
            sb2.setCharAt(sb2.length - 1, ':')
          if (a == Align.CENTER)
            sb2.setCharAt(0, ':')
          sb2
        }).mkString("|", "|", "|\n")

      // And a line for each row
      for (tr <- mds.tail)
        tr.buildRow(aligns, widths, sb, cfg)
      sb
    }

    override def copyMds(newMds: Seq[TableRow]): Self = copy(mds = newMds)

    /** Creates a new table from this one with the given cell value updated. Note that the zeroth row is the column
      * headers.
      *
      * @param column
      *   The index of the column to update
      * @param row
      *   The index of the row to update
      * @param cell
      *   The new value
      * @return
      *   A table with the one cell updated to the given value
      */
    def updated(column: Int, row: Int, cell: String): Table = {
      val cellsUpdated: Seq[String] =
        mds.lift
          .apply(row)
          .map(_.cells)
          .getOrElse(Seq.empty)
          .padTo(column + 1, "")
          .updated(column, cell)
          .reverse
          .dropWhile(_.isEmpty)
          .reverse

      val rowsUpdated = mds
        .padTo(row + 1, TableRow.from())
        .updated(row, new TableRow(cellsUpdated))

      Table(
        if (row == 0) aligns.padTo(column + 1, Align.LEFT) else aligns,
        mds = rowsUpdated
      )
    }

    /** Creates a new table from this one with the given cell value updated. Note that the row head will match the
      * column headers as well.
      *
      * @param column
      *   The index of the column to update
      * @param rowHead
      *   The head of the row to update. This matches the first row found, or adds the row to the table.
      * @param cell
      *   The new value
      * @return
      *   A table with the one cell updated to the given value
      */
    def updated(column: Int, rowHead: String, cell: String): Table = {
      val rowIndex = mds.indexWhere(_.head == rowHead)
      if (rowIndex == -1)
        updated(0, mds.length, rowHead).updated(column, mds.length, cell)
      else updated(column, rowIndex, cell)
    }

    /** Creates a new table from this one with the given cell value updated. Note that the row head will match the
      * column headers as well.
      *
      * @param columnHead
      *   The text in the header column to update. This matches the first column found, or adds the column to the table.
      * @param rowHead
      *   The head of the row to update. This matches the first row found, or adds the row to the table.
      * @param cell
      *   The new value
      * @return
      *   A table with the one cell updated to the given value
      */
    def updated(columnHead: String, rowHead: String, cell: String): Table = {
      val colIndex = mds.head.cells.indexOf(columnHead)
      if (colIndex == -1)
        updated(mds.head.cells.length, 0, columnHead).updated(mds.head.cells.length, rowHead, cell)
      else updated(colIndex, rowHead, cell)
    }
  }

  object Table {

    /** Split into cells by |, taking into account escaped pipes but not other constructions.
      */
    val CellRegex: Regex = raw"(?<!\\)\|".r

    val AlignmentCellRegex: Regex = raw"^\s*(:-+:|---+|:--+|-+-:)\s*$$".r

    /** Shortcut method just for the varargs */
    def from(aligns: Seq[Align], mds: TableRow*): Table = Table(aligns, mds)

    /** Determines if some content can be reasonably parsed into a [[Table]].
      * @param content
      *   The string contents to parse.
      * @return
      *   An [[Option]] containing a [[Table]] if it is possible to construct, or None if it isn't.
      */
    def parse(content: String): Option[Table] = {
      val prelines = content.split("\n").map(parseRow)
      // If there aren't at least two lines, this isn't a Table.
      if (prelines.length < 2) return None

      // If the alignment line starts with pipe, then remove the first strictly empty cell
      // from each row.
      val lines =
        if (!prelines(1).headOption.contains("")) prelines
        else prelines.map(xs => if (xs.headOption.contains("")) xs.drop(1) else xs)

      // Check the second row for alignments.
      val aligns: Seq[Align] = lines(1).flatMap {
        case AlignmentCellRegex(cell) if cell.startsWith(":") && cell.endsWith(":") =>
          Some(Align.CENTER)
        case AlignmentCellRegex(cell) if cell.endsWith(":") =>
          Some(Align.RIGHT)
        case AlignmentCellRegex(_) => Some(Align.LEFT)
        case _                     => None
      }
      // If the alignment row removed any elements, then this is not a Table
      if (aligns.length < lines(1).length) return None

      val rows = lines.patch(1, Seq.empty, 1).map(_.map(_.trim)).map(TableRow.apply)

      Some(Table(aligns, rows))
    }

    /** Parses a string into cells, removing all trailing whitespace-only cells.
      */
    def parseRow(content: String): Seq[String] = {
      val values = CellRegex.pattern.split(content, -1)
      if (values.last.nonEmpty) values
      else {
        val lastNonEmpty = values.lastIndexWhere(!_.isBlank)
        values.dropRight(values.length - lastNonEmpty - 1)
      }
    }
  }

  case class TableRow(cells: Seq[String]) extends Markd {

    /** The row header is the leftmost cell. */
    lazy val head: String = cells.headOption.getOrElse("")

    def apply(i: Int): String = cells.applyOrElse(i, (_: Int) => "")

    def updated(i: Int, c: String): TableRow =
      copy(cells = cells.padTo(i + 1, "").updated(i, c).reverse.dropWhile(_.isEmpty).reverse)

    /** Write this element to the builder.
      *
      * @param sb
      *   The builder to write to.
      * @return
      *   The builder passed in.
      */
    def buildRow(
        aligns: Seq[Align],
        widths: Seq[Int],
        sb: StringBuilder = new StringBuilder(),
        cfg: FormatCfg = FormatCfg.Default
    ): StringBuilder = {

      val aligned =
        for (i <- 0 until Math.max(aligns.length, cells.length)) yield {
          val align = aligns.applyOrElse(i, (_: Int) => Align.LEFT)
          val width = widths.applyOrElse(i, (_: Int) => 0)
          val cell = cells.applyOrElse(i, (_: Int) => "")

          val lPad =
            if (align == Align.CENTER) (width - cell.length) / 2
            else if (align == Align.RIGHT) width - cell.length
            else 0
          val lPadded = " " * Math.max(0, lPad) + cell

          lPadded + " " * Math.max(0, width - lPadded.length)
        }

      sb ++= aligned.mkString("| ", " | ", " |")
      sb ++= "\n"
    }
  }

  object TableRow {

    /** Shortcut method just for the varargs */
    def from(cells: String*): TableRow = TableRow(cells)
  }

  /** Helps build the model when parsing contents.
    * @param sortLinkRefs
    *   Whether to sort and deduplicate LinkRefs while parsing (true by default).
    */
  class ParserCfg(sortLinkRefs: Boolean = true) {

    /** If sorting, provides a key to use from the linkref, allowing custom grouping and deduplication of the links. The
      * linkref will be sorted and deduplicated based on this key. By default, the [[LinkRef.ref]] is used directly.
      */
    def linkSorter(): PartialFunction[LinkRef, (String, LinkRef)] = { case lr => lr.ref -> lr }

    /** Clean up the references at the end of a section. */
    def linkCleaner(links: Seq[LinkRef]): Seq[LinkRef] = if (sortLinkRefs) {
      // Clean the links.
      links.map(linkSorter().orElse { case lr => (lr.ref, lr) }).toMap.toSeq.sortBy(_._1).map(_._2)
    } else links

    /** Apply this configuration to an element, reparsing it as a clean model.
      */
    def clean(md: Markd, cfg: FormatCfg = FormatCfg.Default): Header = Header.parse(md.build().toString, this)
  }

  /** Helps write the model to the output. */
  class FormatCfg {
    // TODO: Minimise
  }

  object FormatCfg {
    val Default = new FormatCfg
  }
}
