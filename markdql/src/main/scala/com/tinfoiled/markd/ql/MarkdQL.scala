package com.tinfoiled.markd.ql

import com.tinfoiled.markd._

import scala.util.matching.Regex

/** Implements a simple query language on Markdown text.
  *
  * Examples:
  *
  * {{{
  * | Query                   | Description                                                                                                                             |
  * |-------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|
  * | `One.Two.Three[*]`      | Find the level one header with the name `One`, with a subheader named `Two` and a third-level header `Three` and return those contents. |
  * | `Top`                   | Find and return the level one header with the title "Top"                                                                               |
  * | `Top[0]`                | Find and return the first child of the level one header with the title "Top"                                                            |
  * | `Top[-1]`               | Find and return the last child of the level one header with the title "Top"                                                             |
  * | `"..Top"[]"`           | Find and return the level one header with the title `..Top"[]`. Use escapes internally to match quotes.                                 |
  * | `/Week .* Failures/`    | Find and return the level one header that matches the regex, such as `Week 21 Failures`                                                 |
  * | `Monthly..2025-02`      | Find the level one header with the title `Monthly` and return the first subheader named `2025-02` at any level inside                   |
  * | `Weekly\|To Do`         | Find the level one header with the title `Weekly` and return the `To Do` table that it contains.                                        |
  * | `..\|Status[12]`        | Find any `Status` table and return the 12th table row (note that row 0 is always the column headers).                                   |
  * | `..\|Status[-1]`        | Find any `Status` table and return the last table row.                                                                                  |
  * | `..\|Status[0][3]`      | Find any `Status` table and return the name of the 4th column (row 0 is the headers, and columns are zero indexed).                     |
  * | `..\|Status[Key,rowId]` | Find any Status table and return the cell under the column `Key` with the row header `rowId`  **Note that this is column-first!**       |
  * | `..Weekly[0]`           | Any header with the title `Weekly` and return the first element it contains.                                                            |
  * | `Weekly[code][0]`       | ❌ Find the top `Weekly` header and return the first code block it contains.                                                             |
  * | `Weekly[0][4]`          | Find the top `Weekly` header, go to its first child and return that elements 5th child.                                                 |
  * | `..\|/.*Status/[1]`     | Find any table with a title ending with `Status` and return the first non-header row.                                                   |
  * }}}
  */
object MarkdQL {

  private[this] val QueryRegex: Regex =
    raw"""(?x)^
              (?<sep>\.{0,2}\|?)                 # Start with a separator of 0-2 periods
              (?:
                (?<token>                        # Either a token and optional index in []
                    [^/"\[.|][^.|\[]*
                    |
                    "(?:[^"\\]|\\.)+"
                    |
                    /(?:[^/\\]|\\.)+/)
                (?:\[(?<optIndex>
                    [^"][^]]*
                    |
                    "(?:[^"\\]|\\.)*")])?
                |
                \[(?<index>                      # Or no token and an index in square brackets
                    [^]"][^]]*
                    |
                    "(?:[^"\\]|\\.)*")]
              )
              (?<rest>.*)
             $$""".r

  /** @param query
    *   The string to query on.
    * @param md
    *   The markdown node that starts the query.
    * @param fail
    *   If true, and a match isn't found, then throw an exception.
    * @return
    */
  def query(query: String, md: MarkdNode, fail: Boolean = false): Seq[MarkdNode] = {
    val queryChain = LazyList.iterate(Query(rest = query, mds = Seq(md))) { _.next }
    try { queryChain.dropWhile(!_.isDone).head.mds }
    catch { case _: UnrecognizedQueryException => sys.error(s"Unrecognized query: $query") }
  }

  class UnrecognizedQueryException(msg: String) extends Exception(msg)

  /** A query to apply on the current set of markdown nodes, with the next separator, token and index to apply to the
    * current node set. The rest of the query is saved for the next steps.
    *
    * A query looks somewhat like `.token[index].token[index].token[index]` (not all parts are mandatory)
    *
    * This is an internal API, exposed for testability.
    *
    * @param mds
    *   The current set of markdown nodes currently being queried (before the query is applied).
    * @param separator
    *   The separator before the string token, zero OR two periods followed by an optional |.
    * @param token
    *   A string token to search for (if any)
    * @param index
    *   The contents of the index to be applied to the query (if any).
    * @param rest
    *   The remainder of the query to be applied next.
    */
  case class Query(
      regex: Boolean = false,
      recursive: Boolean = false,
      separator: String = "",
      token: String = "",
      index: String = "",
      rest: String,
      mds: Seq[MarkdNode] = Seq.empty
  ) {

    /** The query is finished if there are no candidate markdown nodes to be queried, or if the current query string is
      * empty (nothing left to do).
      */
    lazy val isDone: Boolean = mds.isEmpty || token.isEmpty && index.isEmpty && rest.isEmpty

    /** The token as a regex */
    private lazy val tokenRegex = token.r

    /** Applies the query to the current set of nodes and returns the next step for the query with the new set of nodes
      * to be queried, the next separator, token and index to apply.
      */
    lazy val next: Query = {

      // Find all the MarkdNodes that match the current token
      val tokenMatches: Seq[MarkdNode] = (separator, regex, recursive, mds) match {
        case ("", _, _, md) if token.isEmpty => md

        // Tables
        case ("|", true, true, Seq(md: MarkdContainer[_])) =>
          md.collectFirstRecursive { case tbl: Table if tokenRegex.matches(tbl.title) => tbl }.toSeq
        case ("|", true, false, Seq(md: MarkdContainer[_])) =>
          md.mds.collectFirst { case tbl: Table if tokenRegex.matches(tbl.title) => tbl }.toSeq
        case ("|", false, true, Seq(md: MarkdContainer[_])) =>
          md.collectFirstRecursive { case tbl: Table if tbl.title == token => tbl }.toSeq
        case ("|", false, false, Seq(md: MarkdContainer[_])) =>
          md.mds.collectFirst { case tbl: Table if tbl.title == token => tbl }.toSeq

        // Headers
        case ("", true, true, Seq(md: MarkdContainer[_])) =>
          md.collectFirstRecursive { case h @ Header(_, title, _*) if tokenRegex.matches(title) => h }.toSeq
        case ("", true, false, Seq(md: MarkdContainer[_])) =>
          md.mds.collectFirst { case h @ Header(_, title, _*) if tokenRegex.matches(title) => h }.toSeq
        case ("", false, true, Seq(md: MarkdContainer[_])) =>
          md.collectFirstRecursive { case h @ Header(_, title, _*) if title == token => h }.toSeq
        case ("", false, false, Seq(md: MarkdContainer[_])) =>
          md.mds.collectFirst { case h @ Header(_, title, _*) if title == token => h }.toSeq
      }

      val intIndex = index.toIntOption

      // Apply the index to them
      val nextMds = tokenMatches match {
        case Seq(mdx: MarkdContainer[_]) if index == "*"            => mdx.mds
        case Seq(mdx: MarkdContainer[_]) if intIndex.exists(_ >= 0) => mdx.mds.lift(intIndex.get).toSeq
        case Seq(mdx: MarkdContainer[_]) if intIndex.exists(_ < 0)  => mdx.mds.lift(mdx.mds.length + intIndex.get).toSeq
        case Seq(tr: TableRow) if intIndex.exists(_ >= 0) => tr.cells.lift(intIndex.get).map(Paragraph(_)).toSeq
        case Seq(tr: TableRow) if intIndex.exists(_ < 0) =>
          tr.cells.lift(tr.cells.length + intIndex.get).map(Paragraph(_)).toSeq
        case Seq(tbl: Table) if index.contains(',') =>
          val (column, row) = index.span(_ != ',')
          tbl.get(column, row.tail).map(Paragraph(_)).toSeq
        case _ => tokenMatches
      }

      // If there's nothing left, then the next state is just the indexed matches
      if (rest == "" || rest == ".") Query(regex = false, recursive = false, "", "", "", "", nextMds)
      else {
        // Otherwise pop off the next separators, token and index from the query
        val m =
          QueryRegex.findFirstMatchIn(rest).getOrElse { throw new UnrecognizedQueryException(s"Failed on :$rest") }

        // If the token is present, then unquote it if it's quoted.
        val (nextRegex, nextToken) = Option(m.group("token"))
          .map {
            case quoted if quoted.head == '"' =>
              false -> quoted.slice(1, quoted.length - 1).replaceAll(raw"\\(.)", "$1")
            case quoted if quoted.head == '/' => true -> quoted.slice(1, quoted.length - 1).replaceAll(raw"\\(.)", "$1")
            case unquoted                     => false -> unquoted
          }
          .getOrElse((false, ""))

        // Likewise for the optIndex or index group.
        val nextIndex = Option(m.group("optIndex"))
          .orElse(Option(m.group("index")))
          .map {
            case quoted if quoted.head == '"' => quoted.slice(1, quoted.length - 1).replaceAll(raw"\\(.)", "$1")
            case unquoted                     => unquoted
          }
          .getOrElse("")

        val (nextRecursive, nextSeparator) = m.group("sep") match {
          case recursive if recursive.startsWith("..") => (true, recursive.drop(2))
          case single if single.startsWith(".")        => (false, single.tail)
          case other                                   => (false, other)
        }

        Query(nextRegex, nextRecursive, nextSeparator, nextToken, nextIndex, m.group("rest"), nextMds)
      }
    }
  }

}
