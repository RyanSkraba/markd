package com.tinfoiled.markd.ql

import com.tinfoiled.markd._

import scala.util.matching.Regex

/** Implements a simple query language on Markdown text.
  *
  * Examples:
  *
  * {{{
  * | Query                  | Description                                                                                                                             |
  * |------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|
  * | `One.Two.Three[*]`     | Find the level one header with the name `One`, with a subheader named `Two` and a third-level header `Three` and return those contents. |
  * | `Top`                  | Find and return the level one header with the title "Top"                                                                               |
  * | `Monthly..2025-02`     | Find the level one header with the title `Monthly` and return the first subheader named `2025-02` at any level inside                   |
  * | `Weekly!To Do`         | Find the level one header with the title `Weekly` and return the `To Do` table that it contains.                                        |
  * | `..!Status[12]`        | Find any `Status` table and return the 12th table row (note that row 0 is always the column headers).                                   |
  * | `..!Status[0][3]`      | ❌ Find any `Status` table and return the name of the 4th column (row 0 is the headers, and columns are zero indexed).                     |
  * | `..!Status[Key,rowId]` | Find any Status table and return the cell under the column `Key` with the row header `rowId`  **Note that this is column-first!**       |
  * | `..Weekly[0]`          | Any header with the title `Weekly` and return the first element it contains.                                                            |
  * | `Weekly[code][0]`      | ❌ Find the top `Weekly` header and return the first code block it contains.                                                             |
  * | `Weekly[0][4]`         | ❌ Find the top `Weekly` header, go to its first child and return that elements 5th child.                                               |
  * }}}
  */
object MarkdQL {

  /** @param query
    *   The string to query on.
    * @param md
    *   The markdown node that starts the query.
    * @param fail
    *   If true, and a match isn't found, then throw an exception.
    * @return
    */
  def query(query: String, md: MarkdNode, fail: Boolean = false): Seq[MarkdNode] = {
    val queryChain = LazyList.iterate(Query(query, md)) { _.next }
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
    *   The set of markdown nodes currently being queried (before the query is applied).
    * @param separator
    *   The separator before the string token, zero to two periods followed by an optional !.
    * @param token
    *   A string token to search for (if any)
    * @param index
    *   The contents of the index to be applied to the query (if any).
    * @param rest
    *   The remainder of the query to be applied next.
    */
  case class Query(separator: String, token: String, index: String, rest: String, mds: Seq[MarkdNode]) {

    lazy val isDone: Boolean = mds.isEmpty || token.isEmpty && index.isEmpty && rest.isEmpty

    lazy val next: Query = {
      val tokenMatch: Seq[MarkdNode] = this match {
        case Query("" | ".", "", _, _, md) => md
        case Query("!" | ".!", token, _, _, Seq(md: MarkdContainer[_])) =>
          md.mds.collectFirst { case tbl: Table if tbl.title == token => tbl }.toSeq
        case Query("..!", token, _, _, Seq(md: MarkdContainer[_])) =>
          md.collectFirstRecursive { case tbl: Table if tbl.title == token => tbl }.toSeq
        case Query("" | ".", token, _, _, Seq(md: MarkdContainer[_])) =>
          md.mds.collectFirst { case h @ Header(_, title, _*) if title == token => h }.toSeq
        case Query("..", token, _, _, Seq(md: MarkdContainer[_])) =>
          md.collectFirstRecursive { case h @ Header(_, title, _*) if title == token => h }.toSeq
      }

      (index, tokenMatch) match {
        case ("*", Seq(mdx: MarkdContainer[_])) => Query(rest, mdx.mds: _*)
        case (number, Seq(mdx: MarkdContainer[_])) if number.toIntOption.exists(_ >= 0) =>
          Query(rest, mdx.mds.lift(number.toIntOption.get).toSeq: _*)
        case (q, Seq(tbl: Table)) if q.contains(',') =>
          val (column, row) = q.span(_ != ',')
          Query(rest, tbl.get(column, row.tail).map(Paragraph(_)).toSeq: _*)
        case _ => Query(rest, tokenMatch: _*)
      }
    }
  }

  object Query {

    val QueryRegex: Regex =
      raw"""(?x)^
              (?<sep>\.{0,2}!?)                                    # Start with a separator of 0-2 periods
              (?:
                (?<token>                                          # Either a token and optional index in []
                    [^"\[.!][^.!\[]*
                    |
                    "(?:[^"\\]|\\.)+")
                (?:\[(?<optIndex>
                    [^"][^]]*
                    |
                    "(?:[^"\\]|\\.)*")])?
                |
                \[(?<index>                                        # Or no token and an index in square brackets
                    [^]"][^]]*
                    |
                    "(?:[^"\\]|\\.)*")]
              )
              (?<rest>.*)
             $$""".r

    def apply(query: String, mds: MarkdNode*): Query = {
      if (query == "" || query == ".") return Query("", "", "", "", mds)

      // Pop off the first separator, token and index from the path
      val m = QueryRegex.findFirstMatchIn(query).getOrElse {
        throw new UnrecognizedQueryException(s"Failed on :$query")
      }

      // If the token is present, then unquote it if it's quoted.
      val token = Option(m.group("token"))
        .map {
          case quoted if quoted.head == '"' => quoted.slice(1, quoted.length - 1).replaceAll(raw"\\(.)", "$1")
          case unquoted                     => unquoted
        }
        .getOrElse("")

      // Likewise for the optIndex or index group.
      val index = Option(m.group("optIndex"))
        .orElse(Option(m.group("index")))
        .map {
          case quoted if quoted.head == '"' => quoted.slice(1, quoted.length - 1).replaceAll(raw"\\(.)", "$1")
          case unquoted                     => unquoted
        }
        .getOrElse("")

      Query(m.group("sep"), token, index, m.group("rest"), mds)
    }
  }

}
