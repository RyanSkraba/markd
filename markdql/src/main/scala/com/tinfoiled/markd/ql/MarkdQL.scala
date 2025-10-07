package com.tinfoiled.markd.ql

import com.tinfoiled.markd._

import scala.annotation.unused
import scala.util.matching.Regex

/** Implements a simple query language on Markdown text.
  *
  * Examples:
  *
  * {{{
  * | Query | Description
  * |:---------------------|:-----------------------
  * | `One.Two.Three[*]`   | **Find** the level one header with the name "One", with a subheader named "Two" and a third-level header named "Three" and return its contents.
  * | `Top`                | Find and return the level one header with the title "Top"
  * | `Weekly..2025-02-14` | Find the level one header with the title "Weekly" and return the first subheader named "2025-02-14" at any level inside
  * | `Weekly.!To Do`      | Find the level one header with the title "Weekly" and return the To Do table that it contains.
  * | `Weekly.!To Do[y,x]` | In the table found above, look for the row with "x" as it's first element, and return the value in the "y" column (according to the table headers).
  * | `..Weekly[0]`        | ❌ Any header with the title "Weekly" and return the first element it contains.
  * | `Weekly[code][0]`    | ❌ Find the level one header with the title "Weekly" and return the first code block it contains.
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

    /** A query to apply on the current set of markdown nodes, with the first part of the query already parsed.
      *
      * A query looks somewhat like `.token[index].token[index].token[index]` (not all parts are mandatory)
      *
      * @param mds
      *   The set of markdown nodes currently being queried
      * @param separator
      *   The separator before the string token, either "", "." or ".."
      * @param token
      *   A string token to search for (if any)
      * @param index
      *   The contents of the index to be applied to the query (if any).
      * @param rest
      *   The remainder of the query to be applied next.
      */
    case class Query(mds: Seq[MarkdNode], separator: String, token: String, index: String, rest: String) {

      lazy val isDone: Boolean = mds.isEmpty || token.isEmpty && index.isEmpty && rest.isEmpty

      lazy val next: Query = {
        val tokenMatch: Seq[MarkdNode] = this match {
          case Query(md, "" | ".", "", _, _) => md
          case Query(Seq(md: MarkdContainer[_]), "" | ".", token, _, _) if token.startsWith("!") =>
            md.mds.collectFirst { case tbl: Table if tbl.title == token.tail => tbl }.toSeq
          case Query(Seq(md: MarkdContainer[_]), "..", token, _, _) if token.startsWith("!") =>
            md.collectFirstRecursive { case tbl: Table if tbl.title == token.tail => tbl }.toSeq
          case Query(Seq(md: MarkdContainer[_]), "" | ".", token, _, _) =>
            md.mds.collectFirst { case h @ Header(_, title, _*) if title == token => h }.toSeq
          case Query(Seq(md: MarkdContainer[_]), "..", token, _, _) =>
            md.collectFirstRecursive { case h @ Header(_, title, _*) if title == token => h }.toSeq
        }

        (index, tokenMatch) match {
          case ("*", Seq(mdx: MarkdContainer[_])) => Query(mdx.mds, rest)
          case (q, Seq(tbl: Table)) if q.contains(',') =>
            val (column, row) = q.span(_ != ',')
            Query(Seq(Paragraph(tbl.apply(column, row.tail))), rest)
          case _ => Query(tokenMatch, rest)
        }
      }
    }

    object Query {
      val QueryTRegex: Regex = raw"^(?<sep>\.{0,2})(?<token>[^.\[]+)(\[(?<index>[^]]+)])?(?<rest>.*)$$".r
      val QueryIRegex: Regex = raw"^(?<sep>\.{0,2})(?<token>[^.\[]*)(\[(?<index>[^]]+)])(?<rest>.*)$$".r

      def apply(mds: Seq[MarkdNode], query: String): Query = {
        query match {
          case "" | "." => Query(mds, "", "", "", "")
          case QueryTRegex(sep, token, _, index, rest) =>
            Query(mds, sep, token, Option(index).getOrElse(""), rest)
          case QueryIRegex(sep, token, _, index, rest) =>
            Query(mds, sep, token, Option(index).getOrElse(""), rest)
          case _ =>
            throw new UnrecognizedQueryException(s"Failed on :$query")
        }
      }
    }

    class UnrecognizedQueryException(msg: String) extends Exception(msg)

    val queryChain = LazyList.iterate(Query(Seq(md), query)) { _.next }
    try {
      queryChain.dropWhile(!_.isDone).head.mds
    } catch { case _: UnrecognizedQueryException => sys.error(s"Unrecognized query: $query") }
  }
}
