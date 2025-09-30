package com.tinfoiled.markd.ql

import com.tinfoiled.markd._

import scala.util.matching.Regex

object MarkdQL {
  def query(query: String, md: MarkdNode): Seq[MarkdNode] = {

    val QueryRegex: Regex = raw"^(?<sep>\.*)(?<token>[^.\[]*)(?<rest>(\[(?<index>[^]]+)])?.*)$$".r

    def queryInternal(in: (String, Seq[MarkdNode])): (String, Seq[MarkdNode]) = in match {
      case ("[*]", Seq(md: MarkdContainer[_])) => ("", md.mds)
      case (q, md) if q.head == '.'            => (q.tail, md)
      case (QueryRegex(sep, token, rest, _*), Seq(h: MarkdContainer[_])) if token.nonEmpty =>
        (rest, h.mds.collectFirst { case h @ Header(_, title, _*) if title == token => h }.toSeq)
      case _ => sys.error(s"Unrecognized query: $query")
    }

    LazyList
      .iterate((query, Seq(md))) { queryInternal }
      .dropWhile(acc => acc._1.nonEmpty && acc._2.nonEmpty)
      .head match {
      case ("", md)   => md
      case (_, Seq()) => Seq()
    }
  }
}
