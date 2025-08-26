package com.tinfoiled.markd

import play.api.libs.json.Json

import scala.util.Try

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
case class Code(code_type: String, content: String) extends MarkdNode {

  private lazy val builtContent: String = (code_type, content) match {
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
