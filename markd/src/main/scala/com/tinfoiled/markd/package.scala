package com.tinfoiled

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

  /** Helps build the model when parsing contents.
    * @param sortLinkRefs
    *   Whether to sort and deduplicate LinkRefs while parsing (true by default).
    */
  class ParserCfg(sortLinkRefs: Boolean = true) {

    /** If sorting, provides a key to use from the linkref, allowing custom grouping and deduplication of the links. The
      * linkref will be sorted and deduplicated based on this key. By default, the [[LinkRef.ref]] is used directly.
      */
    private def linkSorter(): PartialFunction[LinkRef, (String, LinkRef)] = { case lr => lr.ref -> lr }

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
