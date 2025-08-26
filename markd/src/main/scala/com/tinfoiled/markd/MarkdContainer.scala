package com.tinfoiled.markd

/** A markdown node that can contain other nodes. */
trait MarkdContainer[T <: MarkdNode] extends MarkdNode {

  type Self <: MarkdContainer[T]

  /** The children of this node. */
  def mds: Seq[T]

  /** Write this node to the builder.
    *
    * @param sb
    *   The builder to write to.
    * @param prev
    *   If known, the previous node written to the builder. This can be used to adjust spacing.
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
      for (md: Seq[MarkdNode] <- mds.sliding(2) if md.size == 2) {
        md.last.buildPreSpace(sb, Some(md.head), cfg)
        md.last.build(sb, cfg)
      }
    }
    sb
  }

  /** Create a copy of the node with the new children.
    * @param newMds
    *   The children to replace the existing ones in the copy.
    */
  def copyMds(newMds: Seq[T]): Self

  /** Create a copy of the list of children, replacing some as necessary.
    *
    * A partial function matches and replaces MarkdNode in the children. If the partial function is defined for one of
    * the children, it supplies the list of replacements. It matches on the node (or None to match the end of the list)
    * and its index.
    *
    * @param filter
    *   True if non-matching children should be removed, false to leave non-matching elements unchanged.
    * @param pf
    *   A partial function to replace markd elements.
    * @return
    *   A copy of this [[MarkdContainer]] with the replaced subelements
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

  /** Copies this node, but flatMapping the first matching subelement to new values.
    *
    * A partial function matches and replaces Markd subelements. If the partial function is defined for one of the
    * subelements, it supplies the list of replacements.
    *
    * @param ifNotFound
    *   If nothing is matched, add these elements to the end of the list and try again. This permits insert-and-update
    *   replacements in the children.
    * @param replace
    *   If true, when falling back on ifNotFound, then replace all of the children instead of appending the new elements
    *   when trying again. This can be used to control the modification of the new elements, such as prepending or
    *   filtering.
    * @param pf
    *   A partial function to replace markd elements.
    * @return
    *   A copy of this [[MarkdContainer]] with the replaced subelements
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
    *   If true, when falling back on ifNotFound, then replace all of the children instead of appending the new elements
    *   when trying again. This can be used to control the modification of the new elements, such as prepending or
    *   filtering.
    * @param pf
    *   A partial function to replace markd elements.
    * @return
    *   A copy of this [[MarkdContainer]] with the replaced subelements
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
    *   A copy of this [[MarkdContainer]] with the replaced subelements
    */
  def mapFirstIn(ifNotFound: => T)(pf: PartialFunction[T, T]): Self =
    mapFirstIn(ifNotFound = Seq(ifNotFound))(pf)

  /** Finds the first [[MarkdNode]] element recursively in this element for which the given partial function is defined,
    * and applies the partial function to it.
    *
    * @param pf
    *   the partial function
    * @return
    *   an option value containing pf applied to the first value for which it is defined, or `None` if none exists.
    */
  def collectFirstRecursive[B](pf: PartialFunction[MarkdNode, B]): Option[B] =
    if (pf.isDefinedAt(this)) Some(pf(this))
    else
      mds
        .to(LazyList)
        .map {
          case md if pf.isDefinedAt(md) => Some(pf(md))
          case md: MarkdContainer[_]    => md.collectFirstRecursive(pf)
          case _                        => None
        }
        .find(_.isDefined)
        .flatten

  def replaceRecursively(pf: PartialFunction[MarkdNode, MarkdNode]): Self = {
    replaceIn() {
      case (Some(md), _) if pf.isDefinedAt(md) => Seq(pf.apply(md).asInstanceOf[T])
      case (Some(md: MarkdContainer[_]), _)    => Seq(md.replaceRecursively(pf).asInstanceOf[T])
      case (Some(md), _)                       => Seq(md)
      case (None, _)                           => Seq.empty
    }
  }
}
