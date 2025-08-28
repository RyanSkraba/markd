package com.tinfoiled.markd

import com.tinfoiled.markd.Align.Align

import scala.util.matching.Regex

/** Alignment in a Table. */
object Align extends Enumeration {
  type Align = Value
  val LEFT, CENTER, RIGHT = Value
}

/** Markdown table.
  *
  * {{{
  * | Col1     |      Col2     |  Col3 |
  * |----------|:-------------:|------:|
  * | col 1 is |  left-aligned | 1600€ |
  * | col 2 is |    centered   |   12€ |
  * | col 3 is | right-aligned |    1€ |
  * }}}
  *
  * @param aligns
  *   The alignment for each column.
  * @param mds
  *   The table rows, including the column headers (as the first row) and cell values (all subsequent rows).
  */
case class Table(aligns: Seq[Align], mds: Seq[TableRow]) extends MarkdContainer[TableRow] {

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
  def apply(row: Int): TableRow = mds.applyOrElse(row, (_: Int) => TableRow())

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
      .padTo(row + 1, TableRow())
      .updated(row, new TableRow(cellsUpdated: _*))

    Table(
      if (row == 0) aligns.padTo(column + 1, Align.LEFT) else aligns,
      mds = rowsUpdated
    )
  }

  /** Creates a new table from this one with the given cell value updated. Note that the row head will match the column
    * headers as well.
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

  /** Creates a new table from this one with the given cell value updated. Note that the row head will match the column
    * headers as well.
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
  private val CellRegex: Regex = raw"(?<!\\)\|".r

  private val AlignmentCellRegex: Regex = raw"^\s*(:-+:|---+|:--+|-+-:)\s*$$".r

  /** Shortcut method just for the varargs */
  def from(aligns: Seq[Align], mds: TableRow*): Table = Table(aligns, mds)

  /** Determines if some content can be reasonably parsed into a [[Table]].
    * @param content
    *   The string contents to parse.
    * @return
    *   An {{Option}} containing a [[Table]] if it is possible to construct, or None if it isn't.
    */
  def parse(content: String): Option[Table] = {
    val prelines = content.split("\n").toSeq.map(parseRow)
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
    // If the alignment row removed any columns, then this is not a Table
    if (aligns.length < lines(1).length) return None

    val rows = lines.patch(1, Seq.empty, 1).map(_.map(_.trim)).map(TableRow.apply)

    Some(Table(aligns, rows))
  }

  /** Parses a string into cells, removing all trailing whitespace-only cells.
    */
  def parseRow(content: String): Seq[String] = {
    val values = CellRegex.pattern.split(content, -1).toSeq
    if (values.last.nonEmpty) values
    else {
      val lastNonEmpty = values.lastIndexWhere(!_.isBlank)
      values.dropRight(values.length - lastNonEmpty - 1)
    }
  }
}

case class TableRow(cells: String*) extends MarkdNode {

  /** The row header is the leftmost cell. */
  lazy val head: String = cells.headOption.getOrElse("")

  def apply(i: Int): String = cells.applyOrElse(i, (_: Int) => "")

  def updated(i: Int, c: String): TableRow =
    TableRow(cells = cells.padTo(i + 1, "").updated(i, c).reverse.dropWhile(_.isEmpty).reverse: _*)

  /** Write this node to the builder.
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
