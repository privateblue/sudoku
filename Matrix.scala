package dlx

case class Matrix(
    columns: Map[String, Set[Int]],
    rows: Map[Int, Set[String]],
    headers: Set[String]
):
  def isEmpty: Boolean = headers.isEmpty

  def selectColumn: Option[String] =
    headers.toSeq.sortBy(col => columns(col).size).headOption

  def getRow(rowIdx: Int): Set[String] = rows.getOrElse(rowIdx, Set.empty)

  def getColumn(col: String): Set[Int] = columns.getOrElse(col, Set.empty)

  infix def cover(col: String): Matrix =
    val rowsToRemove = getColumn(col)
    val columnsToUpdate = rowsToRemove.flatMap(getRow)

    val newColumns = columnsToUpdate.foldLeft(columns - col) { (cols, c) =>
      cols.get(c) match {
        case Some(colRows) => cols.updated(c, colRows -- rowsToRemove)
        case None          => cols
      }
    }

    val newRows = rows -- rowsToRemove
    val newHeaders = headers - col

    Matrix(newColumns, newRows, newHeaders)

  def solve(solution: List[Int] = List.empty): LazyList[List[Int]] =
    if isEmpty then LazyList(solution.reverse)
    else
      selectColumn match
        case None      => LazyList.empty
        case Some(col) =>
          val rowsInColumn = getColumn(col)

          if rowsInColumn.isEmpty then LazyList.empty
          else
            rowsInColumn.to(LazyList).flatMap { rowIdx =>
              val columnsInRow = getRow(rowIdx)
              val reducedMatrix = columnsInRow.foldLeft(this)(_ cover _)
              reducedMatrix.solve(rowIdx :: solution)
            }

  def solveFirst(solution: List[Int] = List.empty): Option[List[Int]] =
    if isEmpty then Some(solution.reverse)
    else
      selectColumn.flatMap { col =>
        val rowsInColumn = getColumn(col)

        if rowsInColumn.isEmpty then None
        else
          rowsInColumn
            .to(LazyList)
            .flatMap { rowIdx =>
              val columnsInRow = getRow(rowIdx)
              val reducedMatrix = columnsInRow.foldLeft(this)(_ cover _)
              reducedMatrix.solveFirst(rowIdx :: solution)
            }
            .headOption
      }

object Matrix:
  def apply(
      rows: Map[Int, Set[String]],
      headers: Set[String]
  ): Matrix =
    Matrix(
      columns = headers.map { col =>
        col -> rows.collect {
          case (idx, cols) if cols.contains(col) => idx
        }.toSet
      }.toMap,
      rows = rows,
      headers = headers
    )
