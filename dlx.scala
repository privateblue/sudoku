package dlx

val X = 3

val Y = 2

val S = X * Y

opaque type Grid = Vector[Int]

object Grid:
  def apply(input: String): Grid =
    input
      .split("\n")
      .filterNot(_.isBlank())
      .flatMap(_.map(_.toString().toInt))
      .toVector

  val empty: Grid = 
    Vector.fill(S * S)(0)

extension (grid: Grid)
  def show: String =
    grid.grouped(S).map(_.mkString(" ")).mkString("\n")

  def set(i: Int, digit: Int): Grid =
    grid.updated(i, digit)

  def encodeRowIndex(i: Int, digit: Int): Int =
    (i / S) * (S * S) + (i % S) * S + (digit - 1)

  def decodeRowIndex(idx: Int): (Int, Int) =
    val r = idx / (S * S)
    val c = (idx % (S * S)) / S
    val i = r * S + c
    val digit = (idx % S) + 1
    (i, digit)

  def toMatrix: Matrix =
    val headers =
      (0 until (S * S)).map(i => s"c$i") ++ // cell
        (0 until S).flatMap(r => (1 to S).map(d => s"rw$r-$d")) ++ // row-number
        (0 until S).flatMap(c => (1 to S).map(d => s"cl$c-$d")) ++ // col-number
        (0 until S).flatMap(b => (1 to S).map(d => s"bx$b-$d")) // box-number

    val rows = for {
      i <- 0 until (S * S)
      digit <- 1 to S
      if grid(i) == 0 || grid(i) == digit
    } yield {
      val rowIdx = encodeRowIndex(i, digit)
      val r = i / S
      val c = i % S
      val box = (i / (S * Y)) * Y + ((i % S) / X)
      rowIdx -> Set(s"c$i", s"rw$r-$digit", s"cl$c-$digit", s"bx$box-$digit")
    }

    Matrix(rows.toMap, headers.toSet)

  def solve(): LazyList[Grid] =
    toMatrix
      .solve()
      .map(
        _.foldLeft(Grid.empty) { (g, rowIdx) =>
          val (i, digit) = decodeRowIndex(rowIdx)
          g.set(i, digit)
        }
      )

@main def run: Unit =
  val grid = Grid("""000500
                    |020130
                    |206000
                    |000003
                    |060350
                    |001006""".stripMargin('|'))

  val solutions = grid.solve()

  println(
    solutions.map(_.show).mkString("\n\n")
  )
