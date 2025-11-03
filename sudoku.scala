package base

val B = 3

val S = B * B

def blockOf(n: Int): Vector[Int] =
  1.to(S).grouped(B).find(_.contains(n)).get.toVector

def generatePatterns(v: Vector[Int] = Vector.empty): Vector[Vector[Int]] =
  v match
    case _ if v.size == S => Vector(v)
    case _                =>
      val exclude =
        0.until(v.size % B).flatMap(i => blockOf(v(v.size - 1 - i))) ++ v
      1.to(S)
        .filterNot(exclude.contains)
        .map(v.appended)
        .toVector
        .flatMap(generatePatterns)

val patterns: Vector[Vector[Int]] = generatePatterns()

opaque type Grid = Vector[Vector[Int]]

object Grid:
  def apply(input: String): Grid =
    input
      .split("\n")
      .filterNot(_.isBlank())
      .map(_.toVector.map(_.toString().toInt))
      .toVector

extension (grid: Grid)
  def show: String =
    grid.map(_.mkString).mkString("\n")

  def set(value: Int, pattern: Vector[Int]): Grid =
    grid.zip(pattern).map { case (row, i) => row.updated(i - 1, value) }

  def compatible(value: Int, pattern: Vector[Int]): Boolean =
    grid.zip(pattern).forall { case (row, i) =>
      row(i - 1) == value || (row(i - 1) == 0 && !row.contains(value))
    }

  def compatiblePatterns: Vector[Vector[Vector[Int]]] =
    1.to(S).map(n => patterns.filter(grid.compatible(n, _))).toVector

  def solve(
      value: Int = 1,
      ps: Vector[Vector[Vector[Int]]] = grid.compatiblePatterns
  ): Vector[Grid] =
    if value > S then Vector(grid)
    else
      ps(value - 1)
        .filter(grid.compatible(value, _))
        .flatMap(grid.set(value, _).solve(value + 1, ps))

@main def run() =
  val solutions = Grid("""
                        |200000000
                        |900000180
                        |807043065
                        |006500004
                        |000000006
                        |090070001
                        |000830000
                        |040065000
                        |050200070""".stripMargin('|')).solve()

  println(
    solutions.map(_.show).mkString("\n\n")
  )
