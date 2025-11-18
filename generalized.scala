package generalized

val X = 3

val Y = 2

val S = X * Y

def blockOf(n: Int): Vector[Int] =
  1.to(S).grouped(X).find(_.contains(n)).get.toVector

def generatePatterns(v: Vector[Int] = Vector.empty): Vector[Vector[Int]] =
  v match
    case _ if v.size == S => Vector(v)
    case _                =>
      val exclude =
        0.until(v.size % Y).flatMap(i => blockOf(v(v.size - 1 - i))) ++ v
      1.to(S)
        .filterNot(exclude.contains)
        .map(v.appended)
        .toVector
        .flatMap(generatePatterns)

val patterns: Vector[Vector[Int]] = generatePatterns()

//

opaque type Grid = Vector[Int]

object Grid:
  def apply(input: String): Grid =
    input
      .split("\n")
      .filterNot(_.isBlank())
      .flatMap(_.map(_.toString().toInt))
      .toVector

extension (grid: Grid)
  def show: String =
    grid.grouped(S).map(_.mkString).mkString("\n")

  def set(value: Int, pattern: Vector[Int]): Grid =
    pattern.zipWithIndex.foldLeft(grid) { 
      case (g, (col, row)) => g.updated(row * S + col - 1, value) 
      }

  def compatible(value: Int, pattern: Vector[Int]): Boolean = 
    pattern.zipWithIndex.forall { case (col, row) =>
      grid(row * S + col - 1) == value || 
      (grid(row * S + col - 1) == 0 && !grid.slice(row * S, row * S + S).contains(value))
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
                        |516020
                        |000100
                        |602000
                        |000000
                        |000240
                        |100065""".stripMargin('|')).solve()

  println(
    solutions.map(_.show).mkString("\n\n")
  )
