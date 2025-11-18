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
      constraints: Seq[Constraint] = Seq.empty,
      value: Int = 1,
      ps: Vector[Vector[Vector[Int]]] = grid.compatiblePatterns
  ): Vector[Grid] =
    if value > S then Vector(grid)
    else
      ps(value - 1)
        .flatMap { 
          case p if grid.compatible(value, p) => 
            val updated = grid.set(value, p)

            val matched = constraints.forall(c =>
              c.region.exists(updated(_) == 0) || c.predicate(c.region.map(updated(_)))
            )

            if matched then updated.solve(constraints, value + 1, ps) else Vector.empty
          case _ => Vector.empty
        }

sealed trait Constraint {
  def region: Seq[Int]
  def predicate(values: Seq[Int]): Boolean
}

case class Equal(override val region: Seq[Int]) extends Constraint {
  override def predicate(values: Seq[Int]): Boolean =
    values.distinct.size == 1
}

case class Unequal(override val region: Seq[Int]) extends Constraint {
  override def predicate(values: Seq[Int]): Boolean =
    values.distinct.size == values.size
}

case class SumEquals(override val region: Seq[Int], target: Int) extends Constraint {
  override def predicate(values: Seq[Int]): Boolean =
    values.sum == target
}

case class SumLessThan(override val region: Seq[Int], target: Int) extends Constraint {
  override def predicate(values: Seq[Int]): Boolean =
    values.sum < target
}

case class SumGreaterThan(override val region: Seq[Int], target: Int) extends Constraint {
  override def predicate(values: Seq[Int]): Boolean =
    values.sum > target
}

@main def run() =
  val grid = Grid("""000000
                    |000000
                    |000000
                    |000000
                    |000000
                    |000000""".stripMargin('|'))

  val constraints = Seq(
    SumEquals(region = Seq(8, 9, 14, 15, 20, 21, 26, 27), target = 31),
    Unequal(region = Seq(22, 23, 28, 29)),
    Equal(region = Seq(7, 12, 27, 32)),
    Equal(region = Seq(21, 26)),
    SumLessThan(region = Seq(5, 10, 15, 20, 25, 30), target = 12),
    SumEquals(region = Seq(0,5,30,35), target = 13),
    SumLessThan(region = Seq(10, 11, 16, 17), target = 8),
  )

  val solutions = grid.solve(constraints)

  println(
    solutions.map(_.show).mkString("\n\n")
  )
