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
    grid.grouped(S).map(_.mkString(" ")).mkString("\n")

  def set(digit: Int, pattern: Vector[Int]): Grid =
    pattern.zipWithIndex.foldLeft(grid) { case (g, (col, row)) =>
      g.updated(row * S + col - 1, digit)
    }

  def compatible(digit: Int, pattern: Vector[Int]): Boolean =
    pattern.zipWithIndex.forall { case (col, row) =>
      grid(row * S + col - 1) == digit ||
      (grid(row * S + col - 1) == 0 &&
        !grid
          .slice(row * S, row * S + S)
          .contains(digit))
    }

  def compatiblePatterns: Vector[Vector[Vector[Int]]] =
    1.to(S).map(digit => patterns.filter(grid.compatible(digit, _))).toVector

  def solve(constraints: Seq[Constraint]): Vector[Grid] =
    def solve0(
        g: Grid,
        digits: List[Int],
        ps: Vector[Vector[Vector[Int]]]
    ): Vector[Grid] =
      digits match
        case Nil                => Vector(g)
        case digit :: remaining =>
          ps(digit - 1).flatMap {
            case p if g.compatible(digit, p) =>
              val g2 = g.set(digit, p)
              val matched = constraints.forall(c =>
                c.region.exists(g2(_) == 0) || c.predicate(c.region.map(g2(_)))
              )
              if matched then solve0(g2, remaining, ps) else Vector.empty
            case _ => Vector.empty
          }

    val cp = compatiblePatterns
    val digits = cp.map(_.size).zipWithIndex.sortBy(_._1).map(_._2 + 1).toList
    solve0(grid, digits, cp)

@main def run() =
  val grid = Grid("""103402
                    |000000
                    |306504
                    |405603
                    |000000
                    |204301""".stripMargin('|'))

  val constraints = Seq(
    Unequal(Seq(0, 1, 6, 7, 12, 13)),
    Unequal(Seq(2, 3, 8, 9, 14, 15)),
    Unequal(Seq(4, 5, 10, 11, 16, 17)),
    Unequal(Seq(18, 19, 24, 25, 30, 31)),
    Unequal(Seq(20, 21, 26, 27, 32, 33)),
    Unequal(Seq(22, 23, 28, 29, 34, 35))
  )

  val solutions = grid.solve(constraints)

  println(
    solutions.map(_.show).mkString("\n\n")
  )
