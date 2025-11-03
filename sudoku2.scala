package optimized

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

case class Mask(lo: Long, hi: Long):
  inline def intersect(other: Mask): Boolean =
    (lo & other.lo) != 0L || (hi & other.hi) != 0L
  inline def union(other: Mask): Mask = Mask(lo | other.lo, hi | other.hi)

  def toPattern: Vector[Int] =
    val arr = Array.fill[Int](S)(-1)
    var bits = lo
    while bits != 0L do
      val lsb = bits & -bits
      val idx = java.lang.Long.numberOfTrailingZeros(lsb)
      val r = idx / S
      val c = idx % S
      arr(r) = c + 1
      bits ^= lsb
    bits = hi
    while bits != 0L do
      val lsb = bits & -bits
      val idx = 64 + java.lang.Long.numberOfTrailingZeros(lsb)
      val r = idx / S
      val c = idx % S
      arr(r) = c + 1
      bits ^= lsb
    arr.toVector

object Mask:
  def from(pattern: Vector[Int]): Mask =
    val (lo, hi) = 0.until(S).foldLeft((0L, 0L)) { case ((lo, hi), row) =>
      val idx = row * S + pattern(row) - 1
      if idx < 64 then (lo | (1L << idx), hi) else (lo, hi | (1L << (idx - 64)))
    }
    Mask(lo, hi)

extension (masks: Array[Mask])
  def toGrid: Grid =
    masks.zipWithIndex.foldLeft(Grid.empty) {
      case (grid, (mask, i)) if i > 0 => grid.set(i, mask.toPattern)
      case (grid, _)                  => grid
    }

object Grid:
  def apply(input: String): Grid =
    Grid(
      input
        .split("\n")
        .filterNot(_.isBlank())
        .map(_.toArray.map(_.toString().toInt))
    )

  def empty: Grid =
    Grid(Array.fill(S, S)(0))

case class Grid(grid: Array[Array[Int]]):
  def show: String =
    grid.map(_.mkString).mkString("\n")

  def set(value: Int, pattern: Vector[Int]): Grid =
    Grid(grid.zip(pattern).map { case (row, i) => row.updated(i - 1, value) })

  val compatibleMasks: Array[Array[Mask]] =
    val buf = Array.fill(S + 1)(collection.mutable.ArrayBuffer.empty[Mask])
    for
      pattern <- patterns
      digit <- 1.to(S)
    do
      var ok = true
      var row = 0
      while row < S && ok do
        val value = grid(row)(pattern(row) - 1)
        ok = value == digit || (value == 0 && !grid(row).contains(digit))
        row += 1
      if ok then buf(digit) += Mask.from(pattern)
    buf.map(_.toArray)

  def solve(): Option[Grid] =
    var result = Option.empty[Grid]
    var done = false
    val chosen = new Array[Mask](S + 1)
    var used = Mask(0L, 0L)
    val remaining = collection.mutable.ArrayBuffer.from(1.to(S))

    def backtrack(): Unit =
      if done then return

      if remaining.isEmpty then
        val solution = new Array[Mask](S + 1)
        System.arraycopy(chosen, 0, solution, 0, chosen.length)
        result = Some(solution.toGrid)
        done = true
        return

      var bestDigit = -1
      var bestCount = Int.MaxValue

      var i = 0
      while i < remaining.length do
        val digit = remaining(i)
        var count = 0
        val masks = compatibleMasks(digit)

        var j = 0
        while j < masks.length do
          val mask = masks(j)
          if !mask.intersect(used) then count += 1
          j += 1

        if count < bestCount then
          bestCount = count
          bestDigit = digit

        if bestCount == 0 then return

        i += 1

      remaining -= bestDigit
      val digits = compatibleMasks(bestDigit)

      var k = 0
      while k < digits.length && !done do
        val mask = digits(k)
        if !mask.intersect(used) then
          chosen(bestDigit) = mask
          val prevMask = used
          used = used.union(mask)
          backtrack()
          used = prevMask
        k += 1

      remaining += bestDigit

    backtrack()
    result

@main def run() =
  val solution = Grid("""
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
    solution.map(_.show).getOrElse("no solution found")
  )
