package generalized

sealed trait Constraint:
  /** List of cells */
  def region: Seq[Int]

  /** Predicate on the values of the above cells */
  def predicate(values: Seq[Int]): Boolean

case class Equal(region: Seq[Int]) extends Constraint:
  def predicate(values: Seq[Int]): Boolean =
    values.distinct.size == 1

case class Unequal(region: Seq[Int]) extends Constraint:
  def predicate(values: Seq[Int]): Boolean =
    values.distinct.size == values.size

case class SumEquals(region: Seq[Int], target: Int) extends Constraint:
  def predicate(values: Seq[Int]): Boolean =
    values.sum == target

case class SumLessThan(region: Seq[Int], target: Int) extends Constraint:
  def predicate(values: Seq[Int]): Boolean =
    values.sum < target

case class SumGreaterThan(region: Seq[Int], target: Int) extends Constraint:
  def predicate(values: Seq[Int]): Boolean =
    values.sum > target
