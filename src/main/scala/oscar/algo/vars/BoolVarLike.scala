package oscar.algo.vars

/**
 * A trait that all objects that behave like a BoolVar should implement
 */
trait BoolVarLike extends IntVarLike
{
  final override def isContinuous: Boolean = true

  /** @return true if the variable is bound and bound to value 1 */
  def isTrue: Boolean = isBoundTo(1)

  /** @return true if the variable is bound and bound to value 0 */
  def isFalse: Boolean = isBoundTo(0)

  /** Returns `true` if the domain contains 1. */
  def containsTrue: Boolean = max == 1

  /** Returns `true` if the domain contains 0. */
  def containsFalse: Boolean = min == 0
}