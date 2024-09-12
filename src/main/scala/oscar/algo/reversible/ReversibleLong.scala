package oscar.algo.reversible

/**
 * @author Renaud Hartert ren.hartert@gmail.com
 */

class ReversibleLongTrailEntry(reversible: ReversibleLong, value: Long) extends TrailEntry {
  @inline override final def restore(): Unit = reversible.restore(value)
}

class ReversibleLong(node: ReversibleContext, value: Long) extends ReversiblePointer[Long](node, value) {

  @inline final override def trailEntry = new ReversibleLongTrailEntry(this, pointer)
  
  /** Increments the reversible integer by one */
  def incr(): Long = {
    trail()
    pointer += 1
    pointer
  }

  /** Decrements the reversible integer by one */
  def decr(): Long = {
    trail()
    pointer -= 1
    pointer
  }

  /** Increments the reversible integer by i */
  def +=(i: Long): Long = {
    trail()
    pointer += i
    pointer
  }

  /** Decrements the reversible integer by i */
  def -=(i: Long): Long = {
    trail()
    pointer -= i
    pointer
  }
}

object ReversibleLong {
  def apply(value: Long)(implicit context: ReversibleContext) = new ReversibleLong(context, value)
}