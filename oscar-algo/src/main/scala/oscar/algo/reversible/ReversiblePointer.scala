package oscar.algo.reversible

/**
 * A generic reversible pointer
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */

class ReversibleTrailEntry[T](reversible: ReversiblePointer[T], value: T) extends TrailEntry {
  override def restore(): Unit = reversible.restore(value)
}

class ReversiblePointer[@specialized T](final override val context: ReversibleContext, initialValue: T) extends Reversible {
  
  // Reference on the current value
  protected var pointer: T = initialValue
  
  @inline override def trailEntry: TrailEntry = {
    new ReversibleTrailEntry[T](this, pointer)
  }

  @inline final def setValue(value: T): Unit = {
    if (value != pointer) {
      trail()
      this.pointer = value
    }
  }

  /** @param value to assign */
  @inline final def value_= (value: T): Unit = setValue(value)
  
  /** @param value to assign */
  final def := (value: T): Unit = setValue(value)
  
  /** @return current value */
  @inline final def value: T = pointer

  /**
   * Check if the pointer is different from null
   * @return true if the pointer is != null, false otherwise
   */
  @inline final def hasValue[T]: Boolean = pointer != null

  /** @return the current pointer */
  @inline final def getValue(): T = pointer

  @inline final def restore(value: T): Unit = pointer = value

  override def toString(): String = if (hasValue) pointer.toString else ""
}

object ReversiblePointer {
  def apply[T](node: ReversibleContext, value: T): ReversiblePointer[T] = {
    new ReversiblePointer[T](node, value)
  }
  implicit def reversiblePointerToValue[@specialized T](reversible: ReversiblePointer[T]): T = reversible.value
}