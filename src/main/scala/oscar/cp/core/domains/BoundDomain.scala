package oscar.cp.core.domains

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleContext, ReversibleInt}

import scala.util.Random

/**
 *  @author Renaud Hartert
 *  @author Pierre Schaus
 */

class BoundDomain(override val context: ReversibleContext, val minValue: Int, val maxValue: Int) extends IntervalDomain {

  private val _maxValue = if (maxValue - minValue - 1 < Int.MaxValue) maxValue
  else sys.error("the domain contains more than Int.MaxValue values")

  private val _min = new ReversibleInt(context, minValue)
  private val _max = new ReversibleInt(context, _maxValue)

  @inline
  override final def size: Int = _max - _min + 1
  
  @inline
  override final def isEmpty: Boolean = _max.value < _min.value

  @inline
  override final def isBound: Boolean = _max.value == _min.value

  @inline
  override final def min: Int = {
    assert(!isEmpty, "the domain is empty")
    _min.value
    // if (isEmpty) sys.error("the domain is empty")
    // else _min.value
  }

  @inline
  override final def max: Int = {
    assert(!isEmpty, "the domain is empty")
    _max.value
    // if () sys.error("the domain is empty")
    // else _max.value
  }

  @inline
  override final def randomValue(rand: Random): Int = {
    if (isEmpty) sys.error("the domain is empty")
    else {
      val minVal = _min.value
      minVal + rand.nextInt(_max.value - minVal + 1)
    }
  }

  /**
   * @param value
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */
  @inline
  override final def nextValue(value: Int): Int = {
    if (isEmpty || value > _max) value - 1
    else if (value < _min) _min
    else value
  }

  /**
   * @param value
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */
  @inline
  override final def prevValue(value: Int): Int = {
    if (isEmpty || value < _min) value + 1
    else if (value > _max) _max
    else value
  }

  @inline
  override final def hasValue(value: Int): Boolean = {
    value <= _max.value && value >= _min.value
  }

  override def iterator: Iterator[Int] = new Iterator[Int] {
    var i = _min.value - 1
    val n = _max.value
    override def hasNext: Boolean = i < n
    override def next(): Int = {
      i += 1
      i
    }
  }
  
  @inline
  override final def foreach[U](f: Int => U): Unit = {
    var i = _min.value
    val n = _max.value
    while (i <= n) {
      f(i)
      i += 1
    }
  }

  @inline
  override final def updateMax(value: Int): Unit = {
    if (value < _min.value) {
      _max.value = _min.value - 1
      throw Inconsistency
    }
    else if (value >= _max.value) {
      //TODO remove me
    }
    else {
      _max.value = value
    }
  }

  @inline
  override final def updateMin(value: Int): Unit = {
    if (value > _max.value) {
      _min.value = _max.value + 1
      throw Inconsistency
    }
    else if (value <= _min.value) {
      //TODO remove me
    }
    else {
      _min.value = value
    }
  }

  @inline
  override final def assign(value: Int): Unit = {
    if (!hasValue(value)) {
      _max.value = value
      _min.value = value + 1
      throw Inconsistency
    } else {
      _min.value = value
      _max.value = value
    }
  }

  override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    (oldMin to _min.value - 1).iterator ++ (_max.value + 1 to oldMax).iterator
  }

  override def toString: String = {
    if (isEmpty) "phi"
    else "{" + _min.value + ".." + _max.value + "}"
  }
}