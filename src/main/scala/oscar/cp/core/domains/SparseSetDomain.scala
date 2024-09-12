package oscar.cp.core.domains

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleContext, ReversibleInt}

import scala.util.Random

/**
 *  @author Renaud Hartert
 *  @author Pierre Schaus
 */
final class SparseSetDomain(override val context: ReversibleContext, val minValue: Int, val maxValue: Int) extends IntDomain {

  private val _min = new ReversibleInt(context, minValue)
  private val _max = new ReversibleInt(context, maxValue)

  // Values with a position strictly lower than size are possible
  private val _size = new ReversibleInt(context, maxValue - minValue + 1)

  // Domain representation
  private val values = Array.tabulate(size)(i => i + minValue)
  private val indexes = Array.tabulate(size)(i => i)

  @inline
  override final def size: Int = _size.value

  @inline
  override final def isBound: Boolean = _size.value == 1

  @inline
  override final def min: Int = {
    if (isEmpty) sys.error("the domain is empty")
    else {
      val v = _min.value
      if (hasValue(v)) v
      else updateMinValue()
    }
  }

  @inline
  override final def max: Int = {
    if (isEmpty) sys.error("the domain is empty")
    else {
      val v = _max.value
      if (hasValue(v)) v
      else updateMaxValue()
    }
  }

  @inline
  override final def randomValue(rand: Random): Int = {
    if (isEmpty) sys.error("the domain is empty")
    else {
      val pos = rand.nextInt(_size.value)
      values(pos)
    }
  }

  @inline
  override final def hasValue(v: Int): Boolean = {
    if (v < minValue || v >= minValue + indexes.length) false
    else indexes(v - minValue) < _size.value
  }

  @inline
  override final def isEmpty = _size.value == 0

  @inline
  private def updateMaxValue(): Int = {
    var i = 0
    var max = values(i)
    val s = _size.value
    while (i < s) {
      val v = values(i)
      if (v > max) max = v
      i += 1
    }
    val newMax = max
    _max.value = newMax
    newMax
  }

  @inline
  private def updateMinValue(): Int = {
    var i = 0
    var min = values(i)
    val s = _size.value
    while (i < s) {
      val v = values(i)
      if (v < min) min = v
      i += 1
    }
    val newMin = min
    _min.value = newMin
    newMin
  }

  override def iterator: Iterator[Int] = new Iterator[Int] {
    var i = 0
    def next(): Int = {
      val v = values(i)
      i += 1
      v
    }
    def hasNext: Boolean = i < _size.value
  }

  @inline
  override final def foreach[U](f: Int => U): Unit = {
    var i = 0
    val n = _size.value
    while (i < n) {
      f(values(i))
      i += 1
    }
  }

  override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    (oldMin until minValue).iterator ++ delta(oldSize) ++ (maxValue + 1 to oldMax).iterator
  }

  private def delta(oldSize: Int): Iterator[Int] = {
    var ind = size
    new Iterator[Int] {
      override def next(): Int = {
        val v = values(ind)
        ind += 1
        v
      }
      override def hasNext: Boolean = {
        ind < oldSize && ind < values.size
      }
    }
  }

  @inline
  override final def removeValue(value : Int): Unit = {
    if (hasValue(value)) {
      val s = _size.value - 1
      // Swaps the values
      val v1 = value - minValue
      val v2 = values(s) - minValue
      val i1 = indexes(v1)
      val i2 = indexes(v2)
      values(i1) = values(s)
      values(i2) = value
      indexes(v1) = i2
      indexes(v2) = i1
      // Adjusts the size accordingly
      _size.value = s
      if (s == 0)
        throw Inconsistency
    }
  }

  /**
   * @param v
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */
  @inline
  override final def nextValue(v: Int): Int = {
    if (isEmpty) sys.error("empty domain")
    else {
      var cv = v
      while (cv <= max) {
        if (hasValue(cv)) {
          return cv
        }
        cv += 1
      }
      v - 1
    }
  }

  /**
   * @param v
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */
  @inline
  override final def prevValue(v: Int): Int = {
    if (isEmpty) sys.error("empty domain")
    else {
      var cv = v
      while (cv >= min) {
        if (hasValue(cv)) {
          return cv
        }
        cv -= 1
      }
      v + 1
    }
  }

  @inline
  override final def assign(value: Int): Unit = {
    if (!hasValue(value)) {
      _size.value = 0
      throw Inconsistency
    }
    else {
      val v = values(0)
      val index = indexes(value - minValue)
      indexes(value - minValue) = 0
      values(0) = value
      indexes(v - minValue) = index
      values(index) = v
      _min.value = value
      _max.value = value
      _size.value = 1
    }
  }

  @inline
  override final def updateMin(minv: Int): Unit = {
    if (isEmpty) sys.error("empty domain")
    else {
      val minVal = min // Lazy update
      if (minv > minVal) {
        val maxVal = max // Lazy update
        if (minv > maxVal) {
          _size.value = 0
          throw Inconsistency
        }
        else if (minv == maxVal) {
          assign(minv)
        }
        else {
          var cv = minVal
          while (cv < minv) {
            removeValue(cv)
            cv += 1
          }
          if (hasValue(minv))
            _min.value = minv
        }
      }
    }
  }

  @inline
  override final def updateMax(maxv: Int): Unit = {
    if (isEmpty) sys.error("empty domain")
    else {
      val maxVal = max // Lazy update
      if (maxv < maxVal) {
        val minVal = min // Lazy update
        if (maxv < minVal) {
          _size.value = 0
          throw Inconsistency
        }
        else if (maxv == minVal) {
          assign(maxv)
        }
        else {
          var cv = maxVal
          while (cv > maxv) {
            removeValue(cv)
            cv -= 1
          }
          if (hasValue(maxv))
            _max.value = maxv
        }
      }
    }
  }

  override def toString: String = if (isEmpty) "phi" else "{" + this.mkString(", ") + "}"
}
