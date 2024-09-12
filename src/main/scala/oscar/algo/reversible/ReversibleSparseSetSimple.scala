/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.algo.reversible

/**
 * Initializes a set with all values min..max in it
 * @param s
 * @param minValue
 * @param maxValue >= minValue
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
class ReversibleSparseSetSimple(s: ReversibleContext, val minValue: Int, val maxValue: Int) extends Iterable[Int] {

  private[this] val _size = new ReversibleInt(s, maxValue - minValue + 1)

  private[this] val values = Array.tabulate(size)(i => minValue + i)

  override def size: Int = _size.getValue()

  override def isEmpty = _size.getValue() == 0

  /**
   * remove all elements in the set
   */
  def makeEmpty(): Unit = _size.setValue(0)

  /**
   * @return a safe iterator (ok to remove values while iterating)
   */
  def iterator: Iterator[Int] = {
    this.toArray.iterator
  }

  /**
   * @return an array representation of values present in the set
   */
  def toArray: Array[Int] = {
    val res = Array.ofDim[Int](size)
    fillArray(res)
    res
  }

  /**
   * @return set the first values of dest to the ones
   *         of the set and return the size of the set
   */
  def fillArray(dest: Array[Int]): Int = {
    System.arraycopy(values, 0, dest, 0, size)
    size
  }

  /**
   * You can safely remove the current value while iterating using this function.
   * If you remove any other value, you may encounter the same value multiple times.
   */
  @inline final override def foreach[U](f: Int => U): Unit = {
    var i = size
    while (i != 0) {
      i -= 1
      f(values(i))
    }
  }

  /**
   * Remove from the set the element not satisfing the given predicate
   * @param fun predicate
   * @return this
   */
  @inline override def filter(fun:Int => Boolean) = {
    var _s = size
    var k = _s
    while (k > 0) {
      k -= 1
      val value = values(k)
      if (!fun(value)){
        _s -= 1
        values(k) = values(_s)
        values(_s) = value
      }
    }
    _size.setValue(_s)
    this
  }
}
