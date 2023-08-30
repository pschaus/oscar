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
 * An (initially empty) reversible sparse set of maximal size maxSize that can contain values 0..n-1.
 * This sparse set also allows the sparse array to be shared with other sparse sets.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 */
class ReversibleSharedSparseSet(context: ReversibleContext, n: Int, maxSize: Int, sharedSparse: Array[Int]) {
  private[this] val sparse = sharedSparse
  private[this] val dense = Array.fill(maxSize)(-1)
  private[this] var _size = new ReversibleInt(context, 0)

  def this(context: ReversibleContext, n: Int) = {
    this(context, n, n, Array.fill(n)(-1))
  }
  
  def this(context: ReversibleContext, n: Int, maxSize: Int) = {
    this(context, n, maxSize, Array.fill(n)(-1))
  }
  
  /**
   * Return the ith value in the set.
   * @param i the index of the value.
   * @return the value.
   */
  @inline final def apply(i: Int): Int = dense(i)

  /**
   * Check whether the set is empty or not.
   * @return true if the set is empty, false otherwise.
   */
  @inline final def isEmpty: Boolean = _size.value == 0

  /**
   * Check whether value k is in the set.
   * @param k the value.
   * @return true if k is in the set, false otherwise.
   */
  @inline final def hasValue(k: Int): Boolean = {
    val a = sparse(k)
    a < _size.value && a >= 0 && dense(a) == k
  }

  /**
   * Insert a value in the set.
   * @param k the value to insert.
   */
  @inline final def insert(k: Int): Unit = {
    val a = sparse(k)
    val b = _size.value
    if (a >= b || a < 0 || dense(a) != k) {
      sparse(k) = b
      dense(b) = k
      _size.value += 1
    }
  }
  
  /**
   * Remove a value THAT IS in the set. 
   * If the user is not sure if the value is in the set, call this.hasValue(k) before.
   * @param k the value to remove.
   */
  @inline final def remove(k: Int): Unit = {
    val sK = sparse(k)
    val sL = _size.value - 1
    dense(sK) = dense(sL)
    sparse(dense(sK)) = sK
    sparse(k) = sL
    dense(sL) = k
    _size -= 1
  }

  /**
   * Clear the set.
   */
  @inline final def clear(): Unit = _size.value = 0

  /**
   * Get the size of the set.
   * @return the size of the set
   */
  @inline final def size: Int = _size.value

  /**
   * Restore a value that was previously in the set and that is not in it anymore.
   * @param k the value to restore.
   */
  @inline final def restore(k: Int): Unit = {
    val sK = sparse(k)
    val sL = _size.value
    dense(sK) = dense(sL)
    sparse(dense(sK)) = sK
    sparse(k) = sL
    dense(sL) = k
    _size += 1
  }
  
}
