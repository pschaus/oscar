/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.algo.reversible

final class ReversibleSparseSetManual(context: ReversibleContext, n: Int) extends TrailEntry {

  // Inner trailing queue
  private[this] val trail = new Array[Int](n + 1)
  private[this] var trailSize = 0

  // Id of the last context
  private[this] var lastMagic: Long = -1L

  private[this] val _positions = Array.tabulate(n)(i => i)
  private[this] val _values = _positions.clone()
  private[this] var _size = 0

  @inline def exposeArray: Array[Int] = _values

  @inline def size: Int = _size

  @inline def isEmpty: Boolean = _size == 0

  def add(value: Int): Unit = {
    val p1 = _positions(value)
    if (p1 >= _size) {
      val v2 = _values(_size)
      _positions(value) = _size
      _positions(v2) = p1
      _values(p1) = v2
      _values(_size) = value
      _size += 1
    }
  }

  def remove(value: Int): Unit = {
    val p1 = _positions(value)
    if (p1 < _size) {
      _size -= 1
      val v2 = _values(_size)
      _positions(value) = _size
      _positions(v2) = p1
      _values(p1) = v2
      _values(_size) = value
    }
  }

  def contains(value: Int): Boolean = {
    val pos = _positions(value)
    pos < _size
  }

  def trail(): Unit = {
    val contextMagic = context.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      trail(trailSize) = _size
      trailSize += 1
      context.trail(this)
    }
  }

  final override def restore(): Unit = {
    trailSize -= 1
    _size = trail(trailSize)
  }
}
