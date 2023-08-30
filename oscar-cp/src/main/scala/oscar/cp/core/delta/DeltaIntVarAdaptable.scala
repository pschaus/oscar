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
  *******************************************************************************/

package oscar.cp.core.delta

import oscar.algo.reversible.TrailEntry
import oscar.cp.core.variables.CPIntVar


/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class DeltaIntVarAdaptableTrailEntry(delta: DeltaIntVarAdaptable, min: Int, max: Int, size: Int) extends TrailEntry {
  final override def restore(): Unit = delta.restore(min, max, size)
}

final class DeltaIntVarAdaptable(x: CPIntVar, final override val id: Int) extends DeltaIntVar {

  private[this] val store = x.store
  private[this] var _oldMin: Int = x.min
  private[this] var _oldMax: Int = x.max
  private[this] var _oldSize: Int = x.size

  // Used to trail changes in the delta
  private[this] var lastMagic: Long = -1L
  
  @inline final override def oldMin: Int = _oldMin
  @inline final override def oldMax: Int = _oldMax
  @inline final override def oldSize: Int = _oldSize
  @inline final override def variable: CPIntVar = x
  @inline final override def changed: Boolean = x.size != _oldSize
  @inline final override def size: Int = _oldSize - x.size
  @inline final override def values: Iterator[Int] = x.delta(_oldMin, _oldMax, _oldSize)
  @inline final override def fillArray(values: Array[Int]): Int = x.fillDeltaArray(_oldMin, _oldMax, _oldSize, values)
  @inline final override def minChanged: Boolean = x.min != _oldMin
  @inline final override def maxChanged: Boolean = x.max != _oldMax

  @inline private def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      store.trail(new DeltaIntVarAdaptableTrailEntry(this, _oldMin, _oldMax, _oldSize))
    }
  }

  @inline final def restore(oldMin: Int, oldMax: Int, oldSize: Int): Unit = {
    _oldMin = oldMin
    _oldMax = oldMax
    _oldSize = oldSize
  }
  
  final override def update(): Unit = {
    val xs = x.size
    if (xs != _oldSize) trail()
    _oldMin = x.min
    _oldMax = x.max
    _oldSize = xs
  }


}