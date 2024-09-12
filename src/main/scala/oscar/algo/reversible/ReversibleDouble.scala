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
package oscar.algo.reversible;

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class ReversibleDoubleTrailEntry(reversible: ReversibleDouble, value: Double) extends TrailEntry {
  @inline override final def restore(): Unit = reversible.restore(value)
}

class ReversibleDouble(node: ReversibleContext, value: Double) extends ReversiblePointer[Double](node, value) {

  @inline final override def trailEntry = new ReversibleDoubleTrailEntry(this, pointer)
  
  /** Increments the reversible integer by i */
  def +=(v: Double): Double = {
    trail()
    pointer += v
    pointer
  }

  /** Decrements the reversible integer by i */
  def -=(v: Double): Double = {
    trail()
    pointer -= v
    pointer
  }
}

object ReversibleDouble {
  def apply(value: Double)(implicit context: ReversibleContext) = new ReversibleDouble(context, value)
}
