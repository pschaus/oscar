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

package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.algo.reversible.ReversibleInt
import oscar.cp.constraints.tables.TableAlgo

/**
 * AC Element Constraint on a 2D array
 * @author Pierre Schaus pschaus@gmail.com
 */
object ElementCst2D {

  private var prevT: Array[Array[Int]] = null

  private var prevData: Array[Array[Int]] = null

  def apply(T: Array[Array[Int]], x: CPIntVar, y: CPIntVar, z: CPIntVar): Constraint = {
    apply(T,x,y,z,TableAlgo.CompactTable)
  }

  def apply(T: Array[Array[Int]], x: CPIntVar, y: CPIntVar, z: CPIntVar, algo: TableAlgo.Value): Constraint = {
    if (prevT != T) {
      prevT = T;
      prevData =
      (for (i <- 0 until T.size; j <- 0 until T(i).size) yield {
        Array(i, j, T(i)(j))
      }).toArray
    }
    oscar.cp.constraints.tables.table(Array(x,y,z), prevData, algo)
  }
}

/**
 * BC Element Constraint on a 2D array
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
final class ElementCst2D(T: Array[Array[Int]], x: CPIntVar, y: CPIntVar, z: CPIntVar) extends Constraint(x.store, "ElementCst2D") {

  override def associatedVars(): Iterable[CPVar] = Array(x, y, z)

  require(T.length > 0)
  require(T(0).length > 0)

  private[this] val nRows = T.length
  private[this] val nCols = T(0).length
  private[this] val nTuples = nRows * nCols
  private[this] val sortedTuples = (for (i <- 0 until nRows; j <- 0 until nCols) yield (T(i)(j), i, j)).sortBy(t => t).map(t => Array(t._1, t._2, t._3)).toArray.transpose
  private[this] val nbColSupports = Array.fill(nRows)(new ReversibleInt(s, 0))
  private[this] val nbRowSupports = Array.fill(nCols)(new ReversibleInt(s, 0))
   
  private[this] val zValues = sortedTuples(0)
  private[this] val xValues = sortedTuples(1)
  private[this] val yValues = sortedTuples(2)

  private[this] val lowRev = new ReversibleInt(s, 0)
  private[this] val upRev = new ReversibleInt(s, nTuples - 1)

  override def setup(l: CPPropagStrength): Unit = {
    x.updateMin(0)
    x.updateMax(nRows - 1)
    y.updateMin(0)
    y.updateMax(nCols - 1)

    init()
    x.callPropagateWhenDomainChanges(this)
    y.callPropagateWhenDomainChanges(this)
    z.callPropagateWhenBoundsChange(this)
    propagate()
  }

  @inline private def init(): Unit = {
    var i = nTuples
    while (i > 0) {
      i -= 1
      nbColSupports(xValues(i)).incr()
      nbRowSupports(yValues(i)).incr()
    }
  }

  // entry i disappear
  @inline private def update(i: Int): Unit = {
    if (nbColSupports(xValues(i)).decr() == 0) {
      x.removeValue(xValues(i))
    }
    if (nbRowSupports(yValues(i)).decr() == 0) {
      y.removeValue(yValues(i))
    }
  }

  override def propagate(): Unit = {

    // Cache
    var low = lowRev.value
    var up = upRev.value

    val zMin = z.min
    val zMax = z.max
    
    while (zValues(low) < zMin || !x.hasValue(xValues(low)) || !y.hasValue(yValues(low))) {
      if (up == low) throw Inconsistency
      update(low)
      low += 1
    }
    while (zValues(up) > zMax || !x.hasValue(xValues(up)) || !y.hasValue(yValues(up))) {
      if (up == low) throw Inconsistency
      update(up)
      up -= 1
    }

    z.updateMin(zValues(low))
    z.updateMax(zValues(up))

    // Trail
    lowRev.value = low
    upRev.value = up
  }
}
	

