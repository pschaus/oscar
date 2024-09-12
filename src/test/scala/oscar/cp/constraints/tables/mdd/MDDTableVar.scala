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

package oscar.cp.constraints.tables.mdd

import oscar.cp.core.variables.CPIntVar
import oscar.algo.reversible.{ReversibleContext, ReversibleSharedSparseSet}

/**
 * Represent a variable of a MDD (i.e. a layer). This class makes the link between ReversibleMDD and CPIntVar.
 *
 * @param context the context of the variable.
 * @param linkedVar the associated integer variable.
 * @param indexVar the index of the variable in the tuples of the table.
 * @param nbDifferentValues the number of different values taken by the variable in the table.
 * @param mdd the associated MDD.
 * @param maxEdgesPerValue the maximum number of edges per value.
 * @param sharedEdges the sparse array shared by all edges of the MDD.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 */
class MDDTableVar(context: ReversibleContext, linkedVar: CPIntVar, indexVar: Int, nbDifferentValues: Int, mdd: ReversibleMDD, maxEdgesPerValue: Array[Int], sharedEdges: Array[Int]) {
  private[this] val activeValues = new ReversibleSharedSparseSet(context, nbDifferentValues, nbDifferentValues)
  private[this] val edgesPerValue = Array.tabulate(nbDifferentValues)(i => new ReversibleSharedSparseSet(context, mdd.nbEdges, maxEdgesPerValue(i), sharedEdges))

  /* All values are valid at first */
  (0 until nbDifferentValues).foreach(value => activeValues.insert(value))

  @inline final def hasChanged: Boolean = activeValues.size != linkedVar.size

  @inline final def hasValue(value: Int): Boolean = activeValues.hasValue(value)

  /**
   * Fill an array of values still considered in the MDD but not present anymore in the domain of the variable.
   * @param delta the array to fill.
   * @return the number of values added in delta.
   */
  @inline final def fillDeltaArray(delta: Array[Int]): Int = {
    var size = 0
    var i = 0
    while (i < activeValues.size) {
      val value = activeValues(i)
      if (!linkedVar.hasValue(mdd.valueForIndex(indexVar, value))) {
        delta(size) = value
        size += 1
      }
      i += 1
    }
    size
  }

  /**
   * Add an edge as a support for a given value.
   * @param value the value.
   * @param edge the associated edge.
   */
  @inline final def addEdge(value: Int, edge: Int): Unit = edgesPerValue(value).insert(edge)

  /**
   * Delete values from D(x) that are not in the MDD.
   * @param dom a temporary big enough to store D(x).
   * @return the outcome i.e. Failure or Success.
   */
  @inline final def deleteValuesNotInMDD(dom: Array[Int]): Unit = {
    val size = linkedVar.fillArray(dom)
    var i = 0
    var value = 0
    while (i < size) {
      value = dom(i)
      if (!mdd.containsValue(indexVar, value)) {
        linkedVar.removeValue(value)
      }
      i += 1
    }
  }

  /**
   * Compute the total number of edges associated to the values in delta set.
   * @param deltaSize the number of values in the delta set.
   * @param delta the values of the delta set.
   * @return the total number of edges associated to the values in the delta set.
   */
  @inline final def getNbOfEdgesToRemove(deltaSize: Int, delta: Array[Int]): Int = {
    var i = 0
    var total = 0
    while (i < deltaSize) {
      val value = delta(i)
      total += edgesPerValue(value).size
      i += 1
    }
    total
  }

  /**
   * Fill an array of the edges associated to the values in a delta set.
   * @param deltaSize the number of values in the delta set.
   * @param delta the values of the delta set.
   * @param edgesToRemove the array to fill with the edges.
   * @return the number of edges added in the array.
   */
  @inline final def getEdgesToRemove(deltaSize: Int, delta: Array[Int], edgesToRemove: Array[Int]): Int = {
    var size = 0
    var i = 0
    while (i < deltaSize) {
      val value = delta(i)
      var j = 0
      while (j < edgesPerValue(value).size) {
        val edge = edgesPerValue(value)(j)
        edgesToRemove(size) = edge
        size += 1
        j += 1
      }
      i += 1
    }
    size
  }

  /**
   * Fill an array of the edges that are still consistent in the MDD.
   * @param edgesToKeep the array to fill.
   * @return the number of edges added in the array.
   */
  @inline final def getEdgesToKeep(edgesToKeep: Array[Int]): Int = {
    var size = 0
    for (value <- linkedVar.iterator) {
      val indexOfValue = mdd.indexOfValue(indexVar, value)
      var i = 0
      while (i < edgesPerValue(indexOfValue).size) {
        val edge = edgesPerValue(indexOfValue)(i)
        edgesToKeep(size) = edge
        size += 1
        i += 1
      }
    }
    size
  }

  /**
   * Invalidate all values in a delta set.
   * @param deltaSize the set of values.
   * @param delta the number of values in the delta set.
   */
  @inline final def deleteDeltaValues(deltaSize: Int, delta: Array[Int]): Unit = {
    var i = 0
    while (i < deltaSize) {
      val value = delta(i)
      if (edgesPerValue(value).size > 0) {
        activeValues.remove(value)
        edgesPerValue(value).clear()
      }
      i += 1
    }
  }

  /**
   * Remove an edge associated to a value.
   * @param edge the edge to remove
   * @param value the associated value
   * @return the outcome i.e. Failure or Success.
   */
  @inline final def removeEdgeForValue(edge: Int, value: Int): Unit = {
    if (edgesPerValue(value).size == 1) {
      activeValues.remove(value)
      linkedVar.removeValue(mdd.valueForIndex(indexVar, value))
    }
    edgesPerValue(value).remove(edge)
  }

  /**
   * Clear the supports for every active value
   */
  @inline final def clearSupports(): Unit = {
    var i = 0
    while (i < activeValues.size) {
      val value = activeValues(i)
      edgesPerValue(value).clear()
      i += 1
    }
  }

  /**
   * Restore an edge in its associated edgesPerValue set.
   * @param edge the edge to add.
   * @param value the associated value.
   */
  @inline final def restoreEdge(edge: Int, value: Int): Unit = edgesPerValue(value).restore(edge)

  /**
   * Remove values not supported by any edge.
   * @return the outcome i.e. Failure or Success.
   */
  @inline final def removeUnsupportedValues(): Unit = {
    var i = activeValues.size - 1
    while (i >= 0) {
      val value = activeValues(i)
      if (edgesPerValue(value).size == 0) {
        activeValues.remove(value)
        linkedVar.removeValue(mdd.valueForIndex(indexVar, value))
      }
      i -= 1
    }
  }
}