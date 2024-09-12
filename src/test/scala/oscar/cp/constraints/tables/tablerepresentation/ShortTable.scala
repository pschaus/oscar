/** *****************************************************************************
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
 * *****************************************************************************/

package oscar.cp.constraints.tables.tablerepresentation

import oscar.cp.constraints.diagrams.diagramrepresentation.Diagram
import oscar.cp.constraints.tables.{BasicSmartElement, Equal, Star}
import oscar.cp.core.variables.CPIntVar

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Class representing a short table
 *
 * @param table
 * @param isSorted
 */
class ShortTable(table: Array[Array[Int]], star: Int, isSorted: Boolean = false) extends Table {

  // ordering
  private val shortTableOrdering = new Ordering[Array[Int]] {
    def compare(x: Array[Int], y: Array[Int]): Int = {
      var i = 0
      while (i < x.length) {
        val a = x(i)
        val b = y(i)
        if (a == star && b != star)
          return -1
        if (a != star && b == star)
          return 1
        if (a > b)
          return 1
        if (a < b)
          return -1
        i += 1
      }
      0
    }

    def isEquals(x: Array[Int], y: Array[Int]): Boolean = {
      var i = 0
      while (i < x.length) {
        val a = x(i)
        val b = y(i)
        if (a != b)
          return false
        i += 1
      }
      true
    }

    // is y included in x
    def isIncluded(x: Array[Int], y: Array[Int]): Boolean = {
      var i = 0
      while (i < x.length) {
        val a = x(i)
        val b = y(i)
        if (a != star && a != b)
          return false
        i += 1
      }
      true
    }

    def isOverlapping(x: Array[Int], y: Array[Int]): Boolean = {
      var i = 0
      while (i < x.length) {
        val a = x(i)
        val b = y(i)
        if (a != star && a != b && b != star)
          return false
        i += 1
      }
      true
    }
  }

  def length: Int = table.length

  def arity: Int = table(0).length

  def isTupleIncluded(tuple: Array[Int]): Boolean = {
    var i = length
    while (i > 0) {
      i -= 1
      if (shortTableOrdering.isIncluded(table(i), tuple))
        return true
    }
    false
  }

  protected def isTupleValid(x: Array[CPIntVar], tupleIndex: Int): Boolean = {
    val tuple = table(tupleIndex)
    var i = x.length
    while (i > 0) {
      i -= 1
      if (tuple(i) != star && !x(i).hasValue(tuple(i)))
        return false
    }
    true
  }

  def getHashSetTuple(x: Array[CPIntVar], index: Int): mutable.Set[String] = {
    import scala.collection.mutable.Set
    val tuple = table(index)
    var t = Set("")
    for (i <- tuple.indices) {
      t = t.flatMap { s =>
        if (tuple(i) == star) {
          val temp = new Array[Int](x(i).size)
          x(i).fillArray(temp)
          if (i > 0)
            temp.map(s + "," + _)
          else
            temp.map(s + _)
        } else {
          if (i > 0)
            Array(s + "," + tuple(i))
          else
            Array(s + tuple(i))
        }
      }
    }
    t
  }

  def getHashSet(x: Array[CPIntVar]): mutable.Set[String] = {
    import scala.collection.mutable.Set
    val set = Set[String]()
    for (index <- table.indices) {
      val t = getHashSetTuple(x, index)
      set ++= t
    }
    set
  }

  def filterTable(x: Array[CPIntVar]): Table = {
    val nbTuple = table.length
    val buffer = new Array[Array[Int]](nbTuple)
    var bufferSize = 0
    var tupleIndex = 0
    while (tupleIndex < nbTuple) {
      if (isTupleValid(x, tupleIndex)) {
        buffer(bufferSize) = table(tupleIndex)
        bufferSize += 1
      }
      tupleIndex += 1
    }
    val resArray = new Array[Array[Int]](bufferSize)
    System.arraycopy(buffer, 0, resArray, 0, bufferSize)
    Table(resArray, star, isSorted)
  }

  def removeDuplicate: Table = {
    if (table.length <= 1) this
    else {
      val buff = new ArrayBuffer[Array[Int]]()
      if (isSorted) {
        var prev = table(0)
        buff += prev
        for (i <- 1 until table.length) {
          if (!shortTableOrdering.isEquals(table(i), prev)) {
            prev = table(i)
            buff += prev
          }
        }
      } else {
        for (i <- table.indices) {
          var add = true
          var j = 0
          while (j < buff.length && add) {
            if (shortTableOrdering.isEquals(table(i), buff(j)))
              add = false
            j += 1
          }
          if (add)
            buff += table(i)
        }
      }
      Table(buff.toArray, star, isSorted)
    }
  }


  def sortTable: Table = {
    if (isSorted) {
      this
    } else {
      val sortedTable = table.clone()
      scala.util.Sorting.quickSort(sortedTable)(shortTableOrdering)
      Table(sortedTable, star, true)
    }
  }

  def applyOffset(offsets: Array[Int]): Table = {
    Table(Array.tabulate(table.length)(i => Array.tabulate(offsets.length)(j => if (table(i)(j) == star) star else table(i)(j) - offsets(j))), star, isSorted)
  }

  def decompressToGroundTable(x: Array[CPIntVar]): GroundTable = {
    val buff = new ArrayBuffer[Array[Int]]()
    val size = x.length
    val queueTuple = new mutable.Queue[Array[Int]]()
    val queueIndex = new mutable.Queue[Int]()
    var idx = table.length
    while (idx > 0) {
      idx -= 1
      var tuple = table(idx)
      var index = 0
      while (tuple != null) {
        if (index == size) {
          buff += tuple
          if (queueTuple.isEmpty)
            tuple = null
          else {
            tuple = queueTuple.dequeue()
            index = queueIndex.dequeue()
          }
        } else {
          if (tuple(index) == star) {
            for (v <- x(index).iterator) {
              val newTuple = tuple.clone()
              newTuple(index) = v
              queueTuple.enqueue(newTuple)
              queueIndex.enqueue(index + 1)
            }
            tuple = queueTuple.dequeue()
            index = queueIndex.dequeue()
          } else {
            index += 1
          }
        }
      }
    }
    val tab = Table(buff.toArray, false)
    tab.sortTable.removeDuplicate.asInstanceOf[GroundTable]
  }

  def mapToBasicSmartTable(x: Array[CPIntVar]): BasicSmartTable = {
    mapToBasicSmartTable_simple()
  }

  private def mapToBasicSmartTable_simple(): BasicSmartTable = {
    val tableBs = Array.fill(table.length)(new Array[BasicSmartElement](arity))
    for (i <- table.indices; j <- 0 until arity) {
      if (table(i)(j) == star)
        tableBs(i)(j) = Star()
      else
        tableBs(i)(j) = Equal(table(i)(j))
    }
    Table(tableBs, isSorted)
  }

  def compressToBasicSmartTable(x: Array[CPIntVar]): BasicSmartTable = {
    println("Warning: no direct method to compress short tables into basic smart tables, decompression first")
    val gtable = this.decompressToGroundTable(x)
    gtable.compressToBasicSmartTable(x)
  }

  def transformTo_MDD: Diagram = {
    assert(length != 0,"The table should contain at least one tuple to be transformed into a diagram")
    val bstable = mapToBasicSmartTable_simple()
    bstable.transformTo_MDD
  }

  def transformTo_sMDD: Diagram = {
    assert(length != 0,"The table should contain at least one tuple to be transformed into a diagram")
    val bstable = mapToBasicSmartTable_simple()
    bstable.transformTo_sMDD
  }

  override def toString(): String = {
    val str = new StringBuilder
    for (i <- table.indices) {
      str ++= table(i).mkString(",")
      str += '\n'
    }
    str.toString
  }
}
