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

import oscar.cp.constraints.diagrams.diagramrepresentation.{Diagram, GroundDiagram}
import oscar.cp.constraints.tables.{BasicSmartElement, Equal}
import oscar.cp.core.variables.CPIntVar

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Class representing a ground table
 * @param table
 * @param isSorted
 */
class GroundTable(table: Array[Array[Int]], isSorted: Boolean = false) extends Table {

  // ordering between two tuples
  private val tableOrdering = new Ordering[Array[Int]] {
    def compare(x: Array[Int], y: Array[Int]): Int = {
      var i = 0
      while (i < x.length) {
        val a = x(i)
        val b = y(i)
        if (a > b)
          return 1
        if (a < b)
          return -1
        i += 1
      }
      0
    }

    def isEquals(x: Array[Int], y: Array[Int]): Boolean = {
      compare(x, y) == 0
    }
  }

  // get back the table
  def getTable = table

  def length: Int = table.length

  def arity: Int = table(0).length

  // Check if a tuple is included in a table
  def isTupleIncluded(tuple: Array[Int]): Boolean = {
    if (isSorted) {
      var lb = 0
      var ub = length - 1
      while (lb != ub) {
        val mid = (lb + ub) / 2
        val compare = tableOrdering.compare(table(mid), tuple)
        if (compare == 1) {
          ub = mid
        } else if (compare == -1) {
          lb = mid + 1
        } else {
          return true
        }
      }
      tableOrdering.isEquals(table(lb), tuple)
    } else {
      var i = length
      while (i > 0) {
        i -= 1
        if (tableOrdering.isEquals(table(i), tuple))
          return true
      }
      false
    }
  }


  protected def isTupleValid(x: Array[CPIntVar], tupleIndex: Int): Boolean = {
    isGroundTupleValid(x, table(tupleIndex))
  }

  def getHashSetTuple(x: Array[CPIntVar], index: Int): mutable.Set[String] = {
    mutable.Set(getHashGround(table(index)))
  }

  def getHashSet(x: Array[CPIntVar]): mutable.Set[String] = {
    import scala.collection.mutable.Set
    val set = Set[String]()
    for (tuple <- table)
      set += getHashGround(tuple)
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
    val newArray = new Array[Array[Int]](bufferSize)
    System.arraycopy(buffer, 0, newArray, 0, bufferSize)
    Table(newArray, isSorted)
  }

  def removeDuplicate: Table = {
    if (table.length <= 1) this
    else {
      val buff = new ArrayBuffer[Array[Int]]()
      if (isSorted) {
        var prev = table(0)
        buff += prev
        for (i <- 1 until table.length) {
          if (!tableOrdering.isEquals(table(i), prev)) {
            prev = table(i)
            buff += prev
          }
        }
      } else {
        for (i <- table.indices) {
          var add = true
          var j = 0
          while (j < buff.length && add) {
            if (tableOrdering.isEquals(table(i), buff(j)))
              add = false
            j += 1
          }
          if (add)
            buff += table(i)
        }
      }
      Table(buff.toArray, isSorted)
    }
  }

  def sortTable: Table = {
    if (isSorted) {
      this
    } else {
      val sortedTable = table.clone()
      scala.util.Sorting.quickSort(sortedTable)(tableOrdering)
      Table(sortedTable, true)
    }
  }

  def applyOffset(offsets: Array[Int]): Table = {
    Table(Array.tabulate(table.length)(i => Array.tabulate(offsets.length)(j => table(i)(j) - offsets(j))), isSorted)
  }

  def decompressToGroundTable(x: Array[CPIntVar]): GroundTable = {
    this
  }

  def mapToBasicSmartTable(x: Array[CPIntVar]): BasicSmartTable = {
    val tableBs = Array.fill(table.length)(new Array[BasicSmartElement](x.length))
    for (i <- table.indices; j <- x.indices) {
      tableBs(i)(j) = Equal(table(i)(j))
    }
    Table(tableBs, isSorted)
  }

  def compressToBasicSmartTable(x: Array[CPIntVar]): BasicSmartTable = {
    val a = this.sortTable
    val b = a.removeDuplicate
    val c = b.mapToBasicSmartTable(x)
    val d = c.compressItSelf(x, false)
    d
  }


  def transformTo_MDD: Diagram = {
    assert(length != 0,"The table should contain at least one tuple to be transformed into a diagram")
    if (isSorted) {
      val transaction = Array.fill(table(0).length)(new ArrayBuffer[(Int, Int, Int)]())
      val nodes = Array.fill(table(0).length + 1)(0)
      for (i <- table(0).indices) {
        transaction(i) += ((nodes(i), table(0)(i), nodes(i + 1)))
      }
      for (i <- 1 until table.length) {
        var change = false
        for (j <- 0 until table(0).length - 1) {
          if (change)
            nodes(j + 1) += 1
          else if (table(i)(j) != table(i - 1)(j)) {
            change = true
            nodes(j + 1) += 1
          }
          if (change)
            transaction(j) += ((nodes(j), table(i)(j), nodes(j + 1)))
        }
        transaction(table(0).length - 1) += ((nodes(table(0).length - 1), table(i)(table(0).length - 1), nodes(table(0).length)))
      }
      val trans = transaction.map(_.toArray)
      new GroundDiagram(trans, nodes.map(_ + 1)).reduceTree_MDD
    } else {
      this.sortTable.removeDuplicate.transformTo_MDD
    }
  }

  def transformTo_sMDD: Diagram = {
    assert(length != 0,"The table should contain at least one tuple to be transformed into a diagram")
    if (table(0).length <= 2) {
      this.transformTo_MDD
    } else {
      if (isSorted) {
        val part1 = table(0).length / 2
        val part2 = table(0).length - part1 - 1
        val arity = table(0).length
        val arity1 = part1
        val arity2 = part2

        var tablePart1 = table.map(_.slice(0, arity1))
        var tablePart2 = table.map(_.slice(arity1 + 1, arity).reverse)
        scala.util.Sorting.quickSort(tablePart1)(tableOrdering)
        scala.util.Sorting.quickSort(tablePart2)(tableOrdering)

        val buff1 = new ArrayBuffer[Array[Int]]()
        var prev = tablePart1(0)
        buff1 += prev
        for (i <- 1 until tablePart1.length) {
          if (!tableOrdering.isEquals(tablePart1(i), prev)) {
            prev = tablePart1(i)
            buff1 += prev
          }
        }
        tablePart1 = buff1.toArray

        val buff2 = new ArrayBuffer[Array[Int]]()
        prev = tablePart2(0)
        buff2 += prev
        for (i <- 1 until tablePart2.length) {
          if (!tableOrdering.isEquals(tablePart2(i), prev)) {
            prev = tablePart2(i)
            buff2 += prev
          }
        }
        tablePart2 = buff2.toArray

        val transaction = Array.fill(arity)(new ArrayBuffer[(Int, Int, Int)]())

        val nodes = Array.fill(arity + 1)(0)
        val hashMap1 = new mutable.HashMap[String, Int]()
        var str = ""
        for (i <- tablePart1(0).indices) {
          transaction(i) += ((nodes(i), tablePart1(0)(i), nodes(i + 1)))
          str += tablePart1(0)(i)
          if (i == arity1 - 1)
            hashMap1 += ((str, nodes(arity1)))
          else
            str += ","
        }
        for (i <- 1 until tablePart1.length) {
          str = ""
          var change = false
          for (j <- 0 until arity1) {
            if (tablePart1(i)(j) != tablePart1(i - 1)(j))
              change = true
            if (change) {
              nodes(j + 1) += 1
              transaction(j) += ((nodes(j), tablePart1(i)(j), nodes(j + 1)))
            }
            str += tablePart1(i)(j)
            if (j == arity1 - 1)
              hashMap1 += ((str, nodes(arity1)))
            else
              str += ","
          }
        }

        val hashMap2 = new mutable.HashMap[String, Int]()
        str = ""
        val offset = part1 + 1
        for (i <- tablePart2(0).indices) {
          val oi = offset + (arity2 - 1) - i
          transaction(oi) += ((nodes(oi), tablePart2(0)(i), nodes(oi + 1)))
          str = tablePart2(0)(i) + str
          if (i == arity2 - 1)
            hashMap2 += ((str, nodes(offset)))
          else
            str = "," + str
        }
        for (i <- 1 until tablePart2.length) {
          str = ""
          var change = false
          for (j <- 0 until arity2) {
            val oj = offset + (arity2 - 1) - j
            if (tablePart2(i)(j) != tablePart2(i - 1)(j))
              change = true
            if (change) {
              nodes(oj) += 1
              transaction(oj) += ((nodes(oj), tablePart2(i)(j), nodes(oj + 1)))
            }
            str = tablePart2(i)(j) + str
            if (j == arity2 - 1)
              hashMap2 += ((str, nodes(oj)))
            else
              str = "," + str
          }
        }

        for (tuple <- table) {
          str = tuple(0).toString
          for (i <- 1 until part1)
            str += "," + tuple(i)
          val node1 = hashMap1(str)
          str = tuple(part1 + 1).toString
          for (i <- part1 + 2 until arity) {
            str += "," + tuple(i)
          }
          val node2 = hashMap2(str)
          transaction(arity1) += ((node1, tuple(part1), node2))
        }
        val trans = transaction.map(_.toArray)
        val mdd = new GroundDiagram(trans, nodes.map(_ + 1))
        val mdd2 = mdd.reduceTree_sMDD
        mdd2
      } else {
        this.sortTable.removeDuplicate.asInstanceOf[GroundTable].transformTo_sMDD
      }
    }
  }

  /**
   * Compute the hamming distance between two tuples
   * @param tuple1
   * @param tuple2
   * @return
   */
  private def hammingDistance(tuple1: Array[Int], tuple2: Array[Int]) = {
    tuple1.indices.count(i => tuple1(i) != tuple2(i))
  }

  /**
   * Compute the minimum hamming distance between tuples within a table
   * @return
   */
  def minHammingDistance() = {
    var min = table(0).length
    for (i <- 0 until length; j <- i + 1 until length) {
      val newmin = hammingDistance(table(i), table(j))
      if (newmin < min)
        min = newmin
    }
    min
  }

  /**
   * Check if a table could be compressible
   * @return
   */
  def isCompressible() = {
    var isIt = false
    var i = length - 1
    while (i > 0 && !isIt) {
      i -= 1

      var j = length - 1
      while (j > i && !isIt) {
        var two = -2

        var k = table(i).length
        while (k > 0 && two != 0) {
          k -= 1
          if (table(i)(k) == table(j)(k)) {
            two += 1
          }
        }

        if (two != 0)
          isIt = true

        j -= 1
      }
    }
    isIt
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

