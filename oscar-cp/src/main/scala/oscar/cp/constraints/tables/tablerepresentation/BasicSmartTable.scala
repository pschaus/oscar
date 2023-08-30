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

import oscar.algo.reversible.BitSetOp
import oscar.cp.constraints.diagrams.diagramrepresentation.{BasicSmartDiagram, Diagram}
import oscar.cp.constraints.tables._
import oscar.cp.core.variables.CPIntVar

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Class representing a basic smart table
 * @param table
 * @param isSorted
 */
class BasicSmartTable(table: Array[Array[BasicSmartElement]], isSorted: Boolean = false) extends Table {

  def length: Int = table.length

  def arity: Int = table(0).length

  def getTable = table

  def countBS = {
    var count = 0
    for (tuple <- table) {
      if (!tuple.forall(bs => bs.isInstanceOf[Equal]))
        count += 1
    }
    count
  }

  private val basicSmartTableOrdering2 = new Ordering[Array[BasicSmartElement]] {
    def compare(x: Array[BasicSmartElement], y: Array[BasicSmartElement]): Int = {
      val xt = x.map(SmartElement.getSortingTuple(_))
      val yt = y.map(SmartElement.getSortingTuple(_))
      var i = 0
      while (i < x.length) {
        val a = xt(i)
        val b = yt(i)
        if (a._1 > b._1)
          return 1
        if (a._1 < b._1)
          return -1
        if (a._2 > b._2)
          return 1
        if (a._2 < b._2)
          return -1
        i += 1
      }
      0
    }

    def isEquals(x: Array[BasicSmartElement], y: Array[BasicSmartElement]): Boolean = {
      compare(x, y) == 0
    }
  }

  val basicSmartTableOrdering = new Ordering[Array[BasicSmartElement]] {
    def compare(x: Array[BasicSmartElement], y: Array[BasicSmartElement]): Int = {
      var i = 0
      while (i < x.length) {
        val a = x(i).getValue
        val b = y(i).getValue
        if (a > b)
          return 1
        if (a < b)
          return -1
        i += 1
      }
      0
    }

    def isEquals(x: Array[BasicSmartElement], y: Array[BasicSmartElement]): Boolean = {
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
    def isIncluded(x: Array[BasicSmartElement], y: Array[Int]): Boolean = {
      var i = 0
      while (i < x.length) {
        val a = x(i)
        val b = y(i)
        if (!a.accept(b))
          return false
        i += 1
      }
      true
    }
  }

  private val compressionOrdering = new Ordering[Array[BasicSmartElement]] {
    def compare(x: Array[BasicSmartElement], y: Array[BasicSmartElement]): Int = {
      var i = 0
      while (i < x.length) {
        val a = x(i) match {
          case Equal(v) => v
          case GreatEq(v) => -1
          case LessEq(v) => -1
        }
        val b = y(i) match {
          case Equal(v) => v
          case GreatEq(v) => -1
          case LessEq(v) => -1
        }
        if (a > b)
          return 1
        if (a < b)
          return -1
        i += 1
      }
      0
    }

    def almostEqual(x: Array[BasicSmartElement], y: Array[BasicSmartElement], index: Int): Boolean = {
      var i = 0
      while (i < x.length) {
        if (i != index) {
          if (x(i) != (y(i)))
            return false
        }
        i += 1
      }
      true
    }
  }

  def isTupleIncluded(tuple: Array[Int]): Boolean = {
    var i = length
    while (i > 0) {
      i -= 1
      if (basicSmartTableOrdering.isIncluded(table(i), tuple))
        return true
    }
    false
  }

  protected def isTupleValid(x: Array[CPIntVar], tupleIndex: Int): Boolean = {
    val tuple = table(tupleIndex)
    var i = x.length
    while (i > 0) {
      i -= 1
      if (!tuple(i).isValid(x(i)))
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
        val newArray = new ArrayBuffer[String]()
        tuple(i).foreach(x(i), v => {
          if (i > 0)
            newArray += s + "," + v
          else
            newArray += s + v
        })
        newArray
      }
    }
    t
  }

  def getHashSet(x: Array[CPIntVar]): mutable.Set[String] = {
    import scala.collection.mutable.Set
    val set = Set[String]()
    for (index <- table.indices) {
      var t = getHashSetTuple(x, index)
      set ++= t
    }
    set
  }

  def filterTable(x: Array[CPIntVar]): Table = {
    val nbTuple = table.length
    val buffer = new Array[Array[BasicSmartElement]](nbTuple)
    var bufferSize = 0
    var tupleIndex = 0
    while (tupleIndex < nbTuple) {
      if (isTupleValid(x, tupleIndex)) {
        buffer(bufferSize) = table(tupleIndex)
        bufferSize += 1
      }
      tupleIndex += 1
    }
    val resArray = new Array[Array[BasicSmartElement]](bufferSize)
    System.arraycopy(buffer, 0, resArray, 0, bufferSize)
    Table(resArray, isSorted)
  }

  def removeDuplicate: Table = {
    if (table.length <= 1) this
    else {
      val buff = new ArrayBuffer[Array[BasicSmartElement]]()
      for (i <- table.indices) {
        var add = true
        var j = 0
        while (j < buff.length && add) {
          if (basicSmartTableOrdering.isEquals(table(i), buff(j)))
            add = false
          j += 1
        }
        if (add)
          buff += table(i)
      }
      Table(buff.toArray, isSorted)
    }
  }


  def sortTable: Table = {
    if (isSorted) {
      this
    } else {
      val sortedTable = table.clone()
      scala.util.Sorting.quickSort(sortedTable)(basicSmartTableOrdering)
      Table(sortedTable, true)
    }
  }

  def applyOffset(offsets: Array[Int]): Table = {
    Table(Array.tabulate(table.length)(i => Array.tabulate(offsets.length)(j => table(i)(j).applyOffset(offsets(j)).asInstanceOf[BasicSmartElement])), isSorted)
  }

  def decompressToGroundTable(x: Array[CPIntVar]): GroundTable = {
    val buff = new ArrayBuffer[Array[Int]]()
    val size = x.length
    val queueTupleBs = new mutable.Queue[Array[BasicSmartElement]]()
    val queueTuple = new mutable.Queue[Array[Int]]()
    val queueIndex = new mutable.Queue[Int]()
    var idx = table.length
    while (idx > 0) {
      idx -= 1
      var tupleBs = table(idx)
      var tuple = Array.fill(size)(0)
      var index = 0
      while (tuple != null) {
        if (index == size) {
          buff += tuple
        } else {
          tupleBs(index).foreach(x(index), v => {
            val newTuple = tuple.clone()
            newTuple(index) = v
            queueTupleBs.enqueue(tupleBs)
            queueTuple.enqueue(newTuple)
            queueIndex.enqueue(index + 1)
          })
        }
        if (queueTuple.isEmpty)
          tuple = null
        else {
          tupleBs = queueTupleBs.dequeue()
          tuple = queueTuple.dequeue()
          index = queueIndex.dequeue()
        }
      }
    }
    val tab = Table(buff.toArray, false)
    tab.sortTable.removeDuplicate.asInstanceOf[GroundTable]
  }

  def mapToBasicSmartTable(x: Array[CPIntVar]): BasicSmartTable =
    this

  def compressToBasicSmartTable(x: Array[CPIntVar]): BasicSmartTable =
    this

  // only applicable on an only Equal BasicSmartTable
  def compressItSelf(x: Array[CPIntVar], onlyLessEqGreatEq: Boolean = false): BasicSmartTable = {
    assert(table.forall(tuple => tuple.forall(_.isInstanceOf[Equal])), "You try to use the compression on a basic smart table not entierly composed of Equals")
    val offsets = Array.tabulate(x.length)(i => x(i).min)
    val offsetsOposite = Array.tabulate(x.length)(i => -offsets(i))
    val spans = Array.tabulate(x.length)(i => x(i).max - x(i).min + 1)
    val domains = Array.tabulate(x.length) { i =>
      val array = new Array[Int](x(i).size)
      x(i).fillArray(array)
      for (j <- array.indices)
        array(j) -= offsets(i)
      scala.util.Sorting.quickSort(array)
      array
    }
    val newTablet = this.applyOffset(offsets).asInstanceOf[BasicSmartTable]
    val newTable = newTablet
      .recursiveCompressing(x.length, domains, spans, 0).sortTable.removeDuplicate.asInstanceOf[BasicSmartTable]
    (if (!onlyLessEqGreatEq) {
      val k = newTable.postProcess(domains)
      k
    } else {
      newTable
    }).applyOffset(offsetsOposite).asInstanceOf[BasicSmartTable]
  }

  private def mostMax(domain: Array[Int], v: Int): Int = {
    var low = 0
    var high = domain.length - 1
    while (low != high) {
      val mid = (low + high + 1) / 2
      if (domain(mid) < v) {
        low = mid
      } else {
        high = mid - 1
      }
    }
    low
  }

  private def mostMin(domain: Array[Int], v: Int): Int = {
    var low = 0
    var high = domain.length - 1
    while (low != high) {
      val mid = (low + high) / 2
      if (domain(mid) <= v) {
        low = mid + 1
      } else {
        high = mid
      }
    }
    low
  }

  // add * and != after compression
  private def postProcess(domains: Array[Array[Int]]): BasicSmartTable = {

    val resultTable = Array.tabulate(length)(i => table(i))
    var nbTuple = length
    var idx = domains.length
    while (idx > 0) {
      idx -= 1
      val size = domains(idx).length
      val domMin = domains(idx)(0)
      val domMax = domains(idx).last
      if (size == 2) {
        var k = nbTuple
        while (k > 0) {
          k -= 1
          resultTable(k)(idx) match {
            case GreatEq(v) =>
              if (v <= domMin) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = Star()
                resultTable(k) = newTuple
              }
            case LessEq(v) =>
              if (v >= domMax) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = Star()
                resultTable(k) = newTuple
              }
            case _ =>
          }
        }
      } else if (size == 3) {
        val mid = domains(idx)(1)
        var k = 0
        while (k < nbTuple) {
          resultTable(k)(idx) match {
            case GreatEq(v) =>
              if (v <= domMin) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = Star()
                resultTable(k) = newTuple
              } else if (v <= mid) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = NotEqual(domMin)
                resultTable(k) = newTuple
              }
            case LessEq(v) =>
              if (v >= domMax) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = Star()
                resultTable(k) = newTuple
              } else if (v >= mid) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = NotEqual(domMax)
                resultTable(k) = newTuple
              }
            case Equal(v) =>
              if (v == domMin) {
                var k2 = k
                while (k2 < nbTuple) {
                  val tempTuple = resultTable(k2)
                  if (tempTuple(idx).isInstanceOf[Equal] && tempTuple(idx).getValue == domMax && compressionOrdering.almostEqual(tempTuple, resultTable(k), idx)) {
                    val newTuple = resultTable(k).clone()
                    newTuple(idx) = NotEqual(mid)
                    resultTable(k) = newTuple
                    nbTuple -= 1
                    resultTable(k2) = resultTable(nbTuple)
                    k2 = nbTuple
                  }
                  k2 += 1
                }
              } else if (v == domMax) {
                var k2 = k
                while (k2 < nbTuple) {
                  val tempTuple = resultTable(k2)
                  if (tempTuple(idx).isInstanceOf[Equal] && tempTuple(idx).getValue == domMin && compressionOrdering.almostEqual(tempTuple, resultTable(k), idx)) {
                    val newTuple = resultTable(k).clone()
                    newTuple(idx) = NotEqual(mid)
                    resultTable(k) = newTuple
                    nbTuple -= 1
                    resultTable(k2) = resultTable(nbTuple)
                    k2 = nbTuple
                  }
                  k2 += 1
                }
              }
            case _ =>
          }
          k += 1
        }
      } else if (size > 3) {
        val domPreMax = domains(idx)(size - 2)
        val domPrePreMax = domains(idx)(size - 3)
        val domPreMin = domains(idx)(1)
        val domPrePreMin = domains(idx)(2)
        var k = 0
        while (k < nbTuple) {
          resultTable(k)(idx) match {
            case GreatEq(v) =>
              if (v <= domMin) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = Star()
                resultTable(k) = newTuple
              } else if (v <= domPreMin) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = NotEqual(domMin)
                resultTable(k) = newTuple
              } else if (v <= domPrePreMin) {
                var k2 = k
                while (k2 < nbTuple) {
                  val tempTuple = resultTable(k2)
                  if (tempTuple(idx).isInstanceOf[Equal] && tempTuple(idx).getValue == domMin && compressionOrdering.almostEqual(tempTuple, resultTable(k), idx)) {
                    val newTuple = resultTable(k).clone()
                    newTuple(idx) = NotEqual(domPreMin)
                    resultTable(k) = newTuple
                    nbTuple -= 1
                    resultTable(k2) = resultTable(nbTuple)
                    k2 = nbTuple
                  }
                  k2 += 1
                }
              } else {
                val mid = mostMax(domains(idx), v)
                var k2 = k
                while (k2 < nbTuple) {
                  val tempTuple = resultTable(k2)
                  if (tempTuple(idx).isInstanceOf[LessEq] && mid == mostMin(domains(idx), tempTuple(idx).getValue) && compressionOrdering.almostEqual(tempTuple, resultTable(k), idx)) {
                    val newTuple = resultTable(k).clone()
                    newTuple(idx) = NotEqual(domains(idx)(mid))
                    resultTable(k) = newTuple
                    nbTuple -= 1
                    resultTable(k2) = resultTable(nbTuple)
                    k2 = nbTuple
                  }
                  k2 += 1
                }
              }
            case LessEq(v) =>
              if (v >= domMax) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = Star()
                resultTable(k) = newTuple
              } else if (v >= domPreMax) {
                val newTuple = resultTable(k).clone()
                newTuple(idx) = NotEqual(domMax)
                resultTable(k) = newTuple
              } else if (v >= domPrePreMax) {
                var k2 = k
                while (k2 < nbTuple) {
                  val tempTuple = resultTable(k2)
                  if (tempTuple(idx).isInstanceOf[Equal] && tempTuple(idx).getValue == domMax && compressionOrdering.almostEqual(tempTuple, resultTable(k), idx)) {
                    val newTuple = resultTable(k).clone()
                    newTuple(idx) = NotEqual(domPreMax)
                    resultTable(k) = newTuple
                    nbTuple -= 1
                    resultTable(k2) = resultTable(nbTuple)
                    k2 = nbTuple
                  }
                  k2 += 1
                }
              } else {
                val mid = mostMin(domains(idx), v)
                var k2 = k
                while (k2 < nbTuple) {
                  val tempTuple = resultTable(k2)
                  if (tempTuple(idx).isInstanceOf[GreatEq] && mid == mostMax(domains(idx), tempTuple(idx).getValue) && compressionOrdering.almostEqual(tempTuple, resultTable(k), idx)) {
                    val newTuple = resultTable(k).clone()
                    newTuple(idx) = NotEqual(domains(idx)(mid))
                    resultTable(k) = newTuple
                    nbTuple -= 1
                    resultTable(k2) = resultTable(nbTuple)
                    k2 = nbTuple
                  }
                  k2 += 1
                }
              }
            case Equal(v) =>
              if (v == domMin) {
                var k2 = k
                while (k2 < nbTuple) {
                  val tempTuple = resultTable(k2)
                  if (tempTuple(idx).isInstanceOf[GreatEq] && tempTuple(idx).getValue > domPreMin && tempTuple(idx).getValue <= domPrePreMin && compressionOrdering.almostEqual(tempTuple, resultTable(k), idx)) {
                    val newTuple = resultTable(k).clone()
                    newTuple(idx) = NotEqual(domPreMin)
                    resultTable(k) = newTuple
                    nbTuple -= 1
                    resultTable(k2) = resultTable(nbTuple)
                    k2 = nbTuple
                  }
                  k2 += 1
                }
              } else if (v == domMax) {
                var k2 = k
                while (k2 < nbTuple) {
                  val tempTuple = resultTable(k2)
                  if (tempTuple(idx).isInstanceOf[LessEq] && tempTuple(idx).getValue < domPreMax && tempTuple(idx).getValue >= domPrePreMax && compressionOrdering.almostEqual(tempTuple, resultTable(k), idx)) {
                    val newTuple = resultTable(k).clone()
                    newTuple(idx) = NotEqual(domPreMax)
                    resultTable(k) = newTuple
                    nbTuple -= 1
                    resultTable(k2) = resultTable(nbTuple)
                    k2 = nbTuple
                  }
                  k2 += 1
                }
              }
            case _ =>
          }
          k += 1
        }
      }
    }

    val finalTable = new Array[Array[BasicSmartElement]](nbTuple)
    System.arraycopy(resultTable, 0, finalTable, 0, nbTuple)
    new BasicSmartTable(finalTable)
  }

  private def recursiveCompressing(arity: Int, domains: Array[Array[Int]], spans: Array[Int], nbGorL: Int): BasicSmartTable = {

    val bufferCompressed = new ArrayBuffer[Array[BasicSmartElement]]()
    var countCompressed = 0

    @inline def addCompressTuple(tuple: Array[BasicSmartElement]) = {
      countCompressed += 1
      bufferCompressed += tuple
    }

    val tempTuple = Array.fill(length)(0)
    var tempTupleSize = 0

    @inline def addTempTuple(tuple: Int) = {
      tempTuple(tempTupleSize) = tuple
      tempTupleSize += 1
    }

    @inline def resetTempTuple =
      tempTupleSize = 0

    val maxmin = new Array[Int](nbGorL)
    val maxminsave = new Array[Int](nbGorL)

    val bitsets = Array.tabulate(arity)(i => Array.fill(spans(i))(new SparseBitSet(length)))
    val bitsetV = Array.fill(arity)(new SparseBitSet(length))
    val bitsetG = Array.fill(arity)(new SparseBitSet(length))
    val bitsetL = Array.fill(arity)(new SparseBitSet(length))

    val unUsed = new SparseBitSet(length)
    for (i <- 0 until length)
      unUsed.set(i)
    unUsed.initNonZero()
    val mask = new SparseBitSet(length)
    for (i <- 0 until length)
      mask.set(i)
    mask.initNonZero()

    val usedTemp = Array.fill(spans.max)(-1)
    var usedTempSize = 0

    @inline def addUsed(tuple: Int) = {
      usedTemp(usedTempSize) = tuple
      usedTempSize += 1
    }

    @inline def removeUsedFromUnused = {
      var p = usedTempSize
      while (p > 0) {
        p -= 1
        unUsed.remove(usedTemp(p))
      }
    }

    @inline def resetUsed =
      usedTempSize = 0

    scala.util.Sorting.quickSort(table)(compressionOrdering)

    for {
      t <- 0 until length
      x <- 0 until arity
    } {
      table(t)(x) match {
        case Equal(v) =>
          bitsets(x)(v).set(t)
          bitsetV(x).set(t)
        case GreatEq(v) =>
          bitsetG(x).set(t)
        case LessEq(v) =>
          bitsetL(x).set(t)
      }
    }

    bitsets.foreach(_.foreach(_.initNonZero()))
    bitsetV.foreach(_.initNonZero())
    bitsetL.foreach(_.initNonZero())
    bitsetG.foreach(_.initNonZero())

    val pattern = Array.fill(arity)(-1)
    val indexGorL = Array.fill(nbGorL)(0)

    def compress(index: Int, intersection: SparseBitSet) = {

      resetTempTuple

      intersection.foreach { i =>
        if ((tempTupleSize == 0) || (table(tempTuple(tempTupleSize - 1))(index).getValue != table(i)(index).getValue))
          addTempTuple(i)
      }

      var low = 0
      var high = tempTupleSize
      while (low != high) {
        val mid = (low + high) / 2
        val fromTuple = table(tempTuple(mid))(index).getValue
        val fromDomain = domains(index)(mid)
        if (fromTuple > fromDomain) {
          high = mid
        } else {
          low = mid + 1
        }
      }

      if (low > 1) {

        var i = nbGorL
        while (i > 0) {
          i -= 1
          maxmin(i) = if (pattern(indexGorL(i)) == -2) spans(indexGorL(i)) else -1
        }
        resetUsed

        var t = 0
        while (t < low) {
          System.arraycopy(maxmin, 0, maxminsave, 0, nbGorL)
          var reset = false
          var take = true
          val currTupleID = tempTuple(t)
          val currTuple = table(currTupleID)
          var k = nbGorL
          while (k > 0) {
            k -= 1
            val varIndex = indexGorL(k)
            if (pattern(varIndex) == -2) {
              /// if <=
              if (maxmin(k) > currTuple(varIndex).getValue) {
                maxmin(k) = currTuple(varIndex).getValue
                reset = true
              } else if (maxmin(k) < currTuple(varIndex).getValue) {
                take = false
              }
            } else {
              // if >=
              if (maxmin(k) < currTuple(varIndex).getValue) {
                maxmin(k) = currTuple(varIndex).getValue
                reset = true
              } else if (maxmin(k) > currTuple(varIndex).getValue) {
                take = false
              }
            }
          }
          if (reset && t > 0) {
            if (usedTempSize >= 2) {
              val smartTuple: Array[BasicSmartElement] = Array.tabulate(arity) {
                i =>
                  if (pattern(i) >= 0) {
                    Equal(pattern(i))
                  } else if (pattern(i) == -1) {
                    LessEq(domains(index)(t - 1))
                  } else if (pattern(i) == -2) {
                    LessEq(maxminsave(indexGorL.indexOf(i)))
                  } else {
                    GreatEq(maxminsave(indexGorL.indexOf(i)))
                  }
              }
              addCompressTuple(smartTuple)
              removeUsedFromUnused
            }
            resetUsed
          }
          if (take && unUsed.contains(currTupleID))
            addUsed(currTupleID)
          t += 1
        }
        if (usedTempSize >= 2) {
          val smartTuple: Array[BasicSmartElement] = Array.tabulate(arity) {
            i =>
              if (pattern(i) >= 0) {
                Equal(pattern(i))
              } else if (pattern(i) == -1) {
                LessEq(domains(index)(low - 1))
              } else if (pattern(i) == -2) {
                LessEq(maxmin(indexGorL.indexOf(i)))
              } else {
                GreatEq(maxmin(indexGorL.indexOf(i)))
              }
          }
          addCompressTuple(smartTuple)
          removeUsedFromUnused
        }

      }

      // find ending tail
      if (low < tempTupleSize) {

        val diff = domains(index).length - tempTupleSize
        var low = -1
        var high = tempTupleSize - 1
        while (low != high) {
          val mid = (low + high + 1) / 2
          val fromTuple = table(tempTuple(mid))(index).getValue
          val fromDomain = domains(index)(mid + diff)
          if (fromTuple < fromDomain) {
            low = mid
          } else {
            high = mid - 1
          }
        }

        if (low < tempTupleSize - 2) {
          var i = nbGorL
          while (i > 0) {
            i -= 1
            maxmin(i) = if (pattern(indexGorL(i)) == -2) spans(indexGorL(i)) else -1
          }
          resetUsed

          var t = tempTupleSize - 1
          while (t > low) {
            System.arraycopy(maxmin, 0, maxminsave, 0, nbGorL)
            var reset = false
            var take = true
            val currTupleID = tempTuple(t)
            val currTuple = table(currTupleID)
            var k = nbGorL
            while (k > 0) {
              k -= 1
              val varIndex = indexGorL(k)
              if (pattern(varIndex) == -2) {
                if (maxmin(k) > currTuple(varIndex).getValue) {
                  maxmin(k) = currTuple(varIndex).getValue
                  reset = true
                } else if (maxmin(k) < currTuple(varIndex).getValue) {
                  take = false
                }
              } else {
                if (maxmin(k) < currTuple(varIndex).getValue) {
                  maxmin(k) = currTuple(varIndex).getValue
                  reset = true
                } else if (maxmin(k) > currTuple(varIndex).getValue) {
                  take = false
                }
              }
            }
            if (reset && t < tempTupleSize - 1) {
              if (usedTempSize >= 2) {
                val smartTuple: Array[BasicSmartElement] = Array.tabulate(arity) {
                  i =>
                    if (pattern(i) >= 0) {
                      Equal(pattern(i))
                    } else if (pattern(i) == -1) {
                      GreatEq(domains(index)(diff + t + 1))
                    } else if (pattern(i) == -2) {
                      LessEq(maxminsave(indexGorL.indexOf(i)))
                    } else {
                      GreatEq(maxminsave(indexGorL.indexOf(i)))
                    }
                }
                addCompressTuple(smartTuple)
                removeUsedFromUnused
              }
              resetUsed
            }
            if (take && unUsed.contains(currTupleID))
              addUsed(currTupleID)
            t -= 1
          }
          if (usedTempSize >= 2) {
            val smartTuple: Array[BasicSmartElement] = Array.tabulate(arity) {
              i =>
                if (pattern(i) >= 0) {
                  Equal(pattern(i))
                } else if (pattern(i) == -1) {
                  GreatEq(domains(index)(diff + low + 1))
                } else if (pattern(i) == -2) {
                  LessEq(maxmin(indexGorL.indexOf(i)))
                } else {
                  GreatEq(maxmin(indexGorL.indexOf(i)))
                }
            }
            addCompressTuple(smartTuple)
            removeUsedFromUnused
          }
        }
      }
    }

    def loop(i: Int, index: Int, nb: Int, gorL: Int, intersection: SparseBitSet): Unit = {
      if (i == arity) {
        compress(index, intersection)
      } else if (i == index) {
        pattern(i) = -1 // ?
        loop(i + 1, index, nb + 1, gorL, intersection.intersect(bitsetV(i)))
      } else {
        if (arity - i > nbGorL + 1 - nb && intersection.isIntersectAtLeastTwo(bitsetV(i), unUsed)) {
          for {
            v <- domains(i)
            if intersection.isIntersect(bitsets(i)(v))
          } {
            pattern(i) = v
            loop(i + 1, index, nb, gorL, intersection.intersect(bitsets(i)(v)))
          }
        }
        if (nb <= nbGorL) {
          if (intersection.isIntersectAtLeastTwo(bitsetG(i), unUsed)) {
            pattern(i) = -3 // >=
            indexGorL(gorL) = i
            loop(i + 1, index, nb + 1, gorL + 1, intersection.intersect(bitsetG(i)))
          }
          if (intersection.isIntersectAtLeastTwo(bitsetL(i), unUsed)) {
            pattern(i) = -2 // <=
            indexGorL(gorL) = i
            loop(i + 1, index, nb + 1, gorL + 1, intersection.intersect(bitsetL(i)))
          }
        }
      }
    }

    var i = 0
    while (i < arity) {
      loop(0, i, 0, 0, mask)
      i += 1
    }

    val tableUnused = new Array[Array[BasicSmartElement]](unUsed.count)
    var idx = 0
    unUsed.foreach { i =>
      tableUnused(idx) = table(i)
      idx += 1
    }

    // recursive call if needed
    val compressedTable = if (nbGorL < (arity - 1) && bufferCompressed.length > 1)
      new BasicSmartTable(bufferCompressed.toArray).recursiveCompressing(arity, domains, spans, nbGorL + 1)
    else
      new BasicSmartTable(bufferCompressed.toArray)

    compressedTable.union(tableUnused)
  }

  def union(other: BasicSmartTable): BasicSmartTable = union(other.getTable)

  def union(other: Array[Array[BasicSmartElement]]): BasicSmartTable = {
    val result: Array[Array[BasicSmartElement]] = new Array(this.length + other.length)
    System.arraycopy(this.table, 0, result, 0, this.length)
    System.arraycopy(other, 0, result, this.length, other.length)
    new BasicSmartTable(result)
  }

  def transformTo_MDD: Diagram = {
    assert(length != 0,"The table should contain at least one tuple to be transformed into a diagram")
    if (isSorted) {
      val transaction = Array.fill(table(0).length)(new ArrayBuffer[(Int, BasicSmartElement, Int)]())
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
      new BasicSmartDiagram(trans, nodes.map(_ + 1)).reduceTree_MDD
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
        scala.util.Sorting.quickSort(tablePart1)(basicSmartTableOrdering2)
        scala.util.Sorting.quickSort(tablePart2)(basicSmartTableOrdering2)

        val buff1 = new ArrayBuffer[Array[BasicSmartElement]]()
        var prev = tablePart1(0)
        buff1 += prev
        for (i <- 1 until tablePart1.length) {
          if (!basicSmartTableOrdering2.isEquals(tablePart1(i), prev)) {
            prev = tablePart1(i)
            buff1 += prev
          }
        }
        tablePart1 = buff1.toArray

        val buff2 = new ArrayBuffer[Array[BasicSmartElement]]()
        prev = tablePart2(0)
        buff2 += prev
        for (i <- 1 until tablePart2.length) {
          if (!basicSmartTableOrdering2.isEquals(tablePart2(i), prev)) {
            prev = tablePart2(i)
            buff2 += prev
          }
        }
        tablePart2 = buff2.toArray

        val transaction = Array.fill(arity)(new ArrayBuffer[(Int, BasicSmartElement, Int)]())

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
        val mdd = new BasicSmartDiagram(trans, nodes.map(_ + 1))
        val mdd2 = mdd.reduceTree_sMDD
        mdd2
      } else {
        this.sortTable.removeDuplicate.asInstanceOf[BasicSmartTable].transformTo_sMDD
      }
    }
  }

  override def toString(): String = {
    var str = new StringBuilder
    for (i <- table.indices) {
      str ++= table(i).mkString(",")
      str += '\n'
    }
    str.toString
  }


  // used for the compression algorithm
  private class SparseBitSet(n: Int) {

    val nbWords = BitSetOp.bitLength(n)

    val words = Array.fill(nbWords)(0L)

    val nonZero = Array.fill(nbWords)(Int.MaxValue)
    var nonZeroSize = 0

    def set(v: Int) = {
      BitSetOp.setBit(words, v)
    }

    def remove(v: Int) = {
      words(BitSetOp.bitOffset(v)) &= ~BitSetOp.oneBitLong(v)
    }

    def contains(v: Int) = {
      (words(BitSetOp.bitOffset(v)) & BitSetOp.oneBitLong(v)) != 0
    }

    def initNonZero() = {
      var i = 0
      while (i < nbWords) {
        if (words(i) != 0L) {
          nonZero(nonZeroSize) = i
          nonZeroSize += 1
        }
        i += 1
      }
    }

    def updateNonZero() = {
      var i = nonZeroSize
      while (i > 0) {
        i -= 1
        if (words(nonZero(i)) == 0L) {
          nonZeroSize -= 1
          nonZero(i) = nonZero(nonZeroSize)
          nonZero(nonZeroSize) = Int.MaxValue
        }
      }
      scala.util.Sorting.quickSort(nonZero)
    }


    def count = {
      var cnt = 0
      var i = nonZeroSize
      while (i > 0) {
        i -= 1
        cnt += java.lang.Long.bitCount(words(nonZero(i)))
      }
      cnt
    }

    def isIntersect(bs: SparseBitSet): Boolean = {
      if (this.nonZeroSize < bs.nonZeroSize) {
        var i = this.nonZeroSize
        while (i > 0) {
          i -= 1
          val index = this.nonZero(i)
          //        println("+++" + this.words(index) + " " + bs.words(index))
          if ((this.words(index) & bs.words(index)) != 0L)
            return true
        }
      } else {
        var i = bs.nonZeroSize
        while (i > 0) {
          i -= 1
          val index = bs.nonZero(i)
          //        println("+++" + this.words(index) + " " + bs.words(index))
          if ((this.words(index) & bs.words(index)) != 0L)
            return true
        }
      }
      false
    }

    def isIntersectAtLeastTwo(bs1: SparseBitSet, bs2: SparseBitSet): Boolean = {
      var count = -2
      if (this.nonZeroSize <= bs1.nonZeroSize && this.nonZeroSize <= bs2.nonZeroSize) {
        var i = this.nonZeroSize
        while (i > 0) {
          i -= 1
          val index = this.nonZero(i)
          val intersect = this.words(index) & bs1.words(index) & bs2.words(index)
          if (intersect != 0L) {
            count += java.lang.Long.bitCount(intersect)
            if (count >= 0)
              return true
          }
        }
      } else if (bs1.nonZeroSize <= bs2.nonZeroSize) {
        var i = bs1.nonZeroSize
        while (i > 0) {
          i -= 1
          val index = bs1.nonZero(i)
          val intersect = this.words(index) & bs1.words(index) & bs2.words(index)
          if (intersect != 0L) {
            count += java.lang.Long.bitCount(intersect)
            if (count >= 0)
              return true
          }
        }
      } else {
        var i = bs2.nonZeroSize
        while (i > 0) {
          i -= 1
          val index = bs2.nonZero(i)
          val intersect = this.words(index) & bs1.words(index) & bs2.words(index)
          if (intersect != 0L) {
            count += java.lang.Long.bitCount(intersect)
            if (count >= 0)
              return true
          }
        }
      }
      false
    }

    def intersect(bs: SparseBitSet): SparseBitSet = {
      val result = new SparseBitSet(n)
      if (this.nonZeroSize < bs.nonZeroSize) {
        var i = this.nonZeroSize
        result.nonZeroSize = i
        while (i > 0) {
          i -= 1
          val index = this.nonZero(i)
          result.words(index) = this.words(index) & bs.words(index)
          result.nonZero(i) = index
        }
      } else {
        var i = bs.nonZeroSize
        result.nonZeroSize = i
        while (i > 0) {
          i -= 1
          val index = bs.nonZero(i)
          result.words(index) = this.words(index) & bs.words(index)
          result.nonZero(i) = index
        }
      }
      result.updateNonZero()
      result
    }

    def foreach(block: Int => Unit) {
      var offset = 0
      var k = 0
      var temp: Long = 0L
      while (k < words.length) {
        temp = words(k)
        while (temp != 0L) {
          val v = java.lang.Long.numberOfTrailingZeros(temp)
          block(offset + v)
          temp = temp ^ (1L << v)
        }
        offset += 64
        k += 1
      }
    }


    override def toString: String = {
      val size = n min 64
      words.map(e => String.format(s"%${size}s", java.lang.Long.toBinaryString(e)).replace(' ', '0')).mkString(" ")
    }
  }
}


