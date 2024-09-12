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
 * **************************************************************************** */

package oscar.cp.constraints.diagrams.diagramrepresentation

import oscar.cp.constraints.tables.tablerepresentation.Table
import oscar.cp.constraints.tables.{BasicSmartElement, SmartElement}
import oscar.cp.core.variables.CPIntVar

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */

class GroundDiagram(transactionByVar: Array[Array[(Int, Int, Int)]], nbNodesByVar: Array[Int]) extends Diagram {
  val nbLevel = transactionByVar.length

  def getTransaction(level:Int) = transactionByVar(level)
  def isEmpty:Boolean = {
    transactionByVar(0).isEmpty
  }
  def getEdges(lvl:Int) = transactionByVar(lvl)

  def nbNodes: Int = nbNodesByVar.sum

  def nbEdges: Int = transactionByVar.foldLeft(0)((a, b) => a + b.length)

  def nbEdges(level: Int): Int = transactionByVar(level).length

  def nbSourceNodes(level: Int): Int = nbNodesByVar(level)

  def nbDestinationNodes(level: Int): Int = nbNodesByVar(level + 1)

  def isTupleValid(x: Array[CPIntVar], tuple: Array[Int]): Boolean = ???

  def getHashSet(x: Array[CPIntVar]): mutable.Set[String] = {
    var buff = Array(mutable.Set(""))
    for (lvl <- 0 until nbLevel) {
      val newArray = Array.fill(nbNodesByVar(lvl + 1))(mutable.Set[String]())
      for ((from, value, to) <- transactionByVar(lvl)) {
        if (lvl == 0)
          newArray(to) ++= buff(from).map(s => s + value.toString)
        else
          newArray(to) ++= buff(from).map(s => s + "," + value.toString)
      }
      buff = newArray
    }
    buff(0)
  }

  def filterDiagram(x: Array[CPIntVar]): Diagram = {
    // 1) downward pass
    import scala.collection.mutable.Set
    val removedNode = Array.fill(nbLevel + 1)(Set[Int]())
    val transitionDownPass = Array.fill(nbLevel)(new ArrayBuffer[(Int, Int, Int)]())
    var lvl = 0
    while (lvl < nbLevel) {
      val parents = transactionByVar(lvl).groupBy(_._3)
      var n = nbNodesByVar(lvl + 1)
      while (n > 0) {
        n -= 1
        if (parents.contains(n)) {
          val filtered = parents(n).filter(t => !removedNode(lvl).contains(t._1) && x(lvl).hasValue(t._2))
          if (filtered.isEmpty) {
            removedNode(lvl + 1).add(n)
          } else {
            transitionDownPass(lvl) ++= filtered
          }
        } else {
          removedNode(lvl + 1).add(n)
        }
      }
      lvl += 1
    }

    // 2) upward pass
    val transitionUpPass = Array.fill(nbLevel)(new ArrayBuffer[(Int, Int, Int)]())
    lvl = nbLevel
    while (lvl > 0) {
      lvl -= 1
      val child = transitionDownPass(lvl).groupBy(_._1)
      var n = nbNodesByVar(lvl)
      while (n > 0) {
        n -= 1
        if (!removedNode(lvl).contains(n)) {
          if (child.contains(n)) {
            val filtered = child(n).filter(t => !removedNode(lvl + 1).contains(t._3) && x(lvl).hasValue(t._2))
            if (filtered.isEmpty)
              removedNode(lvl).add(n)
            else
              transitionUpPass(lvl) ++= filtered
          } else {
            removedNode(lvl).add(n)
          }
        }
      }

    }

    // 3) rename nodes
    val mapNode = Array.tabulate(nbLevel + 1)(lvl => Array.tabulate(nbNodesByVar(lvl))(i => i)) // Old node name to new one
    val nbNodes = Array.fill(nbLevel + 1)(0)
    lvl = nbLevel + 1
    while (lvl > 0) {
      lvl -= 1
      val sparseSet = mapNode(lvl).clone()
      var sparseSetSize = sparseSet.length
      var n = sparseSetSize
      while (n > 0) {
        n -= 1
        if (removedNode(lvl).contains(sparseSet(n))) {
          sparseSetSize -= 1
          sparseSet(n) = sparseSet(sparseSetSize)
        }
      }
      n = sparseSetSize
      while (n > 0) {
        n -= 1
        mapNode(lvl)(sparseSet(n)) = n
      }
      nbNodes(lvl) = sparseSetSize
    }

    val transitionFinal = Array.tabulate(nbLevel)(lvl => transitionUpPass(lvl).map(t => (mapNode(lvl)(t._1), t._2, mapNode(lvl + 1)(t._3))).toArray)
    new GroundDiagram(transitionFinal, nbNodesByVar.clone)
  }

  def applyOffset(offsets: Array[Int]): Diagram = {
    val transition = Array.tabulate(transactionByVar.length) {
      lvl => Array.tabulate(transactionByVar(lvl).length) {
        i => (transactionByVar(lvl)(i)._1, transactionByVar(lvl)(i)._2 - offsets(lvl), transactionByVar(lvl)(i)._3)
      }
    }
    new GroundDiagram(transition, nbNodesByVar.clone())
  }

    def decompressToTable(x: Array[CPIntVar]): Table = {
      var buff: Array[Array[Array[Int]]] = Array(Array(Array.fill(nbLevel)(0)))
      for (lvl <- 0 until nbLevel) {
        val newArray = Array.fill(nbNodesByVar(lvl + 1))(new ArrayBuffer[Array[Int]])
        for ((from, value, to) <- transactionByVar(lvl)) {
          newArray(to) ++= buff(from).map { pattern =>
            val newTuple = pattern.clone()
            newTuple(lvl) = value
            newTuple
          }
        }
        buff = newArray.map(_.toArray)
      }
      val table = buff(0)
      Table(table, false).sortTable.removeDuplicate
    }



  def compressToBasicSmartDiagram(x: Array[CPIntVar], withSets:Boolean = false): BasicSmartDiagram = {
    val buff = Array.fill(nbLevel)(new ArrayBuffer[(Int, BasicSmartElement, Int)]())
    var lvl = nbLevel
    while (lvl > 0) {
      lvl -= 1
      val domain = new Array[Int](x(lvl).size)
      x(lvl).fillArray(domain)
      val grouped = transactionByVar(lvl).groupBy(t => (t._1, t._3))
      for (((from, to), trans) <- grouped) {
        val bsElement = SmartElement.compressToBasicSmartElement(domain, trans.map(_._2), withSets)
        for (elem <- bsElement)
          buff(lvl) += ((from, elem, to))
      }
    }
    val newTransition = Array.tabulate(nbLevel)(lvl => buff(lvl).toArray)
    new BasicSmartDiagram(newTransition, nbNodesByVar.clone())
  }

  def reduceTree_MDD: Diagram = {
    rTUpward(nbLevel)
  }

  def reduceTree_sMDD: Diagram = {
    val mid = transactionByVar.length / 2
    val smdd = rTDownward(mid - 1).asInstanceOf[GroundDiagram]
    val smdd2 = smdd.rTUpward(mid + 1)
    smdd2
  }

  // reduct tree bottom up
  private def rTUpward(from: Int): Diagram = {
    val resultTransition = new Array[Array[(Int, Int, Int)]](nbLevel)
    val nodes = nbNodesByVar.clone()

    var transForNext = transactionByVar.last
    var lvl = nbLevel //transactionByVar.length / 2
    while (lvl > 0) {
      lvl -= 1
      if (lvl < from) {
        // 1) transition of this level by node
        val child = Array.fill(nbNodesByVar(lvl))(new ArrayBuffer[Int]())
        for (i <- transForNext.indices) {
          val t = transForNext(i)
          child(t._1) += i
        }
        // 2) sort child for each node by (value,resultnode)
        val sortedChild = Array.tabulate(nbNodesByVar(lvl))(n => child(n).sortBy(i => (transForNext(i)._2, transForNext(i)._3)).toArray)
        // 3) compute hash for each node
        val hashValue = Array.tabulate(nbNodesByVar(lvl))(n => hashNode(transForNext, sortedChild(n), true))
        // 4) merge node
        val levelMap = new HashMap[String, Int]()
        val mapNode = Array.tabulate(nbNodesByVar(lvl))(i => i) // Old node name to new one
        val removed = new ArrayBuffer[Int]()
        var nbTrans = 0
        var n = 0
        while (n < nbNodesByVar(lvl)) {
          if (levelMap.contains(hashValue(n))) {
            mapNode(n) = levelMap(hashValue(n))
            removed += n
          } else {
            levelMap += ((hashValue(n), n))
            nbTrans += child(n).length
          }
          n += 1
        }

        val sparseSet = Array.tabulate(nbNodesByVar(lvl))(i => i) // keep still valid nodes
        var sizeSparseSet = nbNodesByVar(lvl)
        n = removed.length
        while (n > 0) {
          n -= 1
          sizeSparseSet -= 1
          sparseSet(removed(n)) = sparseSet(sizeSparseSet)
        }

        n = sizeSparseSet
        while (n > 0) {
          n -= 1
          mapNode(sparseSet(n)) = n
        }
        n = removed.length
        while (n > 0) {
          n -= 1
          if (mapNode(removed(n)) >= sizeSparseSet)
            mapNode(removed(n)) = mapNode(mapNode(removed(n)))
        }

        // 5) renaming current level
        resultTransition(lvl) = new Array[(Int, Int, Int)](nbTrans)
        var next = 0
        var i = sizeSparseSet
        while (i > 0) {
          i -= 1
          for (index <- child(sparseSet(i))) {
            val k = (mapNode(transForNext(index)._1), transForNext(index)._2, transForNext(index)._3)
            resultTransition(lvl)(next) = k
            next += 1
          }
        }
        nodes(lvl) = sizeSparseSet

        // 6) renaming next level
        if (lvl != 0) {
          transForNext = Array.tabulate(transactionByVar(lvl - 1).length) {
            i => (transactionByVar(lvl - 1)(i)._1, transactionByVar(lvl - 1)(i)._2, mapNode(transactionByVar(lvl - 1)(i)._3))
          }
        }
      } else {
        resultTransition(lvl) = transForNext
        if (lvl != 0) {
          transForNext = transactionByVar(lvl - 1).clone()
        }
      }
    }
    new GroundDiagram(resultTransition, nodes)
  }

  // reduct tree top down
  private def rTDownward(from: Int): Diagram = {
    val resultTransition = new Array[Array[(Int, Int, Int)]](nbLevel)
    val nodes = nbNodesByVar.clone()

    var transForNext = transactionByVar(0)
    var lvl = 0 //transactionByVar.length / 2
    while (lvl < nbLevel) {
      if (lvl > from) {
        // 1) transition of this level by node
        val parents = Array.fill(nbNodesByVar(lvl + 1))(new ArrayBuffer[Int]())
        for (i <- transForNext.indices) {
          val t = transForNext(i)
          parents(t._3) += i
        }
        // 2) sort child for each node by (value,resultnode)
        val sortedParent = Array.tabulate(nbNodesByVar(lvl + 1))(n => parents(n).sortBy(i => (transForNext(i)._2, transForNext(i)._1)).toArray)
        // 3) compute hash for each node
        val hashValue = Array.tabulate(nbNodesByVar(lvl + 1))(n => hashNode(transForNext, sortedParent(n), false))
        // 4) merge node
        val levelMap = new HashMap[String, Int]()
        val mapNode = Array.tabulate(nbNodesByVar(lvl + 1))(i => i) // Old node name to new one
        val removed = new ArrayBuffer[Int]()
        var nbTrans = 0
        var n = 0
        while (n < nbNodesByVar(lvl + 1)) {
          if (levelMap.contains(hashValue(n))) {
            mapNode(n) = levelMap(hashValue(n))
            removed += n
          } else {
            levelMap += ((hashValue(n), n))
            nbTrans += parents(n).length
          }
          n += 1
        }

        val sparseSet = Array.tabulate(nbNodesByVar(lvl + 1))(i => i) // keep still valid nodes
        var sizeSparseSet = nbNodesByVar(lvl + 1)
        n = removed.length
        while (n > 0) {
          n -= 1
          sizeSparseSet -= 1
          sparseSet(removed(n)) = sparseSet(sizeSparseSet)
        }

        n = sizeSparseSet
        while (n > 0) {
          n -= 1
          mapNode(sparseSet(n)) = n
        }
        n = removed.length
        while (n > 0) {
          n -= 1
          if (mapNode(removed(n)) >= sizeSparseSet)
            mapNode(removed(n)) = mapNode(mapNode(removed(n)))
        }

        // 5) renaming current level
        resultTransition(lvl) = new Array[(Int, Int, Int)](nbTrans)
        var next = 0
        var i = sizeSparseSet
        while (i > 0) {
          i -= 1
          for (index <- parents(sparseSet(i))) {
            val k = (transForNext(index)._1, transForNext(index)._2, mapNode(transForNext(index)._3))
            resultTransition(lvl)(next) = k
            next += 1
          }
        }
        nodes(lvl + 1) = sizeSparseSet

        // 6) renaming next level
        if (lvl != nbLevel - 1) {
          transForNext = Array.tabulate(transactionByVar(lvl + 1).length) {
            i => (mapNode(transactionByVar(lvl + 1)(i)._1), transactionByVar(lvl + 1)(i)._2, transactionByVar(lvl + 1)(i)._3)
          }
        }
      } else {
        resultTransition(lvl) = transForNext
        if (lvl != nbLevel - 1) {
          transForNext = transactionByVar(lvl + 1).clone()
        }
      }
      lvl += 1
    }
    new GroundDiagram(resultTransition, nodes)
  }

  // trans = transaction to go out (value, node)
  private def hashNode(transition: Array[(Int, Int, Int)], id: Array[Int], isUpward: Boolean): String = {
    if (id.length > 0) {
      val buff = new StringBuilder()
      buff.append(transition(id(0))._2)
      buff.append("-")
      if (isUpward)
        buff.append(transition(id(0))._3)
      else
        buff.append(transition(id(0))._1)
      var i = 1
      while (i < id.length) {
        buff.append(",")
        buff.append(transition(id(i))._2)
        buff.append("-")
        if (isUpward)
          buff.append(transition(id(i))._3)
        else
          buff.append(transition(id(i))._1)
        i += 1
      }
      buff.toString()
    } else
      ""
  }

  override def toString(): String = {
    val str = new mutable.StringBuilder()
    str.append(nbNodesByVar.mkString(",") + "\n")
    var lvl = 0
    while (lvl < nbLevel) {
      str.append("level ")
      str.append(lvl)
      str.append(":\n")
      for (t <- transactionByVar(lvl)) {
        str.append(t.toString())
        str.append("\n")
      }
      lvl += 1
    }
    str.toString()
  }

  def toString2():String = {
    val str = new mutable.StringBuilder()
    str.append("nodes " + nbNodesByVar.mkString(",") + "\n")
    str.append("edges " + transactionByVar.map(_.size).mkString(",")+"\n")
    str.toString()
  }
}
