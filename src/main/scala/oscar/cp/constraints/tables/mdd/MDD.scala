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

import oscar.algo.VectorMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

/**
 * Representation of a table by a Multivalued Decision Diagram.
 * @param table the tuples composing the MDD
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 */
class MDD(table: Array[Array[Int]]) {

  private[this] val arity = if (table.length > 0) table(0).length else 0
  private[this] val varsRange = (0 until arity).toArray
  private[this] val nbChildren: Array[Int] = varsRange.map(i => table.map(x => x(i)).distinct.length)
  private[this] val vMaps: Array[VectorMap[Int]] =  varsRange.map(i => new VectorMap[Int]())
  private[this] val nbNodesLvl = Array.fill(arity + 1)(0)
  private[this] var nbNodes: Int = 0

  /* Pointers to firstNodes and lastNodes in each layer */
  private[this] val firstNodes = new Array[Node](arity + 1)
  private[this] val lastNodes = new Array[Node](arity + 1)

  /* Construction of the tree */
  val root = buildTreeFromTable()

  /* Reduction of the tree */
  reduceTree()

  def nbInstance = nbNodes

  def valuesMapping = vMaps

  private final def buildTreeFromTable(): Node = {
    /* TRUE node */
    val trueNode = new Node(arity, nbNodes, 0, 0)
    nbNodes += 1

    /* Root of the MDD */
    val root = new Node(0, nbNodes, nbChildren(0), 0) // TODO array out of bound if table length == 0
    nbNodes += 1

    /* Construction of the tree */
    var i = 0
    while (i < table.length) {
      var currentNode = root
      var j = 0
      while (j < arity) {
        val xValues = vMaps(j)
        val a = table(i)(j)

        /* If it's the first time that we meet (x,a) */
        if (!xValues.contains(a)) {
          xValues.add(a)
        }

        val aIndex = xValues.index(a)
        if (currentNode(aIndex) == null) {
          if (j + 1 < arity) {
            val newNode = new Node(j + 1, nbNodes, nbChildren(j + 1), nbNodesLvl(j+1))
            newNode.addParent(currentNode, aIndex)
            currentNode.setChild(aIndex, newNode)
            nbNodes += 1
            nbNodesLvl(j+1) += 1

            if (lastNodes(j+1) != null) {
              lastNodes(j+1).next = newNode
              newNode.prev = lastNodes(j+1)
            }
            else {
              firstNodes(j+1) = newNode
            }
            lastNodes(j+1) = newNode

          }
          else {
            trueNode.addParent(currentNode, aIndex)
            currentNode.setChild(aIndex, trueNode)
          }
        }
        currentNode = currentNode(aIndex)
        j += 1
      }
      firstNodes(0) = root
      lastNodes(0) = root
      nbNodesLvl(0) = 1
      firstNodes(arity) = trueNode
      lastNodes(arity) = trueNode
      nbNodesLvl(arity) = 1

      i += 1
    }

    root
  }

  private final def reduceTree(): Unit = {
    /* Temp arrays */
    val nbChildrenMax = nbChildren.max
    val nbNodesLvlMax = nbNodesLvl.max
    val valuesArrays = Array.fill(nbNodesLvlMax)(Array.fill(nbChildrenMax)(0))
    val valuesArraysSize = Array.fill(nbNodesLvlMax)(0)
    val nodesList = Array.fill(nbNodesLvlMax)(0)
    val stringBuilder = new StringBuilder()

    var i = firstNodes.length - 2
    while (i >= 1) {
      val levelMap = new HashMap[String, Node]()
      var currentNode = firstNodes(i)
      while (currentNode != null) {
        val hash = nodeHash(currentNode, valuesArrays, valuesArraysSize, nodesList, stringBuilder)
        if (levelMap.contains(hash)) {
          val temp = currentNode.prev
          mergeNodes(levelMap(hash), currentNode)
          currentNode = temp
        }
        else {
          levelMap.put(hash, currentNode)
        }
        currentNode = currentNode.next
      }
      i -= 1
    }
  }

  @inline private final def nodeHash(node: Node, valuesArrays: Array[Array[Int]], valuesArraysSize: Array[Int], nodesList: Array[Int], hash: StringBuilder) = {
    val children = new HashMap[Int, Int]()
    var nodesListSize = 0
    var nbChildren = 0

    var i = 0
    while (i < node.nbChildren) {
      val child = node(i)
      if (child != null) {
        if (children.contains(child.levelId)) {
          val arrayIndex = children(child.levelId)
          valuesArrays(arrayIndex)(valuesArraysSize(arrayIndex)) = i
          valuesArraysSize(arrayIndex) += 1
        }
        else {
          valuesArrays(nbChildren)(0) = i
          valuesArraysSize(nbChildren) = 1
          children.put(child.levelId, nbChildren)
          nbChildren += 1

          nodesList(nodesListSize) = child.levelId
          nodesListSize += 1
        }
      }
      i += 1
    }

    i = 0
    hash.clear()
    while (i < nodesListSize) {
      val childIndex = nodesList(i)
      val arrayIndex = children(childIndex)
      val values = valuesArrays(arrayIndex)
      val valuesSize = valuesArraysSize(arrayIndex)
      var j = 0
      while (j < valuesSize) {
        if (j > 0) {
          hash.append(",")
        }
        hash.append(values(j))
        j += 1
      }
      hash.append("(")
      hash.append(childIndex)
      hash.append(")")
      i += 1
    }

    hash.toString()
  }

  @inline private final def mergeNodes(n1: Node, n2: Node): Unit = {
    /* Change parents */
    for (parent <- n2.parents.keySet) {
      val values = n2.parents(parent)
      for (value <- values) {
        parent.setChild(value, n1)
      }
    }

    /* Remove n2 from its layer */
    val layer = n2.indexVar
    if (n2.prev != null) {
      n2.prev.next = n2.next
    }
    else {
      firstNodes(layer) = n2.next
    }

    if (n2.next != null) {
      n2.next.prev = n2.prev
    }
  }

}

class Node(val indexVar: Int, val id: Int, val nbChildren: Int, val levelId: Int) {

  /* Parents and children of this node */
  val children = new Array[Node](nbChildren)
  val parents = new HashMap[Node, ArrayBuffer[Int]]()

  /* Information used to traverse the MDD */
  var visited = false
  var next: Node = null
  var prev: Node = null

  @inline final def apply(i: Int): Node = children(i)

  @inline final def setChild(i: Int, c: Node): Unit = children(i) = c

  @inline final def addParent(p: Node, value: Int): Unit = {
    if (!parents.contains(p)) {
      val valueArray = new ArrayBuffer[Int]()
      valueArray += value
      parents.put(p, valueArray)
    }
    else {
      parents(p) += value
    }
  }

}