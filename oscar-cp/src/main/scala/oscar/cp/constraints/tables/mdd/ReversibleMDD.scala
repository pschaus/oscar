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


import scala.collection.mutable.ArrayBuffer
import oscar.algo.reversible.{ReversibleContext, ReversibleInt, ReversibleSharedSparseSet}

/**
 * A directed edge between two nodes of a MDD.
 * @param value the value associated to the edge.
 * @param start the node from which the edge is leaving.
 * @param end the node to which the edge is directed.
 * @param id the unique ID of the edge.
 */
case class MDDEdge(value: Int, start: Int, end: Int, id: Int)

/**
 * A reversible node of a MDD.
 * @param context the context of the node.
 * @param indexVar the index of the variable associated to this node.
 * @param sharedIn the sparse array shared between all edges (for the 'in' sets).
 * @param sharedOut the sparse array shared between all edges (for the 'out' sets).
 * @param nbEdges the total number of edges in the MDD.
 * @param maxIn the maximum number of edges entering this node.
 * @param maxOut the maximum number of edges leaving this node.
 */
class MDDNode(context: ReversibleContext, val indexVar: Int, sharedIn: Array[Int], sharedOut: Array[Int], nbEdges: Int, maxIn: Int, maxOut: Int) {
  private[this] val in = new ReversibleSharedSparseSet(context, nbEdges, maxIn, sharedIn)
  private[this] val out = new ReversibleSharedSparseSet(context, nbEdges, maxOut, sharedOut)
  
  /* Edges getters */
  @inline final def nbIn: Int = in.size
  @inline final def nbOut: Int = out.size
  @inline final def getIn(index: Int): Int = in(index)
  @inline final def getOut(index: Int): Int = out(index)
  
  /* Edges setters */
  @inline final def removeEdgeIn(edge: Int): Unit = in.remove(edge)
  @inline final def removeEdgeOut(edge: Int): Unit = out.remove(edge)
  @inline final def insertEdgeIn(edge: Int): Unit = in.insert(edge)
  @inline final def insertEdgeOut(edge: Int): Unit = out.insert(edge)
  
  /* Reset functions*/
  @inline final def clearEdgesOut(): Unit = out.clear()
  @inline final def clearEdgesIn(): Unit = in.clear()
  @inline final def restoreEdgeOut(edge: Int): Unit = out.restore(edge)
  @inline final def restoreEdgeIn(edge: Int): Unit = in.restore(edge)
}

/**
 * A reversible MDD representing the valid tuples of a table.
 * @param context the reversible context.
 * @param table the table containing the tuples.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 */
class ReversibleMDD(context: ReversibleContext, table: Array[Array[Int]]) {
  private[this] val arity = if (table.length > 0) table(0).length else 0
  private[this] val staticMDD = new MDD(table)
  private[this] val root = staticMDD.root
  private[this] val valuesMapping = staticMDD.valuesMapping
  private[this] val nbInstance = staticMDD.nbInstance

  /* We fill the edges structure */
  private[this] var nbEdge = 0
  private[this] val tmp = scala.collection.mutable.Queue[Node]()
  
  private[this] val indexOfNodes = ArrayBuffer[Int]()
  private[this] val edgesBuffer = ArrayBuffer[MDDEdge]()
  private[this] val nbNodesLvl = Array.fill(arity + 1)(0)
  private[this] val newIds = Array.fill(nbInstance + 1)(-1)
  private[this] val nbIn = Array.fill(nbInstance)(0)
  private[this] val nbOut = Array.fill(nbInstance)(0)
  val nbOfEdgesByValue = Array.tabulate(arity)(i => Array.fill(nbOfValues(i))(0))
  
  tmp.enqueue(root)
  root.visited = true
  newIds(root.id) = indexOfNodes.size
  indexOfNodes += root.indexVar
  while(tmp.nonEmpty) {
    val mdd = tmp.dequeue()
    nbNodesLvl(mdd.indexVar) += 1
    var child = 0
    while (child < mdd.nbChildren) {
      if (mdd(child) != null) {
        if (!mdd(child).visited) {
          newIds(mdd(child).id) = indexOfNodes.size
          indexOfNodes += mdd(child).indexVar
          
          tmp.enqueue(mdd(child))
          mdd(child).visited = true
        }
        
        edgesBuffer += new MDDEdge(child, newIds(mdd.id), newIds(mdd(child).id), nbEdge)
        
        /* We keep track of some stats for the size of the dense and
         * sparse arrays of the reversible sparse sets. */
        nbEdge += 1
        nbOut(newIds(mdd.id)) += 1
        nbIn(newIds(mdd(child).id)) += 1
        nbOfEdgesByValue(mdd.indexVar)(child) += 1
      }
      child += 1
    }
  }
  
  /* We initialize the structures for the edges, the nodes and the levels */
  private[this] val edges = edgesBuffer.toArray
  val nbEdges = edges.length
  private[this] val sharedIn = Array.fill(nbEdges)(-1)
  private[this] val sharedOut = Array.fill(nbEdges)(-1)
  private[this] val nodes = indexOfNodes.indices.map(i => new MDDNode(context, indexOfNodes(i), sharedIn, sharedOut, nbEdge, nbIn(i), nbOut(i))).toArray
  private[this] val nbNodes = nodes.length
  private[this] val sharedNodes = Array.fill(nbNodes  + 1)(-1)
  private[this] val nodesLvl = Array.tabulate(arity + 1)(i => new ReversibleSharedSparseSet(context, indexOfNodes.size, nbNodesLvl(i), sharedNodes))
  private[this] val sizeBeforeReset = Array.fill(arity + 1)(0)
  for (i <- indexOfNodes.indices) {
    nodesLvl(indexOfNodes(i)).insert(i)
  }
  
  /* We add the edges in the nodes */
  private[this] val nbEdgesPerLvl = Array.fill(arity)(new ReversibleInt(context, 0))
  edges.zipWithIndex.foreach { case (edge, index) => 
    nodes(edge.start).insertEdgeOut(index)
    nodes(edge.end).insertEdgeIn(index)
    nbEdgesPerLvl(nodes(edge.start).indexVar) += 1
  }
  val maxPossibleValues = (0 until arity).map(valuesMapping(_).size).max
  val maxPossibleNbEdges = (0 until arity).map(nbEdgesPerLvl(_).value).max
  
  /* Some useful functions */
  @inline final def varOfEdge(edge: Int): Int = nodes(edges(edge).start).indexVar
  @inline final def valueOfEdge(edge: Int): Int = edges(edge).value
  @inline final def nbOfValues(indexVar: Int): Int = valuesMapping(indexVar).size
  @inline final def indexOfValue(indexVar: Int, value: Int): Int = valuesMapping(indexVar).index(value)
  @inline final def containsValue(indexVar: Int, value: Int): Boolean = valuesMapping(indexVar).contains(value)
  @inline final def valueForIndex(indexVar: Int, index: Int): Int = valuesMapping(indexVar)(index)
  @inline final def getNbOfEdgesLvl(indexVar: Int): Int = nbEdgesPerLvl(indexVar).value
  @inline final def setNbOfEdgesLvl(indexVar: Int, value: Int): Unit = nbEdgesPerLvl(indexVar).value = value
  
  /*
   * Functions for the MDD4-R algorithm
   */
  
  /**
   * Save the current size of a specific level.
   * @param lvl the level to save.
   */
  @inline final def saveNodesSet(lvl: Int): Unit = sizeBeforeReset(lvl) = nodesLvl(lvl).size
  
  /**
   * Save the current size of a specific level then clear it.
   * @param lvl the level to clear & save.
   */
  @inline final def clearAndSaveNodesSet(lvl: Int): Unit = {
    saveNodesSet(lvl)
    nodesLvl(lvl).clear()
  }
  
  /**
   * Return the number of edges that became invalid on the upper layer by removing a specific edge.
   * @param edge the edge we remove.
   * @return the number of edges invalidated.
   */
  @inline final def resetDeleteEdgeUp(edge: Int): Int = {
    var cptUp = 0
    val eStart = edges(edge).start
    
    if (nodes(eStart).nbOut == 1) {
      cptUp += nodes(eStart).nbIn
      nodesLvl(nodes(eStart).indexVar).remove(eStart)
    }else{
      nodes(eStart).removeEdgeOut(edge)

    }
    cptUp
  }
  
  /**
   * Return the number of edges that became invalid on the lower layer by removing a specific edge.
   * @param edge the edge we remove.
   * @return the number of edges invalidated.
   */
  @inline final def resetDeleteEdgeDown(edge: Int): Int = {
    var cptDown = 0
    val eEnd = edges(edge).end
    
    if (nodes(eEnd).nbIn == 1) {
      cptDown += nodes(eEnd).nbOut
      nodesLvl(nodes(eEnd).indexVar).remove(eEnd)
    }else{
      nodes(eEnd).removeEdgeIn(edge)
    }
    cptDown
  }
  
  /**
   * Fill an array with the ID of the edges that were entering nodes removed from a specific level.
   * @param lvl the level.
   * @param edgesToRemove the array to fill.
   * @return the number of edges added in the array.
   */
  @inline final def resetGetEdgesToDeleteUp(lvl: Int, edgesToRemove: Array[Int]): Int = {
    var size = 0
    var i = nodesLvl(lvl).size
    while (i < sizeBeforeReset(lvl)) {
      val n = nodesLvl(lvl)(i)
      var j = 0
      while (j < nodes(n).nbIn) {
        edgesToRemove(size) = nodes(n).getIn(j)
        size += 1
        j += 1
      }
      i += 1
    }
    size
  }
  
  /**
   * Fill an array with the ID of the edges that were leaving nodes removed from a specific level.
   * @param lvl the level.
   * @param edgesToRemove the array to fill.
   * @return the number of edges added in the array.
   */
  @inline final def resetGetEdgesToDeleteDown(lvl: Int, edgesToRemove: Array[Int]): Int = {
    var size = 0
    var i = nodesLvl(lvl).size
    while (i < sizeBeforeReset(lvl)) {
      val n = nodesLvl(lvl)(i)
      var j = 0
      while (j < nodes(n).nbOut) {
        edgesToRemove(size) = nodes(n).getOut(j)
        size += 1
        j += 1
      }
      i += 1
    }
    size
  }
  
  /**
   * Restore an edge in the associated node it is leaving. If the node was removed, we restore it in its layer.
   * @param edge the edge to restore.
   * @return the number of edges that became valid during the process.
   */
  @inline final def resetRestoreEdgeUp(edge: Int): Int = {
    val eStart = edges(edge).start
    var cptUp = 0
    if (!nodesLvl(nodes(eStart).indexVar).hasValue(eStart)) {
      nodesLvl(nodes(eStart).indexVar).restore(eStart)
      cptUp += nodes(eStart).nbIn
      if(nodes(eStart).nbOut == 1){
        return cptUp
      }
      nodes(eStart).clearEdgesOut()

    }
    nodes(eStart).restoreEdgeOut(edge)
    cptUp
  }
  
  /**
   * Restore an edge in the associated node it is entering. If the node was removed, we restore it in its layer.
   * @param edge the edge to restore.
   * @return the number of edges that became valid during the process.
   */
  @inline final def resetRestoreEdgeDown(edge: Int): Int = {
    val eEnd = edges(edge).end
    var cptDown = 0
    if (!nodesLvl(nodes(eEnd).indexVar).hasValue(eEnd)) {
      nodesLvl(nodes(eEnd).indexVar).restore(eEnd)
      cptDown += nodes(eEnd).nbOut
      if(nodes(eEnd).nbIn == 1){
        return cptDown
      }
      nodes(eEnd).clearEdgesIn()

    }
    nodes(eEnd).restoreEdgeIn(edge)
    cptDown
  }
  
  /**
   * Fill an array with the ID of the edges that are entering valid nodes of a specific level.
   * @param lvl the level.
   * @param edgesToKeep the array to fill.
   * @return the number of edges added in the array.
   */
  @inline final def resetGetEdgesToKeepUp(lvl: Int, edgesToKeep: Array[Int]): Int = {
    var size = 0
    var i = 0
    while (i < nodesLvl(lvl).size) {
      val n = nodesLvl(lvl)(i)
      var j = 0
      while (j < nodes(n).nbIn) {
        edgesToKeep(size) = nodes(n).getIn(j)
        size += 1
        j += 1
      }
      i += 1
    }
    size
  }
  
  /**
   * Fill an array with the ID of the edges that are leaving valid nodes of a specific level.
   * @param lvl the level.
   * @param edgesToKeep the array to fill.
   * @return the number of edges added in the array.
   */
  @inline final def resetGetEdgesToKeepDown(lvl: Int, edgesToKeep: Array[Int]): Int = {
    var size = 0
    var i = 0
    while (i < nodesLvl(lvl).size) {
      val n = nodesLvl(lvl)(i)
      var j = 0
      while (j < nodes(n).nbOut) {
        edgesToKeep(size) = nodes(n).getOut(j)
        size += 1
        j += 1
      }
      i += 1
    }
    size
  }
  
}