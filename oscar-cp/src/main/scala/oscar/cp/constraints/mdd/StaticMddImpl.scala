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


package oscar.cp.constraints.mdd


import java.math.BigInteger
import java.util
import java.util.{Collections, Comparator}

import oscar.algo.reversible.ReversibleContext


/**
  * Class representing a static multivalued decision diagram.
  * On this object, we can :
  *   - Add mdd-constraints
  *   - Add a custom tuple
  *   - Remove a custom tuple
  *   - Semi-reduce the mdd in a bottom up way
  *   - Define the heuristic for the refinement process
  *   - Refine (split and prune) based on the constraints that were added previously
  *
  * @author rhenneton romain.henneton@hotmail.fr
  *
  * @param arity : number of layer of edges in the mdd
  */
class StaticMddImpl(val arity: Int) extends StaticMdd {


  /**
    * Basic structure of the mdd : a root, an end node, and layers stored as an array of Sets of nodes
    */
  val layers = Array.fill[java.util.TreeSet[StaticMddNode]](arity + 1)(new util.TreeSet[StaticMddNode]())
  val root: StaticMddNode = new StaticMddNode(0)
  val end: StaticMddNode = new StaticMddNode(arity)
  layers(0).add(root)
  layers(arity).add(end)

  /**
    * tableHashes is used for the reduction (and therefore incremental addition/deletion of tuples)
    * constraints : List of all the static constraints added to this MDD
    * splittedHeuristic : list of the closures used to sort the splitted nodes of a layer and decide greedily which one should be splitted first
    */
  private[this] val tableHashes = Array.fill[java.util.TreeMap[String, StaticMddNode]](arity)(new util.TreeMap[String, StaticMddNode]())
  private[this] val constraints = new util.ArrayList[StaticMddConstraint]()
  private[this] var splittedHeuristic = scala.collection.mutable.ArrayBuffer[(Int, (StaticMddSplittedNode, StaticMddSplittedNode) => Int)]()
  // Add basic heuristic : -10000 uses the ID (very poor), 0 used the number of outEdges (less outEdges first), and you can add your own heuristic
  splittedHeuristic += (-10000,(a : StaticMddSplittedNode, b: StaticMddSplittedNode) => {
    if(a.getId < b.getId) 1
    else if(a.getId == b.getId) 0
    else -1
  })

  splittedHeuristic += (0, (a:StaticMddSplittedNode, b: StaticMddSplittedNode) =>{
    if(a.getOutEdges().size < b.getOutEdges().size) -1
    else if(a.getOutEdges().size == b.getOutEdges().size) 0
    else 1
  })

  /**
    * During the refinement process, we split the nodes. In fact, we extract one in-edge from a node. This is done
    * greedily according to several heuristics. Those heuristics can be customized with this function. The higher
    * the priority, the bigger importance it will have on the extraction decision
    * @param priority : The priority associated with the heuristic. Basic highest priority is 0 and lowest is -10000
    * @param heuristic : Heuristic used to decide which splitted node has to be extracted first
    */
  override def addComparator(priority: Int, heuristic: (StaticMddSplittedNode, StaticMddSplittedNode) => Int): Unit = splittedHeuristic += ((priority,heuristic))

  /**
    * Structures used for the pruning, refinement
    * deadEnds : all the active nodes that have no more out edges (and need to be deleted)
    * nodeWithoutInEdge : same than deadEnds, but they don't have any in-edges
    * reduced : This is used for incremental tuple addition. Set to true when the mdd is semi-reduced (allow
    * to efficiently add or remove custom tuples from the mdd)
    * stillNeedToPrune : Boolean used for the fixed point during the refinement
    */
  private[this] val deadEnds : util.Stack[StaticMddNode] = new util.Stack[StaticMddNode]()
  private[this] val nodeWithoutInEdge : util.Stack[StaticMddNode] = new util.Stack[StaticMddNode]()
  private[this] val edgesToDelete : util.Stack[StaticMddEdge]= new util.Stack[StaticMddEdge]()
  private[this] var reduced : Boolean = false
  private[this] var stillNeedToPrune : Boolean = true

  def pushDeadEnd(node : StaticMddNode) : Unit = this.deadEnds.push(node)
  /**
    * Apply the refinement algorithm on the decision diagram : "Split and prune" iterative process based on the
    * constraints that were applied to the MDD
    * @param maxWidth
    */
  override def refine(maxWidth: Int): Unit = {
    // Sort by decreasing order of priority
    splittedHeuristic = splittedHeuristic.sortBy(-_._1)

    reduced = false
    stillNeedToPrune = true
    while (stillNeedToPrune) {
      stillNeedToPrune = false
      recomputeAllStates()

      for (layerN <- 0 to arity) {
        val splittedList = new util.ArrayList[StaticMddSplittedNode]()

        val layer = layers(layerN)
        val nodesIterator = layer.iterator()

        /** Phase 1 **/
        while (nodesIterator.hasNext) {
          val node = nodesIterator.next()
          val outEdgesIterator = node.getOutEdgeIterator()
          while (outEdgesIterator.hasNext) {
            val outEdge = outEdgesIterator.next()
            var pruned = false
            var i = 0
            while (i < constraints.size() && !pruned) {
              if (constraints.get(i).shouldDeleteEdge(outEdge)) {
                pruned = true
                edgesToDelete.push(outEdge)
              }
              i += 1
            }
          }
        }
        deleteEdgesAndPropagate()

        val nodesIterator2 = layer.iterator()
        while (nodesIterator2.hasNext) {
          val node = nodesIterator2.next()
          /** Phase 2 **/
          val inEdgesIterator = node.getInEdgeIterator()
          while (inEdgesIterator.hasNext) {
            val inEdge = inEdgesIterator.next()
            val splitted_Node = new StaticMddSplittedNode(inEdge, node)
            for (i <- 0 until constraints.size()) {
              constraints.get(i).pruneSplittedNode(splitted_Node)
            }
            if (splitted_Node.getOutEdges().size() == 0 && splitted_Node.originalNode.layer != arity) {
              edgesToDelete.push(inEdge)
            }
            else if (splitted_Node.originalNode.layer != arity) splittedList.add(splitted_Node)
          }

        }
        deleteEdgesAndPropagate()

        // sort the in decreasing order
        Collections.sort(splittedList, new Comparator[StaticMddSplittedNode] {
          override def compare(t: StaticMddSplittedNode, t1: StaticMddSplittedNode): Int = {
            var i = 0
            while(i<splittedHeuristic.size){
              val rez = splittedHeuristic(i)._2(t,t1)
              if(rez >0) return 1
              else if(rez <0) return -1
              i+= 1
            }
            return 0
          }
        })

        /** Phase 3 **/
        var i = 0
        while (i < splittedList.size() && layer.size() < maxWidth) {
          val splitted_Node = splittedList.get(i)
          if (splitted_Node.originalNode.getInSize() > 1) {
            // Do the splitting (isolate in edge )
            val oldInEdge = splitted_Node.inEdge
            oldInEdge.unlink()
            val newNode = new StaticMddNode(splitted_Node.originalNode.layer)
            layers(newNode.layer).add(newNode)
            val newInEdge = new StaticMddEdge(oldInEdge.topNode, newNode, oldInEdge.value)
            // Now add the outEdges (that are still valid for this splittedNode)
            val outIterator = splitted_Node.getOutEdges().keySet().iterator()
            while (outIterator.hasNext) {
              val outVal = outIterator.next()
              val destNode = splitted_Node.getOutEdges().get(outVal)
              val newOut = new StaticMddEdge(newNode, destNode, outVal)
            }
            for (j <- 0 until constraints.size()) {
              constraints.get(j).computeDownStates(newNode)
            }
          }
          i += 1
        }

        /** Phase 4 : recompute all downStates & reprune out Edges :( **/
        val lastNodesIterator = layer.iterator()
        while (lastNodesIterator.hasNext) {
          val node = lastNodesIterator.next()
          for (i <- 0 until constraints.size()) {
            constraints.get(i).computeDownStates(node)
          }
          val outEdgesIterator = node.getOutEdgeIterator()
          while (outEdgesIterator.hasNext) {
            val outEdge = outEdgesIterator.next()
            var pruned = false
            for (i <- 0 until constraints.size(); if !pruned) {
              if (constraints.get(i).shouldDeleteEdge(outEdge)) {
                pruned = true
                edgesToDelete.push(outEdge)
              }
            }
          }
        }
        deleteEdgesAndPropagate()
      }
    }
  }

  /**
    * Compute the number of possible paths from root to end node.
    * As this could grow exponentially, we use java BigInteger
    */
  override def numPaths(): java.math.BigInteger = dfsComputation(this.root,new util.HashMap[StaticMddNode,java.math.BigInteger]())


  /**
    * @return : The arity of the considered mdd (number of layer of edges)
    */
  override def getArity(): Int = this.arity

  /**
    * This helper function computes de number of paths with a dynamic programming algorithm (number of paths in a DAG)
    *
    * @param node
    * @param cache
    * @return
    */
  private def dfsComputation(node : StaticMddNode, cache : java.util.HashMap[StaticMddNode, java.math.BigInteger]) : java.math.BigInteger = {
    val rez = cache.get(node)
    if(rez != null) return rez
    if(node.equals(this.end)) return new BigInteger("1")

    var cum = new BigInteger("0")
    val outEdgesIterator = node.getOutEdgeIterator()
    while(outEdgesIterator.hasNext) cum = cum.add(dfsComputation(outEdgesIterator.next().bottomNode,cache))
    cache.put(node,cum)
    return cum
  }

  /**
    * Reset the states of all constraints, performs a top down pass then a bottom up pass to recompute the states of
    * every constraint
    */
  def recomputeAllStates(): Unit = {
    var j = 0

    /** Clear all constraints states **/
    while (j < constraints.size()) {
      constraints.get(j).clearStates()
      j += 1
    }

    /** Top-down pass **/
    var i = 0
    while (i <= arity) {
      val layer = layers(i)
      val nodeIterator = layer.iterator()
      while (nodeIterator.hasNext) {
        val node = nodeIterator.next()
        j = 0
        while (j < constraints.size()) {
          constraints.get(j).computeDownStates(node)
          j += 1
        }
      }
      i += 1
    }

    /** Bottom-up pass **/
    i = arity
    while (i >= 0) {
      val layer = layers(i)
      val nodeIterator = layer.iterator()
      while (nodeIterator.hasNext) {
        val node = nodeIterator.next()
        j = 0
        while (j < constraints.size()) {
          constraints.get(j).computeUpStates(node)
          j += 1
        }
      }
      i -= 1
    }
  }

  /**
    * Fixed-point to delete all the invalidated edges and nodes (dead ends and node without any out edge);
    * This is a fixed point because deleting edges can lead to invalidation of nodes which can invalidate edges,...
    * The algorithm is applied until everything inside the mdd is "valid"
    */
  def deleteEdgesAndPropagate(): Unit = {
    while (!edgesToDelete.isEmpty() || !nodeWithoutInEdge.isEmpty() || !deadEnds.isEmpty()) {
      /** First, clear edges and add to nodeWithout or deadEnds **/
      while (!edgesToDelete.isEmpty) {
        stillNeedToPrune = true
        val toDelete = edgesToDelete.pop()
        toDelete.unlink()
        if (toDelete.bottomNode.getInSize == 0 && toDelete.bottomNode.getOutSize() != 0) {
          nodeWithoutInEdge.add(toDelete.bottomNode)
        }
        if (toDelete.topNode.getOutSize() == 0 && toDelete.topNode.getInSize() != 0) {
          deadEnds.add(toDelete.topNode)
        }

        if (toDelete.topNode.getOutSize() == 0 && toDelete.bottomNode.getInSize() == 0) {
          layers(toDelete.topNode.layer).remove(toDelete.topNode)
        }
        if (toDelete.bottomNode.getOutSize() == 0 && toDelete.bottomNode.getInSize() == 0) {
          layers(toDelete.bottomNode.layer).remove(toDelete.bottomNode)
        }

      }


      /** Clear dead ends **/
      while (!deadEnds.isEmpty) {
        val deadEnd = deadEnds.pop()
        val edgesIterator = deadEnd.getInEdgeIterator()
        while (edgesIterator.hasNext) {
          edgesToDelete.add(edgesIterator.next())
        }
        deadEnd.clearInEdges()
        layers(deadEnd.layer).remove(deadEnd)
      }

      /** Clear node without in edge **/
      while (!nodeWithoutInEdge.isEmpty) {
        val unreachable = nodeWithoutInEdge.pop()
        val edgesIterator = unreachable.getOutEdgeIterator()
        while (edgesIterator.hasNext) {
          edgesToDelete.add(edgesIterator.next())
        }
        unreachable.clearOutEdges()
        layers(unreachable.layer).remove(unreachable)
      }
    }
  }

  /**
    * Add a tuple to the existing mdd
    * @param tuple
    */
  override def addTuple(tuple: Array[Int]): Unit = {
    if(this.contains(tuple)) return
    if(!reduced) reduce()

    val modifiedNode = Array.fill[util.HashSet[StaticMddNode]](arity+1)(new util.HashSet[StaticMddNode]())
    val stringHashToRemove = Array.fill[util.ArrayList[String]](arity+1)(new util.ArrayList[String]())

    /**
      * Step 1 : Isolate the tuple
      */
    var alreadySplitted = false
    var i = 0
    var curNode = root
    var deadEndReached = false
    while (i < arity - 1 && ! deadEndReached) {
      if(curNode.containsOutValue(tuple(i))) {
        if (!alreadySplitted && curNode.getOutEdge(tuple(i)).bottomNode.getInSize() == 1) {
          curNode = curNode.getOutEdge(tuple(i)).bottomNode
          modifiedNode(curNode.layer).add(curNode)
        }
        else {
          modifiedNode(curNode.layer).add(curNode)
          stringHashToRemove(curNode.layer).add(curNode.strHash())

          val newNode = new StaticMddNode(i + 1)
          modifiedNode(newNode.layer).add(newNode)
          layers(i + 1).add(newNode)
          val outEdge = curNode.getOutEdge(tuple(i))
          outEdge.unlink()
          val newInEdge = new StaticMddEdge(curNode, newNode, tuple(i))
          val outIterator = outEdge.bottomNode.getOutEdgeIterator()
          while (outIterator.hasNext) {
            val outToCopy = outIterator.next()
            val copiedEdge = new StaticMddEdge(newNode, outToCopy.bottomNode, outToCopy.value)
          }
          curNode = newNode
          alreadySplitted = true
        }
        i += 1
      }
      else{
        deadEndReached = true
      }
    }

    /** Step 2 : Create new path
      * We start from curNode and add edges until the end
      */
    while(i<arity-1){
      val newNode = new StaticMddNode(i+1)
      layers(i+1).add(newNode)
      val newEdge = new StaticMddEdge(curNode,newNode,tuple(i))
      curNode = newNode
      modifiedNode(i+1).add(newNode)
      i += 1
    }
    val lastEdge = new StaticMddEdge(curNode,end,tuple(i))
    /** Step 3 : Incremental reduce **/
    pReduce(modifiedNode,stringHashToRemove)

  }


  /**
    * Remove a tuple (if present) from the MDD
    * @param tuple
    */
  override def removeTuple(tuple: Array[Int]): Unit = {
    if(! this.contains(tuple)) return

    val modifiedNode = Array.fill[util.HashSet[StaticMddNode]](arity+1)(new util.HashSet[StaticMddNode]())
    val stringHashToRemove = Array.fill[util.ArrayList[String]](arity+1)(new util.ArrayList[String]())

    // First, check that the hashMap of destination -> node is properly built
    if (!reduced) reduce()

    /**
      * Step 1 : Isolate the tuple
      */
    var alreadySplitted = false
    var i = 0
    var curNode = root
    while (i < arity - 1) {
      if (!alreadySplitted && curNode.getOutEdge(tuple(i)).bottomNode.getInSize() == 1) {
        curNode = curNode.getOutEdge(tuple(i)).bottomNode
        modifiedNode(curNode.layer).add(curNode)
      }
      else {
        modifiedNode(curNode.layer).add(curNode)
        if(!alreadySplitted) {
          stringHashToRemove(curNode.layer).add(curNode.strHash())
        }
        val newNode = new StaticMddNode(i + 1)
        modifiedNode(newNode.layer).add(newNode)
        layers(i + 1).add(newNode)
        val outEdge = curNode.getOutEdge(tuple(i))
        outEdge.unlink()
        val newInEdge = new StaticMddEdge(curNode, newNode, tuple(i))
        val outIterator = outEdge.bottomNode.getOutEdgeIterator()
        while (outIterator.hasNext) {
          val outToCopy = outIterator.next()
          val copiedEdge = new StaticMddEdge(newNode, outToCopy.bottomNode, outToCopy.value)
        }
        curNode = newNode
        alreadySplitted = true
      }
      i += 1
    }

    /** Step 2, delete last edge and up-propagate. Note that as there is only one tuple on the path, only one incomming edge per node **/
    val outEdge = curNode.getOutEdge(tuple(i))
    outEdge.unlink()
    while(curNode.getOutSize() == 0 && curNode.layer != 0){
      val inEdge = curNode.peekFirstInEdge()
      inEdge.unlink()
      layers(curNode.layer).remove(curNode)
      curNode = inEdge.topNode
    }
    /** Step 3 : The reduction **/
    pReduce(modifiedNode,stringHashToRemove)
  }

  /**
    * Application of the bottom up reduction only on modified nodes (and not on the whole mdd).
    * This method is used for the addition/removal of tuples inside the mdd
    * @param modifiedNode
    * @param stringHashToRemove
    */
  private def pReduce(modifiedNode : Array[util.HashSet[StaticMddNode]], stringHashToRemove : Array[util.ArrayList[String]]) : Unit = {
    // First, removeHashes
    for(i <- 0 to arity){
      for(j <- 0 until stringHashToRemove(i).size()){
        tableHashes(i).remove(stringHashToRemove(i).get(j))
      }
    }
    // Then, bottom up merging
    for(i <- arity to 0 by -1){
      val iterator = modifiedNode(i).iterator()
      while(iterator.hasNext){
        val node = iterator.next()
        if(layers(i).contains(node)) {
          val nodeHash = node.strHash()
          if (tableHashes(i).containsKey(nodeHash) && tableHashes(i).get(nodeHash).getId() != node.getId()) {
            tableHashes(i).get(nodeHash).merge(node)
            layers(i).remove(node)
          }
          else {
            tableHashes(i).put(nodeHash, node)
          }
        }
      }
    }
  }

  /**
    * Check if a tuple is a path inside the MDD
    * @param tuple
    * @return
    */
  override def contains(tuple : Array[Int]) : Boolean = {
    if(tuple.length != arity) return false
    var j = 0
    var cur = root
    while(j<arity){
      val outE = cur.getOutEdge(tuple(j))
      if(outE == null) return false
      cur = outE.bottomNode
      j += 1
    }
    return true
  }

  /**
    * Bottom up reduction used in the creation of the table mdd and the regular constraint.
    */
  override def reduce(): Unit = {
    reduced = true
    for (map <- tableHashes) map.clear()

    /**
      * In a bottom up way, merge the nodes from a similar layer
      */
    for (level <- arity - 1 to 1 by -1) {
      val levelMap = tableHashes(level)
      val levelLayer = layers(level)

      val nodeIterator = levelLayer.iterator()
      while (nodeIterator.hasNext) {
        val curNode = nodeIterator.next()
        val hash = curNode.strHash()
        if (!levelMap.containsKey(hash)) {
          levelMap.put(hash, curNode)
        }
        else {
          levelMap.get(hash).merge(curNode)
        }
      }

      /**
        * All the merge have been done, now, clear the layer and put only the remaining nodes
        */
      levelLayer.clear()
      val valuesIterator = levelMap.values().iterator()
      while (valuesIterator.hasNext) levelLayer.add(valuesIterator.next())
    }
  }

  /**
    * Add a static constraint to the MDD constraints. Those will be used with the refine/prune function to
    * restrict the paths in the mdd to represent "as much as possible" only paths that meets the constraints
    * @param cstr
    */
  override def addConstraint(cstr: StaticMddConstraint): Unit = constraints.add(cstr)

  /**
    * Returns the domains of each layer of the static mdd
    */
  override def staticDomains(): Array[util.TreeSet[Int]] = {
    val domains = Array.fill[util.TreeSet[Int]](arity)(new util.TreeSet[Int]())
    var i = 0
    while (i < arity) {
      val layer = layers(i)
      val nodeIterator = layer.iterator()
      while (nodeIterator.hasNext) {
        val node = nodeIterator.next()
        val edgeValuesIterator = node.getOutValuesIterator()
        while (edgeValuesIterator.hasNext) domains(i).add(edgeValuesIterator.next())
      }
      i += 1
    }
    domains
  }

  override def getNumberOfEdges(): Int = {
    var tot = 0
    for (i <- 0 until arity) {
      val nodeIterator = layers(i).iterator()
      while (nodeIterator.hasNext) tot += nodeIterator.next().getOutSize()
    }
    tot
  }

  override def getNumberOfNodes(): Int = {
    var tot = 0
    for (i <- 0 to arity) {
      tot += layers(i).size()
    }
    tot
  }

  /**
    * Map the nodes of the static mdd to a new numbering inside the reversible MDD. If the number of nodes is n, the
    * new id's of the reversibleNodes will be 0,1,..., n-1. Return the node mapping (id in static mdd -> id in reversibleMdd)
    * @param nodes
    * @param reversibleContext
    * @return nodesMapping
    */
  override def mapNodes(nodes: Array[ReversibleMddNode], reversibleContext: ReversibleContext): util.HashMap[Long, Int] = {
    val map = new util.HashMap[Long, Int]()
    var i = 0
    val nE = getNumberOfEdges()
    val edgeIdsIn = Array.ofDim[Int](nE)
    val edgeIdsOut = Array.ofDim[Int](nE)
    for (set <- layers) {
      val nodeIterator = set.iterator()
      while (nodeIterator.hasNext) {
        val node = nodeIterator.next()
        map.put(node.getId(), i)
        nodes(i) = new ReversibleMddNode(i, node.layer, reversibleContext, edgeIdsIn, edgeIdsOut)
        i += 1
      }
    }
    map
  }

  /**
    * Map the static edges to a new numbering of the id's inside the reversible MDD. If the number of edges is n, the
    * new id's of the reversibleEdges will be 0,1,..., n-1. Retrn the edges mapping (id in the static mdd -> id in reversibleMdd)
    * @param edges
    * @param nodes
    * @param nodesMapping
    * @return edgesMapping
    */
  override def mapEdgesAndLink(edges: Array[ReversibleMddEdge], nodes: Array[ReversibleMddNode], nodesMapping: util.HashMap[Long, Int]): util.HashMap[Long, Int] = {
    val map = new util.HashMap[Long, Int]()
    var i = 0
    var layer = 0
    for (set <- layers) {
      val nodeIterator = set.iterator()
      while (nodeIterator.hasNext) {
        val node = nodeIterator.next()
        val edgesIterator = node.getOutEdgeIterator()
        while (edgesIterator.hasNext) {
          val edge = edgesIterator.next()
          map.put(edge.getId, i)
          val topNodeId = nodesMapping.get(edge.topNode.getId())
          val botNodeId = nodesMapping.get(edge.bottomNode.getId())
          edges(i) = new ReversibleMddEdge(i, layer, edge.value, topNodeId, botNodeId)
          nodes(topNodeId).addOutEdge(i)
          nodes(botNodeId).addInEdge(i)
          i += 1
        }
      }
      layer += 1
    }
    nodes.foreach(_.createReversibleStructures)
    map
  }


  def createNode(layer: Int): StaticMddNode = {
    val node = new StaticMddNode(layer)
    layers(layer).add(node)
    node
  }


  def addEdge(fromNode: StaticMddNode, toNode: StaticMddNode, value: Int): StaticMddEdge = {
    val edge = new StaticMddEdge(fromNode,toNode,value)
    edge
  }


}