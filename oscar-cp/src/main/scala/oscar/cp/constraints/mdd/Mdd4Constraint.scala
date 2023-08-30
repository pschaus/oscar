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
import java.util

import oscar.algo.reversible.{ReversibleContext, ReversibleSharedSparseSet, ReversibleSparseSet}
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar

/**
  * @author rhenneton romain.henneton@hotmail.fr
  */
class Mdd4Constraint(variables : Array[CPIntVar], mdd : StaticMdd, reversibleContext: ReversibleContext) extends Constraint(variables(0).store,"MddGlobal") {

  private[this] val edgesToDeleteStack: util.Stack[Int] = new util.Stack[Int]
  private[this] val nodesToDeleteStack: util.Stack[Int] = new util.Stack[Int]
  private[this] val nEdges = mdd.getNumberOfEdges()
  private[this] val nNodes = mdd.getNumberOfNodes()

  private[this] val nVariables = variables.length

  private[this] val staticDomains : Array[util.TreeSet[Int]] = mdd.staticDomains()
  for(i <- 0 until nVariables){
    for(value <- variables(i).toArray){
      staticDomains(i).add(value)
    }
  }

  private[this] val nodes: Array[ReversibleMddNode] = Array.ofDim(nNodes)
  private[this] val edges: Array[ReversibleMddEdge] = Array.ofDim(nEdges)
  private[this] val nodesMapping: util.HashMap[Long, Int] = mdd.mapNodes(nodes, reversibleContext)
  private[this] val edgesMapping: util.HashMap[Long, Int] = mdd.mapEdgesAndLink(edges, nodes, nodesMapping)
  // first dimension is for the layers, second for the sparse set containing edges in that layer
  private[this] val supports: Array[Array[ReversibleSparseSet]] = setSupports()

  private[this] val edgesValues = Array.ofDim[Int](nEdges)


  /**
    * setup the constraint, typically this is the place where
    * - the constraint registers to modifications of the domains of variables in its scope
    * - a first consistency check and propagation is done
    *
    * @param l the propagation strenght
    * @return The outcome of the first propagation and consistency check
    */
  override def setup(l: CPPropagStrength): Unit = {
    for (variableId <- variables.indices) {
      variables(variableId).callValRemoveIdxWhenValueIsRemoved(this,variableId)
    }
    for (variableId <- 0 until nVariables){
      val domainIterator = staticDomains(variableId).iterator()
      while (domainIterator.hasNext) {
        val value = domainIterator.next()
        if (value >= supports(variableId).length || supports(variableId)(value) == null || supports(variableId)(value).size == 0) {
          variables(variableId).removeValue(value)
        }
        else if (!variables(variableId).hasValue(value)) {
          val edgesToRemove = supports(variableId)(value).toArray
          edgesToRemove.foreach(edgesToDeleteStack.push(_))
        }
      }
    }
    cleanEdgeStack()
  }

  /** Propagation method of Level L1 that is called if variable x has asked to do so
    * with the method call x.callValRemoveIdxWhenValueRemoved(this)
    * @param value is a value that has been removed from the domain of x since last call
    * @param idx is a key value that was given to callValBind(x,idx) attached to variable x.
    *        This is typically used to retrieve the index of x in an array of variables in constant time
    * @return the outcome i.e. Failure, Success or Suspend
    */
  override def valRemoveIdx(x: CPIntVar, idx: Int, value: Int): Unit = {
    nodesToDeleteStack.clear()
    edgesToDeleteStack.clear()

    val valEdges = supports(idx)(value)
    if (valEdges != null) {
      // TODO: fillArray
      val nE = valEdges.fillArray(edgesValues)
      var i = nE
      while (i > 0) {
        i -= 1
        edgesToDeleteStack.push(edgesValues(i))
      }
    }
    cleanEdgeStack()
  }

  def cleanEdgeStack(): Unit = {
    while (!edgesToDeleteStack.isEmpty || !nodesToDeleteStack.isEmpty) {
      // Delete edges
      while (!edgesToDeleteStack.isEmpty) {
        val edgeId = edgesToDeleteStack.pop()
        val edge = edges(edgeId)
        val sup = supports(edge.variableId)(edge.value)
        nodes(edge.topNode).removeOutEdge(edgeId)
        nodes(edge.bottomNode).removeInEdge(edgeId)

        sup.removeValue(edgeId)

        if (sup.size == 0) {
         variables(edge.variableId).removeValue(edge.value)
        }

        if (nodes(edge.topNode).sizeOutEdges == 0) {
          nodesToDeleteStack.push(edge.topNode)
        }
        if (nodes(edge.bottomNode).sizeInEdges == 0) {
          nodesToDeleteStack.push(edge.bottomNode)
        }
      }

      // Delete nodes
      while (!nodesToDeleteStack.isEmpty) {
        val nodeId = nodesToDeleteStack.pop()
        val node = nodes(nodeId)
        if (node.sizeInEdges != 0) {
          var i = 0
          while (i < node.sizeInEdges) {
            edgesToDeleteStack.push(node.getIn(i))
            i += 1
          }
        }
        else if (node.sizeOutEdges != 0) {
          var i = 0
          while (i < node.sizeOutEdges) {
            edgesToDeleteStack.push(node.getOut(i))
            i += 1
          }
        }
      }
    }
  }

  def getNodeById(id: Int): ReversibleMddNode = nodes(id)

  def getEdgeById(id: Int): ReversibleMddEdge = edges(id)

  def setSupports(): Array[Array[ReversibleSparseSet]] = {

    val supSets = Array.fill[java.util.HashMap[Int, util.TreeSet[Int]]](nVariables)(new util.HashMap[Int,util.TreeSet[Int]]())

    for(edge <- edges){
      val varId = edge.variableId
      val value = edge.value
      if(!supSets(varId).containsKey(value)){
        supSets(varId).put(value,new util.TreeSet[Int]())
      }
      supSets(varId).get(value).add(edge.id)
    }

    val nValues = staticDomains.map(_.last()).max + 1

    val outputSupportSet = Array.ofDim[ReversibleSparseSet](nVariables,nValues)

    //println("nVars:"+nVariables+" nvalues:"+nValues)

    for (varId <- 0 until nVariables) {
      val valueIterator = supSets(varId).keySet().iterator()
      while (valueIterator.hasNext) {
        val value = valueIterator.next()
        val set = supSets(varId).get(value)
        val sparseSet = new ReversibleSparseSet(reversibleContext, set.first(), set.last())
        for (i <- set.first() + 1 until set.last()) {
          if (!set.contains(i)) sparseSet.removeValue(i)
        }
        if (value < nValues) outputSupportSet(varId)(value) = sparseSet
      }
    }
    outputSupportSet
  }

  def associatedVars(): Iterable[CPVar] = variables

}

class ReversibleMddNode(val id: Int, val layer : Int, reversibleContext: ReversibleContext, mapIdToIn: Array[Int], mapIdToOut: Array[Int]) {

  private[this] val staticInEdgesSet = new util.TreeSet[Int]()
  private[this] val staticOutEdgesSet = new util.TreeSet[Int]()

  private[this] var inEdges : ReversibleSharedSparseSet = _
  private[this] var outEdges : ReversibleSharedSparseSet = _

  private[this] var mappingInToId : Array[Int] = _

  private[this] var mappingOutToId : Array[Int] = _

  def createReversibleStructures() : Unit = {

    if(staticInEdgesSet.size() > 0) inEdges = new ReversibleSharedSparseSet(reversibleContext,staticInEdgesSet.size)
    else inEdges = new ReversibleSharedSparseSet(reversibleContext,0)

    mappingInToId = Array.ofDim(staticInEdgesSet.size)
    var i = 0
    var iterator = staticInEdgesSet.iterator()

    while(iterator.hasNext){
      val value = iterator.next()
      mappingInToId(i) = value
      mapIdToIn(value) = i
      inEdges.insert(i)
      i += 1
    }

    if(staticOutEdgesSet.size() >0) outEdges = new ReversibleSharedSparseSet(reversibleContext,staticOutEdgesSet.size)
    else outEdges = new ReversibleSharedSparseSet(reversibleContext,0)

    mappingOutToId = Array.ofDim(staticOutEdgesSet.size)
    i = 0
    iterator = staticOutEdgesSet.iterator()

    while(iterator.hasNext){
      val value = iterator.next()
      mappingOutToId(i) = value
      mapIdToOut(value) = i
      outEdges.insert(i)
      i += 1
    }
  }

  def getIn(pos : Int) : Int = mappingInToId(inEdges(pos))

  def getOut(pos : Int) : Int = mappingOutToId(outEdges(pos))

  def addInEdge(edgeId: Int) = staticInEdgesSet.add(edgeId)

  def addOutEdge(edgeId: Int) = staticOutEdgesSet.add(edgeId)

  def reAddInEdge(edgeId : Int) = inEdges.restore(mapIdToIn(edgeId))

  def reAddOutEdge(edgeId : Int) = outEdges.restore(mapIdToOut(edgeId))

  def removeInEdge(edgeId: Int) = if(inEdges.hasValue(mapIdToIn(edgeId))) inEdges.remove(mapIdToIn(edgeId))

  def removeOutEdge(edgeId: Int) = if(outEdges.hasValue(mapIdToOut(edgeId))) outEdges.remove(mapIdToOut(edgeId))

  def clearOutEdges() : Unit = outEdges.clear()

  def clearInEdges() : Unit = inEdges.clear()

  def sizeInEdges: Int = inEdges.size

  def sizeOutEdges: Int = outEdges.size


}

class ReversibleMddEdge(val id: Int, val variableId: Int, val value: Int, val topNode: Int, val bottomNode: Int)