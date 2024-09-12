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


package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core._
import oscar.cp.core.variables.{CPGraphVar, CPVar}

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Defines the constraint Bipartite(G)
 * 	this constraint holds if the graph is bipartite
 */

class GraphBipartite(val g : CPGraphVar) extends Constraint(g.s, "Bipartite") {

	override def associatedVars(): Iterable[CPVar] = Array(g)

	override def setup(l: CPPropagStrength): Unit = {
	  // add filter when domain changes
	  g.callPropagateWhenDomainChanges(this)
	  propagate()
	}
	
	override def propagate(): Unit = {
	  // #1 If required domain of G is not bipartite, the constraint fails
	  // #2 If possible domain of G is bipartite, the constraint is entailed
	  // #3 Pruning : remove possible element of the domain of G
	  //         which would create an odd-length cycle
	  
	  // compute required components list to prune each component independently
	  val reqNodes : List[Int] = g.requiredNodes
	  val requiredComponents : List[List[Int]] = requiredConnectedComponents(reqNodes)
	  
	  // #1 If required domain of G is not bipartite, the constraint fails
	  // Go trough all required elements of G using DFS and color nodes by alternating between two sets
	  //   coloring nodes is equal to set them into two different sets
	  // we will have coloring for each component
	  var color1 : List[List[Int]] = List(List())
	  var color2 : List[List[Int]] = List(List())
	  val requiredEdges : List[Int]= reqNodes.flatMap(g.requiredOutEdges(_))
	  for(e <- requiredEdges){
	    val (src,dest) = g.edge(e)
	    val componentIdx = requiredComponents.indexWhere(l => l.contains(src))
	    
	    // append new list to color new component if needed
	    if (componentIdx >= color1.length) color1 = color1 ::: List(List())
	    if (componentIdx >= color2.length) color2 = color2 ::: List(List())

	    // add each endPoint to one color list
	    if (color1(componentIdx).contains(src)){
	      color2 = addElementInIdx(dest, componentIdx, color2)
	    } else {
	      if (color2(componentIdx).contains(src)) {
	        color1 = addElementInIdx(dest, componentIdx, color1)
	      } else { // src has no color yet
	        if (color1(componentIdx).contains(dest)){
	          color2 = addElementInIdx(src, componentIdx, color2)
	        } else {
	          color1 = addElementInIdx(src, componentIdx, color1)
	          color2 = addElementInIdx(dest, componentIdx, color2)
	        }
	      }
	    }
	  }
	  // remove duplicates
	  color1 = color1.map(_.distinct)
	  color2 = color2.map(_.distinct)
	    
	  // if there is one node in both colors, failure
	  for (i <- 0 to color1.length-1; if ( color1(i).exists(color2(i).contains(_)) ) ) throw Inconsistency
	  
	  // #2 If possible domain of G is bipartite, the constraint is entailed
	  // we will add all nodes having possible edges into two sets 
	  //    if sets are bipartite -> constraints is entailed
	  val possibleEdges : List[Int]= g.possibleNodes.flatMap(g.possibleOutEdges(_))
	  var possibleColor1 : List[Int] = List()
	  var possibleCcolor2 : List[Int] = List()
	  for(e <- possibleEdges){
	    val (src,dest) = g.edge(e)
	    // add each endPoint to one color list
	    if (possibleColor1.contains(src)){
	      possibleCcolor2 = dest :: possibleCcolor2
	    } else {
	      if (possibleCcolor2.contains(src)) {
	        possibleColor1 = dest :: possibleColor1
	      } else { // src has no color yet
	        if (possibleColor1.contains(dest)){
	          possibleCcolor2 = src ::  possibleCcolor2
	        } else {
	          possibleColor1 = src ::  possibleColor1
	          possibleCcolor2 = dest :: possibleCcolor2
	        }
	      }
	    }
	  }
	  possibleColor1 = possibleColor1.distinct
	  possibleCcolor2 = possibleCcolor2.distinct
	  // if there is one node in both colors, failure
	  if ( !possibleColor1.exists(possibleCcolor2.contains(_)) ) {
			deactivate()
			return
		}
	  
	  
	  // #3 Pruning : for each component, 
	  //      remove possible element of the domain of G which could create an odd-length cycle
	  for (e <- possibleEdges){
	    val (src,dest) = g.edge(e)
	    val compIdx1 = requiredComponents.indexWhere(l => l.contains(src))
	    val compIdx2 = requiredComponents.indexWhere(l => l.contains(dest))
	    // if the edge is connected to two required nodes from the same connected component
	    if (compIdx1 == compIdx2 && compIdx1 != -1){
	      if (  (color1(compIdx1).contains(src) && color1(compIdx1).contains(dest))
	         || (color2(compIdx1).contains(src) && color2(compIdx1).contains(dest)) ) {
	        // edge has same color at both endPoints -> remove it
	        g.removeEdgeFromGraph(src, dest)
	      }
	    }
	  }
	}
    
    /**
     * @param reqNodes Nodes to check
     * @return a list of list of integer, each inner list contains one required connected component
     */
    private def requiredConnectedComponents(reqNodes : List[Int]) : List[List[Int]] = {
	  def requiredConnectedNeighbors(nodeId : Int) : List[Int] = 
      reqNodes.filter(n => (nodeId != n) && 
        (!g.requiredEdges(nodeId).map(idx => g.edge(idx)).filter(e => e._2 == n).isEmpty || 
         !g.requiredEdges(n).map(idx => g.edge(idx)).filter(e => e._2 == nodeId).isEmpty) )
      def connectedNodesList(neighborsNotVisited : List[Int], connectedNodesAcc : List[Int]) : List[Int] = {
        neighborsNotVisited match {
          case Nil => connectedNodesAcc.sorted
          case node :: t => {
            val neighbors : List[Int] = requiredConnectedNeighbors(node) 
            val newNeighbors : List[Int] = neighbors.filter(elem => !(node :: connectedNodesAcc).contains(elem) )
            connectedNodesList((t ++ newNeighbors).distinct, node :: connectedNodesAcc)
          }
        }
      }
	  def possibleConnectedComponents(reqNodes : List[Int]) : List[List[Int]] = {
        var visitedNodes : List[Int] = List()
        var componentsList : List[List[Int]] = Nil
        for (n <- reqNodes) {
          if (!visitedNodes.contains(n)) {
            val connectedNodes : List[Int] = connectedNodesList(List(n),List())
            visitedNodes = visitedNodes ++ connectedNodes
            componentsList = connectedNodes :: componentsList
          }
        }
        componentsList
      }
      possibleConnectedComponents(reqNodes)
    }
    
    private def addElementInIdx(elem : Int, idx : Int, color : List[List[Int]]) : List[List[Int]] =
      color.take(idx) ::: List(elem :: color(idx)) ::: color.takeRight(color.length - idx -1)
	
}