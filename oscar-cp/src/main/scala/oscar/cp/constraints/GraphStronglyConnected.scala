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
 * Defines the constraint Strongly Connected(G) for a directed graph 
 */

class GraphStronglyConnected(val g : CPGraphVar) extends Constraint(g.s, "Strongly Connected") {

  override def associatedVars(): Iterable[CPVar] = Array(g)

	override def setup(l: CPPropagStrength): Unit = {
	  // add filter when domain changes
	  g.callPropagateWhenDomainChanges(this)
	  propagate()
	}
	
	override def propagate(): Unit = {
	  // #1 If required D(G) != empty and possible D(G) contains more than one connected component, 
	  //	   remove all but the connected component containing required D(G). 
	  // #2 Then all cutnodes and bridges on a path between two nodes of required D(G) are included in required D(G)

	  val reqNodes : List[Int] = g.requiredNodes
	  val possNodes : List[Int] = g.possibleNodes
	  if (!reqNodes.isEmpty){
	    // we have to compute connected components
	    val connectedComponents : Array[List[Int]] = stronglyConnectedComponents(possNodes).toArray
	    
	    // #1
	    if (connectedComponents.size > 1) {
	      // check whether if two required nodes belongs to two different components -> Failure
	      val componentNumber : Int = connectedComponents.indexWhere(l => l.contains(reqNodes.head))
	      for ( n <- reqNodes.tail)
	        if (! connectedComponents(componentNumber).contains(n)) throw Inconsistency
	      // remove all but the connected component containing required D(G)
	      for (n <- possNodes)
	        if (! connectedComponents(componentNumber).contains(n))
	          g.removeNodeFromGraph(n)
	    }
	    
	    // #2
	  	// add in required D(G) all cutnodes and bridges on a path between two nodes of required D(G)
	    val newPossNodes = g.possibleNodes
	    val possNotReqNodes : List[Int] = newPossNodes.filter(!reqNodes.contains(_))
	    var cutnodes : List[Int] = List()
	  	for (n1 <- reqNodes; n2 <- reqNodes; if n1!=n2) {
	  	  // check for all possible nodes not in reqNodes nor already cutnots that its removal would disconnect n1 from n2
	  	  val nodesToCheck = newPossNodes.filter(!cutnodes.contains(_))
	  	  for (n <- nodesToCheck; if ! existPath(n1, n2, n)){
	  	    // n is a cutnode
	  	    cutnodes = n :: cutnodes
	  	    g.addNodeToGraph(n)
	  	  }
	  	}
	    // check for bridge : if an edge removal would disconnect two required nodes, its a bridge
	    val possEdges = newPossNodes.flatMap(g.possibleOutEdges(_)).map(g.edge(_))
	    val newReqNodes : List[Int] = g.requiredNodes
	    var added : List[(Int,Int)] = List()
	    for (n1 <- newReqNodes; n2 <- newReqNodes; if n1!=n2){
	      val edgesToCheck = possEdges.filter(!added.contains(_))
	      for (e <- edgesToCheck; if !existPath(n1, n2, e)){
	        added = e :: added
	        g.addEdgeToGraph(e._1,e._2)
	      }      
	    }
	  }
	} // end propagate
	
	/* ---------------------------------- 
     * Useful functions to compute the constraint
     * --------------------------------- */
	
	/**
     * @return nodes neighbors to the node with index nodeId
     *   both edges between the two nodes (in both direction) should be available
     */
    private def stronglyConnectedNeighbors(nodeId : Int) : List[Int] =
      g.possibleNodes.filter(n => (nodeId != n) && 
        !g.possibleEdges(nodeId).map(idx => g.edge(idx)).filter(e => e._2 == n).isEmpty && 
        !g.possibleEdges(n).map(idx => g.edge(idx)).filter(e => e._2 == nodeId).isEmpty)
        
    /**
     * @param : Nodes to check
     * @return a list of list of integer, each inner list contains one connected component
     */
    private def stronglyConnectedComponents(nodesToCheck : List[Int]) : List[List[Int]] = {
      def possibleConnectedComponents(nodesToCheck : List[Int]) : List[List[Int]] = {
        var visitedNodes : List[Int] = List()
        var componentsList : List[List[Int]] = Nil
        for (n <- nodesToCheck; if g.possibleNodes.contains(n)) {
          if (!visitedNodes.contains(n)) {
            val connectedNodes : List[Int] = connectedNodesList(List(n),List())
            visitedNodes = visitedNodes ++ connectedNodes
            componentsList = connectedNodes :: componentsList
          }
        }
        componentsList
      }
      def connectedNodesList(neighborsNotVisited : List[Int], connectedNodesAcc : List[Int]) : List[Int] = {
        neighborsNotVisited match {
          case Nil => connectedNodesAcc.sorted
          case node :: t => {
            val neighbors : List[Int] = stronglyConnectedNeighbors(node) 
            val newNeighbors : List[Int] = neighbors.filter(elem => !(node :: connectedNodesAcc).contains(elem) )
            connectedNodesList((t ++ newNeighbors).distinct, node :: connectedNodesAcc)
          }
        }
      }
      possibleConnectedComponents(nodesToCheck)
    }
    
    // return true if there exists at least one path from s to d without going through n
    private def existPath(s : Int, d : Int, n : Int) : Boolean = {
      def possibleNeighborsList(nodeId : Int) : List[Int] = g.possibleOutEdges(nodeId).map(g.edge(_)._2)
      // Search to find a path from s to d
      if (s == d) return true
      var toVisit : List [Int] = List(s)
      var visited : List[Int] = List(n) // we will not visit n anymore
      while (! toVisit.isEmpty) {
        val node : Int = toVisit.head
        val possNList = possibleNeighborsList(node)
        if (possNList.contains(d)) return true
        visited = node :: visited
        val neighb =  possNList.filter(!visited.contains(_))
        toVisit = toVisit.tail ++ neighb
      }
      return false
    }
    
    // return true if there exists at least one path from s to d without going through edge e
	private def existPath(s : Int, d : Int, e : (Int,Int)) : Boolean = {
	  val (e1,e2) = e
	  def possibleNeighborsWithoutE(nodeId : Int) : List[Int] = {
	    if (nodeId == e1) g.possibleOutEdges(nodeId).map(g.edge(_)._2).filter(_ != e2)
	    else g.possibleOutEdges(nodeId).map(g.edge(_)._2)
	  }
      // Search to find a path from s to d
      if (s == d) return true
      var toVisit : List [Int] = List(s)
      var visited : List[Int] = List()
      while (! toVisit.isEmpty) {
        val node : Int = toVisit.head
        val possNList = possibleNeighborsWithoutE(node) 
        if (possNList.contains(d)) return true
        visited = node :: visited
        val neighb =  possNList.filter(!visited.contains(_))
        toVisit = toVisit.tail ++ neighb
      }
      return false
    }
	
}