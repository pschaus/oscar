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

import oscar.cp.core._

import scala.collection.mutable.PriorityQueue
import oscar.algo.reversible.ReversibleBoolean
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.{CPGraphVar, CPVar}

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Defines the constraint SimplePath(g, n1, n2) for a directed graph :
 * 	this constraint holds if there is a path from n1 to n2 (n1 != n2) in the graph g  
 *  @param  g   the graph
 *          src  the source of the path
 *          dest the destination of the path
 */

class GraphSimplePath(val g : CPGraphVar, src : Int, dest : Int) extends Constraint(g.s, "Simple Path") {
  override def associatedVars(): Iterable[CPVar] = Array(g)

  val pNodes : List[Int] = g.possibleNodes
   val n : Int = pNodes.length
    // build transitive closure of the possible values of the graph
    //   tc is composed of ReversibleBool to allow backtrack in search and still update tc
    var tc : Array[Array[ReversibleBoolean]] = buildTC(pNodes)
    // count the number of possible edges/nodes to be able to detect modification when propagate is called
    var nbEdges : ReversibleInt = new ReversibleInt(g.s, g.nbPossibleEdges)
    var nbNodes : ReversibleInt = new ReversibleInt(g.s, n)
  
    override def setup(l: CPPropagStrength): Unit = {
      // add src and dest mandatory
      g.addNodeToGraph(src)
      g.addNodeToGraph(dest)
      // remove all edges incoming to the source and all outgoing edges from the destination (they must not be used to make a path from src to dest)
      for (e <- g.possibleInEdges(src)) g.removeEdgeFromGraph(e)
      for (e <- g.possibleOutEdges(dest)) g.removeEdgeFromGraph(e)
      
	  // add filter when domain changes
	  g.callPropagateWhenDomainChanges(this)
	  
	  // launch initial propagation
	  propagate()
	}
    
    override def propagate(): Unit = {
      val possNodes : List[Int] = g.possibleNodes
      val reqNodes  : List[Int] = g.requiredNodes
      
      // check if the constraint is entailed (there is a required path)
      isEntailed(possNodes,reqNodes)
      if(!isActive)
        return
      
      // check if we need to rebuild tc before pruning
      val newNbEdges : Int = g.nbPossibleEdges
      val newNbNodes : Int = possNodes.length
      if (newNbEdges < nbEdges.getValue || newNbNodes < nbNodes.getValue) {
        // at least one edge/node was removed since last propagate -> rebuild TC and update nbEdges/nbNodes
        nbEdges.setValue(newNbEdges)
        nbNodes.setValue(newNbNodes)
        updateTC(possNodes, reqNodes)
      }

      // pruning
	  
      // prune upper bound : forbidden edges
      // an arc (u,v) is forbidden if 
      //	   (src,u) not in TC
      //    OR (v,dest) not in TC
      //    OR exists n in requiredNodes such that : (n,u) not in TC AND (v,n) not in TC
      val possibleEdges : List[Int] = possNodes.flatMap(g.possibleOutEdges(_))
      for (e <- possibleEdges){
        val (u,v) = g.edge(e)
	    if (tc(src)(u).getValue == false){
	      // remove e because no path from src to u in tc
	      g.removeEdgeFromGraph(e)
	    } else if (tc(v)(dest).getValue == false) {
	      // remove e because no path from v to dest in tc
	      g.removeEdgeFromGraph(e)
	    } else {
	      var b : Boolean = true
	      for (n <- reqNodes; if b){
	        if (tc(n)(u).getValue == false && tc(v)(n).getValue == false){
	          // remove e because no path from src to dest passing by required node n using e
	          g.removeEdgeFromGraph(e)
	          b = false // edge is removed, no need to look for it again
	        }
	      }
	    }
	  }
      // for each required edge,
      // 	we should remove its counterpart, all other edges starting from its source and all other edges which destination is the same
      val requiredEdges : List[Int] = possNodes.flatMap(g.requiredOutEdges(_))
      val posNotReqEdgesTuple : List[(Int,Int)] = possNodes.flatMap(g.possibleOutEdges(_)).filter(!requiredEdges.contains(_)).map(g.edge(_))
      for (req <- requiredEdges){
        val (u,v) = g.edge(req)
        // counterpart edge
        if(posNotReqEdgesTuple.contains((v,u)))
          g.removeEdgeFromGraph(v,u)
        // all edges starting from same source or with same destination
        val toBeRemovedEdges : List[(Int,Int)] = posNotReqEdgesTuple.filter(e => (e._1 == u) || (e._2 == v))
        for (e <- toBeRemovedEdges) g.removeEdgeFromGraph(e._1,e._2)
      }

      //  prune lower bound : mandatory edges
      for (n1 <- reqNodes; if (g.requiredOutEdges(n1).length == 0)){
        if (g.possibleOutEdges(n1).length == 1){
          // there is a bridge (n1,n2) as n1 is mandatory and has only one outgoing edge
          val n2 : Int = g.edge(g.possibleOutEdges(n1).head)._2
          g.addEdgeToGraph(n1,n2)
        }
      }
      
      // prune upper bound : forbidden nodes
      // isolated nodes are forbidden : if a node is not the destination (target) and does not have outgoing edges, it should be removed
      for (node <- possNodes; if node != dest) {
        if (g.possibleOutEdges(node).isEmpty)
          g.removeNodeFromGraph(node)
          
      }
      
      // prune lower bound : mandatory nodes
      val possibleNotMandatoryNodes = g.possibleNodes.filter(!reqNodes.contains(_))
      // we have to check that, if we remove a possible node; 
      //   all mandatory nodes are still reachable from src and dest can be reachable from all mandatory nodes
      for (n <- possibleNotMandatoryNodes){
        if (checkMandatoryNodes(src, dest, n))
          g.addNodeToGraph(n)
      }
    }
	
	
  /* ---------------------------------- 
   * Useful functions to compute the constraint
   * --------------------------------- */
	
    // check if there exists a required path (=path composed of only required edges from source to destination)
    private def isEntailed(possNodes : List[Int], reqNodes : List[Int]) : Unit = {
      if (existRequiredPath(src, dest, reqNodes)){
        val requiredEdges : List[Int] = reqNodes.flatMap(g.requiredOutEdges(_))
        val possibleNotRequiredEdges : List[Int] = possNodes.flatMap(g.possibleOutEdges(_)).filter(!requiredEdges.contains(_))       
        // remove all other possible edges
        for (e <- possibleNotRequiredEdges)
          g.removeEdgeFromGraph(e)
        deactivate()
      }
    }
    
    private def requiredNeighborsList(nodeId : Int) : List[Int] = g.requiredOutEdges(nodeId).map(g.edge(_)._2)
          
    // return true if there is a required path from s to d of length equal to all required nodes
    private def existRequiredPath(s : Int, d : Int, reqNodes : List[Int]) : Boolean = {
      // Search to find a path from s to d
      var toVisit : List [Int] = List(s)
      var visited : List[Int] = List()
      val nbReqNodes = reqNodes.length
      while (! toVisit.isEmpty) {
        val node : Int = toVisit.head
        visited = node :: visited
        val neighb =  requiredNeighborsList(node).filter(!visited.contains(_))
        if (neighb.contains(d) && (visited.length + 1) == nbReqNodes) return true
        toVisit = toVisit.tail ++ neighb
      }
      return false
    }
    
    private def possibleNeighborsList(nodeId : Int) : List[Int] = g.possibleOutEdges(nodeId).map(g.edge(_)._2)
	
    // build transitive closure of possible edges of the graph
    private def buildTC(possNodes : List[Int]) : Array[Array[ReversibleBoolean]] = {
      val tempTc =  Array.fill(n)(Array.fill(n)(new ReversibleBoolean(g.s,true)))
      // precompute possible neighbors to spare computation time and give it to existsPath()
      val possNeigh : List[List[Int]] = (0 until n).toList.map(possibleNeighborsList(_))
      // update tc
      for (n1 <- possNodes){
        for (n2 <- possNodes; if n1 != n2){
            if (!existPath(n1, n2, possNeigh))
              tempTc(n1)(n2).setValue(false)
        }
      }
      tempTc
    }
    
    // update TC with nodes that might accordingly
    private def updateTC(possNodes : List[Int], reqNodes : List[Int]) : Unit = {
      // compute only nodes that might change
      val srcReqNodes = reqNodes.flatMap(g.requiredOutEdges(_)).map(g.edge(_)._1)
      val nodesToCheck : List[Int] = possNodes.filter(!srcReqNodes.contains(_))
      // precompute possible neighbors to spare computation time and give it to existsPath()
      val possNeigh : List[List[Int]] = (0 until n).toList.map(possibleNeighborsList(_))
      // update tc
      for (n1 <- nodesToCheck){
        for (n2 <- nodesToCheck; if n1 != n2){
          if (tc(n1)(n2).getValue != false) { // if they were already no available path, no reason to go further
            if (!existPath(n1, n2, possNeigh))
              tc(n1)(n2).setValue(false)
          }
        }
      }
    }
          
    // return true if there exists at least one path from s to d
    private def existPath(s : Int, d : Int, possNeigh : List[List[Int]]) : Boolean = {
      // Search to find a path from s to d
      if (s == d) return true
      var toVisit : List [Int] = List(s)
      var visited : List[Int] = List()
      while (! toVisit.isEmpty) {
        val node : Int = toVisit.head
        val possNList = possNeigh(node)
        if (possNList.contains(d)) return true
        visited = node :: visited
        val neighb =  possNList.filter(!visited.contains(_))  
        toVisit = neighb ::: toVisit.tail
      }
      return false
    }
    
    // return true if there exists at least one path from s to d without going through n
    private def existPath(s : Int, d : Int, n : Int) : Boolean = {
      // Search to find a path from s to d
      var toVisit : List [Int] = List(s)
      var visited : List[Int] = List(n) // we will not visit n anymore
      while (! toVisit.isEmpty) {
        val node : Int = toVisit.head
        visited = node :: visited
        if (node == d) return true
        val neighb =  possibleNeighborsList(node).filter(!visited.contains(_))
        toVisit = toVisit.tail ++ neighb
      }
      return false
    }
    
    // we have to check that, if we remove a possible node; 
    //   all mandatory nodes are still reachable from src
    //   and dest can be reachable from all mandatory nodes
    private def checkMandatoryNodes(s : Int, d : Int, n : Int) : Boolean = {
      val mandatoryNodes = g.requiredNodes
      for (mandNode <- mandatoryNodes) { 
        if (!existPath(s, mandNode, n)){
        	// there is no path from source s to a mandatory node manNode without going through n
        	return true  // n should be mandatory
        }
        if (!existPath(mandNode, d, n)){
        	// there is no path from a mandatory node manNode to the destination d without going through n
        	return true  // n should be mandatory
        }
      }
      return false    
    }
}