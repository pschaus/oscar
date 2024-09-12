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
import oscar.cp.core.variables.{CPGraphVar, CPVar}

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Defines the constraint Complement(G1,G2) between two graphs G1 and G2
 * 	this constraint holds if G1 and G2 are complement
 */

class GraphComplement(val g1 : CPGraphVar, val g2: CPGraphVar) extends Constraint(g1.s, "Complement") {

	override def associatedVars(): Iterable[CPVar] = Array(g1, g2)

	override def setup(l: CPPropagStrength): Unit = {
	  // add filter when domain changes
	  g1.callPropagateWhenDomainChanges(this)
	  g2.callPropagateWhenDomainChanges(this)
	  propagate()
	}
	
	override def propagate(): Unit = {
	  // We can do all the propagation symmetrically pruning on the two graphs
	  
	  val g1PossNodes : List[Int] = g1.possibleNodes
	  val g1ReqNodes : List[Int] = g1.requiredNodes
	  val g2PossNodes : List[Int] = g2.possibleNodes
	  val g2ReqNodes : List[Int] = g2.requiredNodes
	  // should have equal node set :
	  // 	nodes set upper bound should be the intersection
	  for (n <- g1PossNodes; if !g2PossNodes.contains(n)){
	      g1.removeNodeFromGraph(n)
	  }
	  for (n <- g2PossNodes; if !g1PossNodes.contains(n)){
	      g2.removeNodeFromGraph(n)
	  }
	  // 	nodes set lower bound should be the union
	  for (n <- g1ReqNodes; if !g2ReqNodes.contains(n)){
	      g2.addNodeToGraph(n)
	  }
	  for (n <- g2ReqNodes; if !g1ReqNodes.contains(n)){
	      g1.addNodeToGraph(n)
	  }
	  
	  val newG1PossNodes : List[Int] = g1.possibleNodes
	  val newG1ReqNodes : List[Int] = g1.requiredNodes
	  val newG2PossNodes : List[Int] = g2.possibleNodes
	  val newG2ReqNodes : List[Int] = g2.requiredNodes
	  
	  // if an edge is required in one of the graphs, it should no longer be possible in the other
	  for (n <- newG1ReqNodes){
		  for (e <- g1.requiredEdges(n); if g2.possibleEdges(n).contains(e)){
		    val (src,dest) = g2.edge(e)
		    g2.removeEdgeFromGraph(src,dest)
		  }
	  }
	  for (n <- newG2ReqNodes){
		  for (e <- g2.requiredEdges(n); if g1.possibleEdges(n).contains(e)){
		    val (src,dest) = g1.edge(e)
		    g1.removeEdgeFromGraph(src,dest)
		  }
	  }
	  
	  // if an edge is possible in one graph but not possible in the other and that endNodes to this edge are required, 
	  // 	it should be required (only way to have this edge in the complement)
	  for (n <- newG1PossNodes){
		  for (e <- g1.possibleEdges(n); if !g2.possibleEdges(n).contains(e)){
		      val (src,dest) = g1.edge(e)
		      if (newG1ReqNodes.contains(src) && newG1ReqNodes.contains(dest))
		        g1.addEdgeToGraph(src,dest)
		  }
	  }
	  for (n <- newG2PossNodes){
		  for (e <- g2.possibleEdges(n); if !g1.possibleEdges(n).contains(e)){
		      val (src,dest) = g2.edge(e)
		      if (newG2ReqNodes.contains(src) && newG2ReqNodes.contains(dest))
		        g2.addEdgeToGraph(src,dest)
		  }
	  }
	}
	
}