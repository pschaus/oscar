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
 * Defines the constraint Subgraph(G1,G2) between two graphs G1 and G2
 * 	this constraint holds if G1 is a subgraph of G2
 */

class SubGraph(val g1 : CPGraphVar, val g2: CPGraphVar) extends Constraint(g1.s, "SubGraph") {

	override def associatedVars(): Iterable[CPVar] = Array(g1, g2)

	override def setup(l: CPPropagStrength): Unit = {
	  // check inconsistency
	  if(inconsistent) throw Inconsistency
	  // add filter when domain changes
	  g1.callPropagateWhenDomainChanges(this)
	  g2.callPropagateWhenDomainChanges(this)
	  // launch initial propagate
	  propagate()
	}
	
	override def propagate(): Unit = {
	  // test for entailment	  
	  // test if g1.possible values are included in g2.required values => entailment, success
	  val g1PossNodes = g1.possibleNodes
	  val g2ReqNodes = g2.requiredNodes  
	  if (g1PossNodes.length <= g2ReqNodes.length) {
	    val g1PossEdges : List[Int] = g1PossNodes.flatMap(g1.possibleOutEdges(_))
	    val g2ReqEdgesMap : List[(Int,Int)] = g2ReqNodes.flatMap(g2.requiredOutEdges(_)).map(g2.edge(_))
	    var nCorrect : Int = 0 // count g1.possibleEdges included in g2.requiredEdge
	    for (e <- g1PossEdges; if g2ReqEdgesMap.contains(g1.edge(e)))
	      nCorrect += 1
	    if (g1PossEdges.length == nCorrect) {
				deactivate()
				return
			}
	  }
	  
	  // pruning
	  
	  // for all required values in g1 and not required in g2, should be required in g2
	  val g1ReqNodes = g1.requiredNodes
	  for (n <- g1ReqNodes){
	    if (!g2ReqNodes.contains(n))
				g2.addNodeToGraph(n)

	    for (e <- g1.requiredEdges(n)){
	      val (src, dest) = g1.edge(e)
	      // test if g2 contains the required edge, otherwise, add it to required
	      if (!g2.requiredEdges(n).map(g2.edge(_)).contains(src, dest))
	    	  g2.addEdgeToGraph(src, dest)
	    }
	  }
	  
	  // for all possible values in g1 and not possible in g2, should be removed from g1
	  for (n <- g1PossNodes){
	    if (!g2.possibleNodes.contains(n))
	      g1.removeNodeFromGraph(n)
	     for (e <- g1.possibleEdges(n)){
	        val (src, dest) = g1.edge(e)
	        // test if g2 has the possible edge, otherwise, remove it from g1
	    	if (!g2.possibleEdges(n).map(g2.edge(_)).contains(src, dest))
	    	  g1.removeEdgeFromGraph(src, dest)
	    }
	  }
	}
	
	private def inconsistent : Boolean = {
	  // test if g1.required values are not included in g2.possible values => no solution, inconsistency
	  val g1ReqNodes = g1.requiredNodes
	  val g2PossNodes = g2.possibleNodes
	  val g1ReqEdges : List[Int] = g1ReqNodes.flatMap(g1.requiredOutEdges(_))
	  val g2PossEdgesMap : List[(Int,Int)] = g2PossNodes.flatMap(g2.possibleOutEdges(_)).map(g2.edge(_))  
	  if (g1ReqNodes.length > g2PossNodes.length) return true
	  for (e <- g1ReqEdges; if ! g2PossEdgesMap.contains(g1.edge(e)))
	    return true
	  false	    		  
	}
	
}