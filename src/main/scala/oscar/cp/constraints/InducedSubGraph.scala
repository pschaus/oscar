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
 * Defines the constraint InducedSubgraph(G1,G2) between two graphs G1 and G2
 * 	this constraint holds if G1 is an induced Subgraph of G2
 */

class InducedSubGraph(val g1 : CPGraphVar, val g2: CPGraphVar) extends Constraint(g1.s, "InducedSubGraph") {

	override def associatedVars(): Iterable[CPVar] = Array(g1, g2)

	override def setup(l: CPPropagStrength): Unit = {
	  // InducedSubGraph  =  SubGraph + induced part
	  s.post(new SubGraph(g1,g2))
	  // add filter when domain changes
	  g1.callPropagateWhenDomainChanges(this)
	  g2.callPropagateWhenDomainChanges(this)
	  propagate()
	}
	
	override def propagate(): Unit = {
	  // We only have to deal with the induced part of the constraint
	  // 	we dealt with the subgraph part by posting constraint in setup()
	  
	  // enforce equality
	  val g1ReqN : List[Int] = g1.requiredNodes
	  val g2ReqN : List[Int] = g2.requiredNodes
	  if (! g1ReqN.sameElements(g2ReqN)){
	    for (n <- g1ReqN)
	      if( ! g2ReqN.contains(n))
	        g2.addNodeToGraph(n)
	    for (n <- g2ReqN)
	      if( ! g1ReqN.contains(n))
	        g1.addNodeToGraph(n)
	  }
	  
	  // for each edge incident to required nodes,
	  // 	if the edge was required in a graph, then required in the other
	  // 	if the edge was not possible in a graph, then not possible in the other
	  val g1ReqNodes : List[Int] = g1.requiredNodes
	  for (n <- g1ReqNodes){
	    for (e <- g1.possibleEdges(n); if (g1ReqNodes.contains(g1.edge(e)._1) && g1ReqNodes.contains(g1.edge(e)._2)) ){
	      if (g1.requiredEdges(n).contains(e))
	        g2.addEdgeToGraph(g1.edge(e)._1,g1.edge(e)._2)
	      if (!g2.possibleEdges(n).map(g2.edge(_)).contains(g1.edge(e)))
	        g1.removeNodeFromGraph(e)
	    }
	  }
	  val g2ReqNodes : List[Int] = g2.requiredNodes
	  for (n <- g2ReqNodes){
	    for (e <- g2.possibleEdges(n); if (g2ReqNodes.contains(g2.edge(e)._1) && g2ReqNodes.contains(g2.edge(e)._2)) ){
	      if (g2.requiredEdges(n).contains(e))
	        g1.addEdgeToGraph(g2.edge(e)._1,g2.edge(e)._2)
	      if (!g1.possibleEdges(n).map(g1.edge(_)).contains(g1.edge(e)))
	        g2.removeNodeFromGraph(e)
	    }
	  }
	}
}