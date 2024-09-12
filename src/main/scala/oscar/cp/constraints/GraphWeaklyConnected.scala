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
 * Defines the constraint weakly Connected(G) for a directed graph 
 */

class GraphWeaklyConnected(val g : CPGraphVar) extends Constraint(g.s, "Weakly Connected") {

	override def associatedVars(): Iterable[CPVar] = Array(g)

	override def setup(l: CPPropagStrength): Unit = {
	  // create undirected version of g
	  val gu = CPGraphVar(g.possibleNodes.size)(s)
	  // link g and gu
	  s.post(new GraphUndirected(g,gu))  // should not happen
	  // post connected constraint on gu
	  s.post(new GraphUndirectedConnected(gu))
	}
}