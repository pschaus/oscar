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
package oscar.cp.test

import oscar.cp.testUtils.TestSuite
import oscar.cp._
import oscar.cp.constraints.InducedSubGraph
import oscar.cp.testUtils.TestSuite

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

class TestSubGraphInduced extends TestSuite  {

  test("Test 1 : All possible") {
    val cp = CPSolver()
    val nnodes1 : Int = 2
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    postAndCheckSuspend(cp,new InducedSubGraph(g1,g2))
    
    // all should be possible for g1
    for (n <- 0 to (nnodes1-1)){
      g1.requiredInEdges(n).isEmpty should be (true)
      g1.requiredOutEdges(n).isEmpty should be (true)
      g1.requiredEdges(n).isEmpty should be (true)
    }
    g1.requiredNodes.isEmpty should be (true)
    g1.possibleNodes         should be ((0 to (nnodes1-1)))
    // edge is : 0 = (0,1)
    g1.requiredInEdges(0).sorted  should be (List())
    g1.requiredOutEdges(0).sorted should be (List())
    g1.requiredEdges(0).sorted    should be (List())
    g1.possibleInEdges(0).sorted  should be (List())
    g1.possibleOutEdges(0).sorted should be (List(0))
    g1.possibleEdges(0).sorted    should be (List(0))
    g1.requiredInEdges(1).sorted  should be (List())
    g1.requiredOutEdges(1).sorted should be (List())
    g1.requiredEdges(1).sorted    should be (List())
    g1.possibleInEdges(1).sorted  should be (List(0))
    g1.possibleOutEdges(1).sorted should be (List())
    g1.possibleEdges(1).sorted    should be (List(0))
    
    // all should be possible for g2
    for (n <- 0 to (nnodes2-1)){
      g2.requiredInEdges(n).isEmpty should be (true)
      g2.requiredOutEdges(n).isEmpty should be (true)
      g2.requiredEdges(n).isEmpty should be (true)
    }
    g2.requiredNodes.isEmpty should be (true)
    g2.possibleNodes         should be ((0 to (nnodes2-1)))
    // edges are : 0 = (0,1), 1 = (1,2)
    g2.requiredInEdges(0).sorted  should be (List())
    g2.requiredOutEdges(0).sorted should be (List())
    g2.requiredEdges(0).sorted    should be (List())
    g2.possibleInEdges(0).sorted  should be (List())
    g2.possibleOutEdges(0).sorted should be (List(0))
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.requiredInEdges(1).sorted  should be (List())
    g2.requiredOutEdges(1).sorted should be (List())
    g2.requiredEdges(1).sorted    should be (List())
    g2.possibleInEdges(1).sorted  should be (List(0))
    g2.possibleOutEdges(1).sorted should be (List(1))
    g2.possibleEdges(1).sorted    should be (List(0,1))
    g2.requiredInEdges(2).sorted  should be (List())
    g2.requiredOutEdges(2).sorted should be (List())
    g2.requiredEdges(2).sorted    should be (List())
    g2.possibleInEdges(2).sorted  should be (List(1))
    g2.possibleOutEdges(2).sorted should be (List())
    g2.possibleEdges(2).sorted    should be (List(1))
  }
  
  test("Test 2 : Add required node in g1") {
    val cp = CPSolver()
    val nnodes1 : Int = 2
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    // #1 add before posting constraint
    postAndCheckSuspend(cp, g1.addNode(0))
    postAndCheckSuspend(cp,new InducedSubGraph(g1,g2))
    // if node 0 is required in g1 and subgraph(g1,g2)
    //    => node 0 should be required in g2
    g1.requiredNodes should be (List(0))
    g2.requiredNodes should be (List(0))
    
    // #2 add after posting constraint (check propagator working properly)
    postAndCheckSuspend(cp,g1.addNode(1))
    // if node 1 is required in g1 and subgraph(g1,g2)
    //    => node 1 should be required in g2
    g1.requiredNodes should be (List(0,1))
    g2.requiredNodes should be (List(0,1))
  }
}