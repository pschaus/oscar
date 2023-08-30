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

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp._
import oscar.cp.constraints.GraphSymmetric

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

class TestGraphSymmetric extends TestSuite  {
  
  test("Test 1 : Test constraint initial propagation") {
    val cp = CPSolver()
    val nnodes1 : Int = 3
    val edges1 = List((0,1),(1,2),(2,1),(1,0))
    val g = CPGraphVar(cp, nnodes1, edges1)
    
    // 1) add some mandatory nodes/edges
    postAndCheckSuspend(cp,g.addNode(0))
    postAndCheckSuspend(cp,g.addNode(1))
    postAndCheckSuspend(cp,g.addEdge(0,1))
    
    // 2) check that all is correct : 
    // check nodes
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List(0,1))
    // check edges
    g.requiredEdges(0).sorted    should be (List(0))
    g.requiredEdges(1).sorted    should be (List(0))
    g.requiredEdges(2).sorted    should be (List())
    g.possibleEdges(0).sorted    should be (List(0,3))
    g.possibleEdges(1).sorted    should be (List(0,1,2,3))
    g.possibleEdges(2).sorted    should be (List(1,2))
    
    // 3) add constraint
    postAndCheckSuspend(cp,new GraphSymmetric(g))
    
    // 4) check that all changes are correct acc. to definition
    // check nodes
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List(0,1))
    
    // check edges
    // -> (1,0) should be required now as (0,1) was required
    g.requiredEdges(0).sorted    should be (List(0,3))
    g.requiredEdges(1).sorted    should be (List(0,3))
    g.requiredEdges(2).sorted    should be (List())
    g.possibleEdges(0).sorted    should be (List(0,3))
    g.possibleEdges(1).sorted    should be (List(0,1,2,3))
    g.possibleEdges(2).sorted    should be (List(1,2))
  }
  
  test("Test 2 : Add required edge") {
    // we start from graph as described in test 1 to which we will add an edge
    
  }
  
  test("Test 3 : Remove required edge") {
    // we start from graph as described in test 1 to which we will add an edge
    
  }
}