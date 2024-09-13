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
import oscar.cp.constraints.GraphUndirected

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

class TestGraphUndirected extends TestSuite  {
  
  test("Test 1 : Test constraint initial propagation") {
    val cp = CPSolver()
    val nnodes : Int = 3
    val edges = List((0,1),(1,2))
    val g1 = CPGraphVar(cp, nnodes, edges)
    // g2 will be the undirected version of g1
    val nnodes2 : Int = nnodes
    val g2 = CPGraphVar(cp, nnodes)
    // g2.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    
    // check that all is correct : 
    g1.possibleNodes should be (List(0,1,2))
    g2.possibleNodes should be (List(0,1,2))
    g1.requiredNodes should be (List())
    g2.requiredNodes should be (List())
    
    for (x <- 0 to nnodes-1) {
      g1.requiredEdges(x).sorted  should be (List())
      g2.requiredEdges(x).sorted  should be (List())
    }
    g1.possibleEdges(0).sorted 	should be (List(0))
    g1.possibleEdges(1).sorted 	should be (List(0,1))
    g1.possibleEdges(2).sorted 	should be (List(1))
    g2.possibleEdges(0).sorted  should be (List(0,1,2,4))
    g2.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g2.possibleEdges(2).sorted  should be (List(1,3,4,5))
    
    
    // add constraint
    postAndCheckSuspend(cp,new GraphUndirected(g1,g2))
    
    // check that all changes are correct acc. to definition
    // 	 -> no changes to g1
    // 	 -> remove edges (0,2),(2,0) from g2 =  edges nb 1 and 4
    g1.possibleNodes should be (List(0,1,2))
    g2.possibleNodes should be (List(0,1,2))
    g1.requiredNodes should be (List())
    g2.requiredNodes should be (List())
    
    for (x <- 0 to nnodes-1) {
      g1.requiredEdges(x).sorted  should be (List())
      g2.requiredEdges(x).sorted  should be (List())
    }
    g1.possibleEdges(0).sorted 	should be (List(0))
    g1.possibleEdges(1).sorted 	should be (List(0,1))
    g1.possibleEdges(2).sorted 	should be (List(1))
    g2.possibleEdges(0).sorted  should be (List(0,2))
    g2.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g2.possibleEdges(2).sorted  should be (List(3,5))
  }
  
  test("Test 2 : Add required edge") {
    // we start from graph as described in test 1 to which we will add an edge
    val cp = CPSolver()
    val nnodes : Int = 3
    val edges = List((0,1),(1,2))
    val g1 = CPGraphVar(cp, nnodes, edges)
    // g2 will be the undirected version of g1
    val nnodes2 : Int = nnodes
    val g2 = CPGraphVar(cp, nnodes)
    // g2.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    postAndCheckSuspend(cp,new GraphUndirected(g1,g2))
    // g2.edges are (0,1),(1,0),(1,2),(2,1)
    
    // we add a required edge in g1 -> should also be required in g2
    postAndCheckSuspend(cp,g1.addEdge(0,1))
    // check all edges : (0,1) required in g1
    // 	and (0,1),(1,0) required in g2
    g1.requiredEdges(0).sorted 	should be (List(0))
    g1.requiredEdges(1).sorted 	should be (List(0))
    g1.requiredEdges(2).sorted 	should be (List())
    g2.requiredEdges(0).sorted  should be (List(0,2))
    g2.requiredEdges(1).sorted  should be (List(0,2))
    g2.requiredEdges(2).sorted  should be (List())
    g1.possibleEdges(0).sorted 	should be (List(0))
    g1.possibleEdges(1).sorted 	should be (List(0,1))
    g1.possibleEdges(2).sorted 	should be (List(1))
    g2.possibleEdges(0).sorted  should be (List(0,2))
    g2.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g2.possibleEdges(2).sorted  should be (List(3,5))
  }
  
  test("Test 3 : Remove possible edge") {
	// we start from graph as described in test 1 to which we will remove an edge
    val cp = CPSolver()
    val nnodes : Int = 3
    val edges = List((0,1),(1,2))
    val g1 = CPGraphVar(cp, nnodes, edges)
    // g2 will be the undirected version of g1
    val nnodes2 : Int = nnodes
    val g2 = CPGraphVar(cp, nnodes)
    // g2.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    postAndCheckSuspend(cp,new GraphUndirected(g1,g2))
    // g2.edges are (0,1),(1,0),(1,2),(2,1)
    
    // we add a required edge in g1 -> should also be required in g2
    postAndCheckSuspend(cp,g1.removeEdge(0,1))
    // check all edges : (0,1) removed in g1
    // 	and (0,1),(1,0) removed in g2
    g1.possibleEdges(0).sorted 	should be (List())
    g1.possibleEdges(1).sorted 	should be (List(1))
    g1.possibleEdges(2).sorted 	should be (List(1))
    g2.possibleEdges(0).sorted  should be (List())
    g2.possibleEdges(1).sorted  should be (List(3,5))
    g2.possibleEdges(2).sorted  should be (List(3,5))
  }
  
  test("Test 4 : Add required node") {
	// we start from graph as described in test 1 to which we will add a node to required set
    val cp = CPSolver()
    val nnodes : Int = 3
    val edges = List((0,1),(1,2))
    val g1 = CPGraphVar(cp, nnodes, edges)
    // g2 will be the undirected version of g1
    val nnodes2 : Int = nnodes
    val g2 = CPGraphVar(cp, nnodes)
    // g2.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    postAndCheckSuspend(cp,new GraphUndirected(g1,g2))
    // g2.edges are (0,1),(1,0),(1,2),(2,1)
    
    // we add a required node in g1 -> should also be required in g2
    postAndCheckSuspend(cp,g1.addNode(1))
    
    g1.possibleNodes.sorted should be (List(0,1,2))
    g2.possibleNodes.sorted should be (List(0,1,2))
    g1.requiredNodes.sorted should be (List(1))
    g2.requiredNodes.sorted should be (List(1))
    
    for (x <- 0 to nnodes-1) {
      g1.requiredEdges(x).sorted  should be (List())
      g2.requiredEdges(x).sorted  should be (List())
    }
    g1.possibleEdges(0).sorted 	should be (List(0))
    g1.possibleEdges(1).sorted 	should be (List(0,1))
    g1.possibleEdges(2).sorted 	should be (List(1))
    g2.possibleEdges(0).sorted  should be (List(0,2))
    g2.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g2.possibleEdges(2).sorted  should be (List(3,5))
  }
  
  test("Test 5 : Remove possible node") {
	// we start from graph as described in test 1 to which we will remove a node
    val cp = CPSolver()
    val nnodes : Int = 3
    val edges = List((0,1),(1,2))
    val g1 = CPGraphVar(cp, nnodes, edges)
    // g2 will be the undirected version of g1
    val nnodes2 : Int = nnodes
    val g2 = CPGraphVar(cp, nnodes)
    // g2.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    postAndCheckSuspend(cp,new GraphUndirected(g1,g2))
    // g2.edges are (0,1),(1,0),(1,2),(2,1)
    
    // we remove a node in g1 -> should also be removed in g2
    postAndCheckSuspend(cp,g1.removeNode(2))
    
    g1.possibleNodes.sorted should be (List(0,1))
    g2.possibleNodes.sorted should be (List(0,1))
    g1.requiredNodes.sorted should be (List())
    g2.requiredNodes.sorted should be (List())
    
    for (x <- 0 to nnodes-1) {
      g1.requiredEdges(x).sorted  should be (List())
      g2.requiredEdges(x).sorted  should be (List())
    }
    g1.possibleEdges(0).sorted 	should be (List(0))
    g1.possibleEdges(1).sorted 	should be (List(0))
    g1.possibleEdges(2).sorted 	should be (List())
    g2.possibleEdges(0).sorted  should be (List(0,2))
    g2.possibleEdges(1).sorted  should be (List(0,2))
    g2.possibleEdges(2).sorted  should be (List())
  }
  
}