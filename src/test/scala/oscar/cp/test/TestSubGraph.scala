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
import oscar.cp.constraints.SubGraph

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

class TestSubGraph extends TestSuite  {

  test("Test 1 : All possible") {
    val cp = CPSolver()
    val nnodes1 : Int = 2
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    postAndCheckSuspend(cp,new SubGraph(g1,g2))
    
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
    postAndCheckSuspend(cp,g1.addNode(0))
    postAndCheckSuspend(cp,new SubGraph(g1,g2))
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
   
  test("Test 3 : Add required node in g2") {
    val cp = CPSolver()
    val nnodes1 : Int = 2
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    // #1 add before posting constraint
    postAndCheckSuspend(cp,g2.addNode(0))
    postAndCheckSuspend(cp,new SubGraph(g1,g2))
    
    // should change nothing in g1
    g1.requiredNodes should be (List())
    g2.requiredNodes should be (List(0))
    
    // #2 add after posting constraint
    postAndCheckSuspend(cp,g2.addNode(1))
    // should change nothing in g1
    g1.requiredNodes should be (List())
    g2.requiredNodes should be (List(0,1))
  }
 
    test("Test 4 : add required edge in g1 before post constraint") {
    val cp = CPSolver()
    val nnodes1 : Int = 2
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    // #1 add before posting constraint
    postAndCheckSuspend(cp,g1.addEdge(0, 1))
    g1.requiredInEdges(1) should be (List(0))
    g1.requiredOutEdges(0) should be (List(0))
    g2.requiredInEdges(1) should be (List())
    g2.requiredOutEdges(0) should be (List())
    val c : Constraint = new SubGraph(g1,g2)
    cp.post(c)
    // after posting constraint, the edge (0,1) should be required in g2
    g1.requiredInEdges(1) should be (List(0))
    g1.requiredOutEdges(0) should be (List(0))
    g2.requiredInEdges(1) should be (List(0))
    g2.requiredOutEdges(0) should be (List(0))
    // also, as (0,1) is required in g2, the constraint is entailed and c no longer active
    assert(!c.isActive)
    }
    
    test("Test 5 : add required edge in g1 after post constraint") {
    val cp = CPSolver()
    val nnodes1 : Int = 2
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    val c : Constraint = new SubGraph(g1,g2)
    postAndCheckSuspend(cp,c)
    // #2 add after posting constraint
    postAndCheckSuspend(cp,g1.addEdge(0, 1))
    // constraint propagator should be called and set edge 0:=(0,1) required in g2
    g1.requiredInEdges(1) should be (List(0))
    g1.requiredOutEdges(0) should be (List(0))
    g2.requiredInEdges(1) should be (List(0))
    g2.requiredOutEdges(0) should be (List(0))
    // also, as (0,1) is required in g2, the constraint is entailed and c no longer active
    assert(c.isActive == false)
    }
    
    test("Test 6 : add required edge in g2 st entailment") {
    val cp = CPSolver()
    val nnodes1 : Int = 2
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    // #1 add before posting constraint
    postAndCheckSuspend(cp,g2.addEdge(0, 1))
    g1.requiredInEdges(0)  should be (List())
    g1.requiredOutEdges(0) should be (List())
    g1.requiredInEdges(1)  should be (List())
    g1.requiredOutEdges(1) should be (List())
    g2.requiredInEdges(0) should be (List())
    g2.requiredOutEdges(0) should be (List(0))
    g2.requiredInEdges(1) should be (List(0))
    g2.requiredOutEdges(1) should be (List())
    g2.requiredNodes.sorted	should be (List(0,1))
    // the constraint is entailed, we can check it directly by noticing that is is no longer active 
    val c : Constraint =  new SubGraph(g1,g2)
    postAndCheckSuccess(cp,c)
    }
    
    test("Test 7 : add required edge in g2 st entailment (2)") {
    val cp = CPSolver()
    val nnodes1 : Int = 2
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)

    val c : Constraint = new SubGraph(g1,g2) 
    postAndCheckSuspend(cp,c)
    // #2 add after posting constraint
    postAndCheckSuspend(cp,g2.addEdge(0, 1))
    // propagator should get entailement of constraint
    // check entailment -> constraint no longer active
    assert(c.isActive == false)
    }
    
    test("Test 8 : Remove possible edge in g1 before c") {
    val cp = CPSolver()
    val nnodes1 : Int = 4
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    postAndCheckSuspend(cp,g1.removeEdge(0,1))
    val c : Constraint = new SubGraph(g1,g2) 
    postAndCheckSuspend(cp,c)
    g1.possibleEdges(0).sorted    should be (List())
    g1.possibleEdges(1).sorted    should be (List())
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0,1))
    g2.possibleEdges(2).sorted    should be (List(1))
    }
    
    test("Test 9 : Remove possible edge in g1 after c") {
    val cp = CPSolver()
    val nnodes1 : Int = 4
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    val c : Constraint = new SubGraph(g1,g2) 
    postAndCheckSuspend(cp,c)
    g1.possibleEdges(0).sorted    should be (List(0))
    g1.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0,1))
    g2.possibleEdges(2).sorted    should be (List(1))
    
    postAndCheckSuspend(cp,g1.removeEdge(0,1))
    g1.possibleEdges(0).sorted    should be (List())
    g1.possibleEdges(1).sorted    should be (List())
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0,1))
    g2.possibleEdges(2).sorted    should be (List(1))
    }
    
    test("Test 10 : Remove possible edge in g2 before c") {
    val cp = CPSolver()
    val nnodes1 : Int = 4
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    postAndCheckSuspend(cp,g2.removeEdge(1,2))
    g1.possibleEdges(0).sorted    should be (List(0))
    g1.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(2).sorted    should be (List())
    
    val c : Constraint = new SubGraph(g1,g2) 
    postAndCheckSuspend(cp,c)
    // posting constraint should change nothing
    g1.possibleEdges(0).sorted    should be (List(0))
    g1.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(2).sorted    should be (List())
    }
    
    test("Test 11 : Remove possible edge in g2 after c") {
    val cp = CPSolver()
    val nnodes1 : Int = 4
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    val c : Constraint = new SubGraph(g1,g2) 
    postAndCheckSuspend(cp,c)
    g1.possibleEdges(0).sorted    should be (List(0))
    g1.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0,1))
    g2.possibleEdges(2).sorted    should be (List(1))
    
    postAndCheckSuspend(cp,g2.removeEdge(1,2))
    g1.possibleEdges(0).sorted    should be (List(0))
    g1.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(2).sorted    should be (List())
    }
    
    test("Test 12 : Remove possible edge in g2 before c leading to Failure") {
    val cp = CPSolver()
    val nnodes1 : Int = 4
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    postAndCheckSuspend(cp,g1.addEdge(0,1))
    g1.requiredEdges(0).sorted    should be (List(0))
    g1.requiredEdges(1).sorted    should be (List(0))
    g1.possibleEdges(0).sorted    should be (List(0))
    g1.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0,1))
    g2.possibleEdges(2).sorted    should be (List(1))
    
    postAndCheckSuspend(cp,g2.removeEdge(0,1))
    g1.requiredEdges(0).sorted    should be (List(0))
    g1.requiredEdges(1).sorted    should be (List(0))
    g1.possibleEdges(0).sorted    should be (List(0))
    g1.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(0).sorted    should be (List())
    g2.possibleEdges(1).sorted    should be (List(1))
    g2.possibleEdges(2).sorted    should be (List(1))
    
    val c : Constraint = new SubGraph(g1,g2) 
    postAndCheckFailure(cp, c)
    }
    
    test("Test 13 : Remove possible edge in g2 after c leading to entailment") {
    val cp = CPSolver()
    val nnodes1 : Int = 4
    val edges1 = List((0,1))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    postAndCheckSuspend(cp,g1.addEdge(0,1))
    g1.requiredEdges(0).sorted    should be (List(0))
    g1.requiredEdges(1).sorted    should be (List(0))
    g1.possibleEdges(0).sorted    should be (List(0))
    g1.possibleEdges(1).sorted    should be (List(0))
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0,1))
    g2.possibleEdges(2).sorted    should be (List(1))
    g2.requiredEdges(0).sorted 	  should be (List())
    g2.requiredEdges(1).sorted 	  should be (List())
    g2.requiredEdges(2).sorted 	  should be (List())
    
    val c : Constraint = new SubGraph(g1,g2) 
    postAndCheckSuspend(cp,c)
    // adding constraint set edge (0,1) in g2 required because (0,1) was required in g1
    g2.requiredEdges(0).sorted 	  should be (List(0))
    g2.requiredEdges(1).sorted 	  should be (List(0))
    g2.requiredEdges(2).sorted 	  should be (List())
    cp.isFailed should be (false)
    
    // failure because we want to remove a node that was mandatory
    postAndCheckFailure(cp, g2.removeEdge(0,1))
    cp.isFailed should be (true)
    }
    
    test("Test 14 : Test failure of posting constraint") {
    val cp = CPSolver()
    val nnodes1 : Int = 4
    val edges1 = List((1,3))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    postAndCheckSuspend(cp,g1.addEdge(1, 3))
    // g1 has a required edge (1,3) which is not part of g2 graph
    // posting constraint subgrah(g1,g2) should fail
    postAndCheckFailure(cp, new SubGraph(g1,g2))
    cp.isFailed should be (true)
    }

    test("Test 15 : Test possible values in g1 not in g2 domain") {
    val cp = CPSolver()
    val nnodes1 : Int = 5
    val edges1 = List((1,3))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(1,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    g1.possibleEdges(0).sorted    should be (List())
    g1.possibleEdges(1).sorted    should be (List(0))
    g1.possibleEdges(2).sorted    should be (List())
    g1.possibleEdges(3).sorted    should be (List(0))
    g1.possibleEdges(4).sorted    should be (List())
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0,1))
    g2.possibleEdges(2).sorted    should be (List(1))
    g1.possibleNodes	should be (0 to 4)
    g2.possibleNodes	should be (0 to 3)
    
    // g1 has a possible edge (1,3) which is not part of g2 graph
    // posting constraint subgrah(g1,g2) should remove it
    // g1 has a possible node 4 which is not part of g2 graph
    // posting constraint subgrah(g1,g2) should also remove it
    postAndCheckSuspend(cp,new SubGraph(g1,g2))
    g1.possibleEdges(0).sorted    should be (List())
    g1.possibleEdges(1).sorted    should be (List())
    g1.possibleEdges(2).sorted    should be (List())
    g1.possibleEdges(3).sorted    should be (List())
    g1.possibleEdges(4).sorted    should be (List())
    g2.possibleEdges(0).sorted    should be (List(0))
    g2.possibleEdges(1).sorted    should be (List(0,1))
    g2.possibleEdges(2).sorted    should be (List(1))
    g1.possibleNodes	should be (0 to 3)
    g2.possibleNodes	should be (0 to 3)
    }
    
}