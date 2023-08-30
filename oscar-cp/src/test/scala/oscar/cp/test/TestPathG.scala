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
import oscar.cp.constraints._
import oscar.cp._

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

class TestPathG extends TestSuite  {

  test("Test 1 : Test constraint initial propagation") {
    val cp = CPSolver()
    val nnodes : Int = 3
    val g = CPGraphVar(cp, nnodes)
    val i = CPIntVar(cp,0,10)
    // g.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    //               0     1     2     3     4     5
    
    // check that all is correct
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List())
    g.requiredEdges(0).sorted  should be (List())
    g.requiredEdges(1).sorted  should be (List())
    g.requiredEdges(2).sorted  should be (List())
    g.possibleEdges(0).sorted  should be (List(0,1,2,4))
    g.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g.possibleEdges(2).sorted  should be (List(1,3,4,5))
    
    // add constraint
    def myfun(u :Int,v : Int) : Int = 1
    postAndCheckSuspend(cp,new GraphPath(g,0,1,myfun,i))

    // check that all changes are correct acc. to definition:
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List(0,1))
    g.requiredEdges(0).sorted  should be (List())
    g.requiredEdges(1).sorted  should be (List())
    g.requiredEdges(2).sorted  should be (List())
    g.possibleEdges(0).sorted  should be (List(0,1))
    g.possibleEdges(1).sorted  should be (List(0,5))
    g.possibleEdges(2).sorted  should be (List(1,5))
  }
  
  test("Test 2 : Forbidden arcs") {
    // example from Dooms thesis p98
    val cp = CPSolver()
    val nnodes : Int = 7
    val edges = List((0,1),(1,2),(1,3),(2,3),(2,6),(3,4),(5,2))
    // edge nb :       0     1     2     3     4     5     6
    val g = CPGraphVar(cp, nnodes,edges)
    val i = CPIntVar(cp,0,10)
    
    // add a mandatory node
    postAndCheckSuspend(cp,g.addNode(2))
    
    // add constraint
    cp.post(new GraphPath(g,0,4, (u,v) => 1,i))

    //  check that all changes are correct acc. to definition:
    // 	  - remove edge 2:(1,3) because it skip required node 2 in path
    // 	  - remove edge 4:(2,6) because a path containing this edge cannot end with 4
    //    - remove edge 6:(5,2) because a path containing this edge cannot start from 0
    g.possibleEdges(0).sorted  should be (List(0))
    g.possibleEdges(1).sorted  should be (List(0,1))
    g.possibleEdges(2).sorted  should be (List(1,3))
    g.possibleEdges(3).sorted  should be (List(3,5))
    g.possibleEdges(4).sorted  should be (List(5))
    g.possibleEdges(5).sorted  should be (List())
    g.possibleEdges(6).sorted  should be (List())
  }
  
  test("Test 3 : Forbidden arcs with max dist") {
    val cp = CPSolver()
    val nnodes : Int = 4
    val g = CPGraphVar(cp, nnodes)
    val i = CPIntVar(cp,0,1)
    // g.edges are (0,1),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,1),(2,3),(3,0),(3,1),(3,2)
    //               0     1     2     3     4     5     6     7     8     9     10    11
    
    // add constraint and define weight function
    def myfun(u : Int, v : Int) : Int = 1
    cp.post(new GraphPath(g,0,2,myfun,i))

    //  check that all changes are correct acc. to definition:
    // 	  - remove all edges except 1:(0,2) which is the only simple path of length <= 1
    //    - 1:(0,2) is a bridge => mandatory
    g.possibleEdges(0).sorted  should be (List(1))
    g.possibleEdges(1).sorted  should be (List())
    g.possibleEdges(2).sorted  should be (List(1))
    g.possibleEdges(3).sorted  should be (List())
    g.requiredEdges(0).sorted  should be (List(1))
    g.requiredEdges(1).sorted  should be (List())
    g.requiredEdges(2).sorted  should be (List(1))
    g.requiredEdges(3).sorted  should be (List())
  }
  
   test("Test 4 : Forbidden arcs with max dist (2) : more complicated example") {
    val cp = CPSolver()
    val nnodes : Int = 6
    val edges = List((0,1),(0,3),(1,2),(1,3),(1,4),(2,4),(2,5),(3,4),(4,5))
    // edge nb :       0     1     2     3     4     5     6     7     8
    val g = CPGraphVar(cp, nnodes,edges)
    val i = CPIntVar(cp,0,5)
    
    // add constraint and define weight function
    def myfun(u : Int, v : Int) : Int = {
      (u,v) match {
        case (1,2) => 2
        case (2,4) => 2
        case (3,4) => 2
        case (4,5) => 3
        case (_,_) => 1
      }
    }
    postAndCheckSuspend(cp,new GraphPath(g,0,5,myfun,i))

    //  check that all changes are correct acc. to definition:
    // 	  - remove edges 1:(0,3), 3:(1,3), 5:(2,4), 7:(3,4) because no path from length <= 5 using these edges
    //    - as 0:(0,1) is a bridge => mandatory
    g.possibleEdges(0).sorted  should be (List(0))
    g.possibleEdges(1).sorted  should be (List(0,2,4))
    g.possibleEdges(2).sorted  should be (List(2,6))
    g.possibleEdges(3).sorted  should be (List())
    g.possibleEdges(4).sorted  should be (List(4,8))
    g.possibleEdges(5).sorted  should be (List(6,8))
    g.requiredEdges(0).sorted  should be (List(0))
    g.requiredEdges(1).sorted  should be (List(0))
    g.requiredEdges(2).sorted  should be (List())
    g.requiredEdges(3).sorted  should be (List())
    g.requiredEdges(4).sorted  should be (List())
    g.requiredEdges(5).sorted  should be (List())
  }
   
   test("Test 5 : Forbidden nodes") {
    // example from Dooms thesis p98
    val cp = CPSolver()
    val nnodes : Int = 7
    val edges = List((0,1),(1,2),(1,3),(2,3),(2,6),(3,4),(5,2))
    // edge nb :       0     1     2     3     4     5     6
    val g = CPGraphVar(cp, nnodes,edges)
    val i = CPIntVar(cp,0,10)
    
    // add a mandatory node
    postAndCheckSuspend(cp,g.addNode(2))
    
    // add constraint
    cp.post(new GraphPath(g,0,4, (u,v) => 1,i))

    //  check that all changes are correct acc. to definition:
    // 	  - as we removed edges 2:(1,3), 4:(2,6) and 6:(5,2),
    //         nodes 5 and 6 are isolated and removed
    g.possibleNodes.sorted should be (List(0,1,2,3,4))
  }
   
   test("Test 6 : Forbidden nodes with max dist") {
    val cp = CPSolver()
    val nnodes : Int = 4
    val g = CPGraphVar(cp, nnodes)
    val i = CPIntVar(cp,0,1)
    // g.edges are (0,1),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,1),(2,3),(3,0),(3,1),(3,2)
    //               0     1     2     3     4     5     6     7     8     9     10    11
    
    // add constraint and define weight function
    def myfun(u : Int, v : Int) : Int = 1
    cp.post(new GraphPath(g,0,2,myfun,i))

    //  check that all changes are correct acc. to definition:
    // 	  - as we removed all edges except 1:(0,2) which is the only simple path of length <= 1,
    //       nodes 1 and 3 are isolated and should be removed
    g.possibleNodes.sorted should be (List(0,2))
    g.requiredNodes.sorted should be (List(0,2))
  }
   
   test("Test 7 : Mandatory nodes") {
    val cp = CPSolver()
    val nnodes : Int = 4
    val edges = List((0,1),(0,2),(0,3),(1,2),(2,3))
    val g = CPGraphVar(cp, nnodes,edges)
    val i = CPIntVar(cp,0,10)
    // g.edges are (0,1),(0,2),(0,3),(1,2),(2,3)
    //               0     1     2     3     4
    
    // add constraint and define weight function
    def myfun(u : Int, v : Int) : Int = 1
    cp.post(new GraphPath(g,0,3,myfun,i))
    
    g.possibleNodes.sorted should be (List(0,1,2,3))
    g.requiredNodes.sorted should be (List(0,3))

    // add node 1
    postAndCheckSuspend(cp,g.addNode(1))
    
    //  check that all changes are correct acc. to definition:
    //    - add mandatory node 2
    g.possibleNodes.sorted should be (List(0,1,2,3))
    g.requiredNodes.sorted should be (List(0,1,2,3))
  }

   test("Test 8 : Mandatory Edges") {
    val cp = CPSolver()
    val nnodes : Int = 4
    val edges = List((0,1),(0,2),(1,2),(1,3),(2,3))
    val g = CPGraphVar(cp, nnodes,edges)
    val i = CPIntVar(cp,0,10)
    // g.edges are (0,1),(0,2),(1,2),(1,3),(2,3)
    //               0     1     2     3     4
    
    // add constraint and define weight function
    def myfun(u : Int, v : Int) : Int = 1
    postAndCheckSuspend(cp,new GraphPath(g,0,3,myfun,i))
    
    g.possibleNodes.sorted should be (List(0,1,2,3))
    g.requiredNodes.sorted should be (List(0,3))
    g.possibleEdges(0).sorted  should be (List(0,1))
    g.possibleEdges(1).sorted  should be (List(0,2,3))
    g.possibleEdges(2).sorted  should be (List(1,2,4))
    g.possibleEdges(3).sorted  should be (List(3,4))
    for (n <- 0 to nnodes-1) g.requiredEdges(n).sorted  should be (List())

    // add nodes
    postAndCheckSuspend(cp,g.addNode(1))
    postAndCheckSuspend(cp,g.addNode(2))
    
    //  check that all changes are correct acc. to definition:
    // 	  - remove 1:(0,2),3:(1,3)
    //    - add mandatory 2:(1,2); 0:(0,1), 4:(2,3)
    g.possibleNodes.sorted should be (List(0,1,2,3))
    g.requiredNodes.sorted should be (List(0,1,2,3))
    g.possibleEdges(0).sorted  should be (List(0))
    g.possibleEdges(1).sorted  should be (List(0,2))
    g.possibleEdges(2).sorted  should be (List(2,4))
    g.possibleEdges(3).sorted  should be (List(4))
    g.requiredEdges(0).sorted  should be (List(0))
    g.requiredEdges(1).sorted  should be (List(0,2))
    g.requiredEdges(2).sorted  should be (List(2,4))
    g.requiredEdges(3).sorted  should be (List(4))
  }
}