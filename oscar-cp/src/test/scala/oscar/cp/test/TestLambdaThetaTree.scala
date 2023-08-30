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
 * @author: Pierre Schaus pschaus@gmail.com
 */
class TestLambdaThetaTree extends TestSuite {

  //todo: port this test to new version
 /*
  test("unary unit fig2.8 of petr vilim's thesis") {
    implicit val cp = CPSolver()
    val starts = Array(CPIntVar(0 to 100),CPIntVar(25 to 100),CPIntVar(30 to 100),CPIntVar(32 to 100))
    val durs = Array(CPIntVar(5),CPIntVar(6),CPIntVar(4),CPIntVar(10))
    val ends = Array(CPIntVar(0 to 5),CPIntVar(0 to 31),CPIntVar(0 to 34),CPIntVar(0 to 52))
    for (i <- 0 until starts.size) {
      add(starts(i) + durs(i) == ends(i))
    }
    val activities = Array.tabulate(starts.size)(i => new ActivityUnary(cp,starts(i),durs(i),ends(i),CPBoolVar(true),i))
    
    val ltTree = new LambdaThetaTreee2(4)
    ltTree.left(0) should be(1)
    ltTree.right(0) should be(2)
    
    ltTree.insert(activities(0), 0)
    ltTree.ect should be(5)
    ltTree.ectOpt should be(5)
    ltTree.sumP should be(5)
    ltTree.sumPOpt should be(5)

    
    ltTree.insert(activities(1), 1)
    ltTree.ect should be(31)
    ltTree.ectOpt should be(31)
    ltTree.sumP should be(11)
    ltTree.sumPOpt should be(11)
    
    
    ltTree.reset()
    ltTree.insert(activities(2), 2)
    ltTree.insert(activities(3), 3)
    ltTree.ect should be(44)
    ltTree.ectOpt should be(44)
    ltTree.sumP should be(14)
    ltTree.sumPOpt should be(14)

    ltTree.insert(activities(0), 0)
    ltTree.insert(activities(1), 1)
    ltTree.ect should be(45)
    ltTree.ectOpt should be(45)
    ltTree.sumP should be(25)
    ltTree.sumPOpt should be(25)

     
  }
  
  
  test("unary unit fig2.9 of petr vilim's thesis") {
    implicit val cp = CPSolver()
    val starts = Array(CPIntVar(0 to 100),CPIntVar(25 to 100),CPIntVar(30 to 100),CPIntVar(32 to 100))
    val durs = Array(CPIntVar(5),CPIntVar(6),CPIntVar(5),CPIntVar(10))
    val ends = Array(CPIntVar(0 to 5),CPIntVar(0 to 31),CPIntVar(0 to 35),CPIntVar(0 to 42))
    for (i <- 0 until starts.size) {
      add(starts(i) + durs(i) == ends(i))
    }
    val activities = Array.tabulate(starts.size)(i => new ActivityUnary(cp,starts(i),durs(i),ends(i),CPBoolVar(true),i))
    
    val ltTree = new LambdaThetaTreee2(4)
   
    ltTree.insert(activities(0), 0)
    ltTree.insert(activities(1), 1)
    ltTree.insert(activities(2), 2)
    ltTree.insert(activities(3), 3)
    ltTree.grey(2)
    
    ltTree.ect should be(42)
    ltTree.ectOpt should be(46)
    ltTree.sumP should be(21)
    ltTree.sumPOpt should be(26)
     
  }  */
 
}
