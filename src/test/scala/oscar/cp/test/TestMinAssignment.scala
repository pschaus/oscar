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
import oscar.cp.constraints.{MinAssignment}

/**
  * Test for Minimum Assignment (or weighted matching)
  *
  * @author Pierre Schaus pschaus@gmail.com
  */
class TestMinAssignment extends TestSuite {

  test("Test Assignment 1") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 2)(cp))
    cp.post(allDifferent(x),Strong)
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(7)
    cp.post(x(0) !== 0)
    cost.min should be(8)
    cp.post(x(2) !== 2)
    cost.min should be(10)
  }
  

  test("Test Assignment 2") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array(CPIntVar(Set(1,2))(cp), CPIntVar(Set(0,1,2))(cp), CPIntVar(Set(0,1))(cp))
    cp.post(allDifferent(x),Strong)
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(10)
  }
  
  
  test("Test Assignment 3") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array(CPIntVar(Set(0,1,2))(cp), CPIntVar(Set(0,1,2))(cp), CPIntVar(Set(0,1))(cp))
    cp.post(allDifferent(x),Strong)
    cp.post(new MinAssignment(x, w, cost))
    //println(cost)
    cost.min should be(10)
  }  
  
  test("Test Assignment 4") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 2)(cp))
    cp.post(allDifferent(x),Strong)
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(7)
    cp.pushState()
    cp.post(x(0) !== 0)
    cost.min should be(8)
    cp.pop()
    cp.post(x(2) !== 2)
    cost.min should be(10)
  }
  
  
  test("Test Assignment 5") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 2)(cp))
    cp.post(allDifferent(x),Strong)
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(7)
    cp.post(cost <= 7)
    //println(x.mkString(","))
    x(0).value should be(0)
    //x(1).value should be(1) // if we do AC
    x(2).value should be(2)
  } 
  
  test("Test Assignment 6") {
    val cp = CPSolver()
    val w = Array(Array(0, 1, 2), 
                  Array(0, 0, 0), 
                  Array(2, 3, 0))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 2)(cp))
    cp.post(allDifferent(x),Strong)
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(0)
    cp.post(cost <= 0)
    //println(x.mkString(","))
    x(0).value should be(0)
    //x(1).value should be(1) // if we do AC
    x(2).value should be(2)
  }
  
  test("Test Assignment 7") {
    val cp = CPSolver()
    val w = Array(Array(0, 1, 2, 3), 
                  Array(1, 1, 0, 0), 
                  Array(2, 3, 0, 3))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 3)(cp))
    cp.post(allDifferent(x),Strong)
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(0)
    cp.post(cost <= 0)
    x(0).value should be(0)
    x(1).value should be(3)
    x(2).value should be(2)
  }


  test("Test random min assignment") {

    for (i <- 0 until 200) {
      val cp = CPSolver()
      cp.silent = true
      val rand = new scala.util.Random(i)
      val n = 5
      val w = Array.tabulate(n,n){case(i,j) => rand.nextInt(20)}
      val x = Array.fill(n)(CPIntVar(0 until n)(cp))



      val cost = CPIntVar(0 to 300)(cp)

      cp.add(allDifferent(x))
      cp.add(sum(0 until n)(i => w(i)(x(i))) === cost)


      cp.search {
        binaryStatic(x)
      }

      cp.onSolution {
        //"solution obj = "+cost
      }

      cp.minimize(cost)

      val stat1 = cp.start()


      cp.obj(cost).relax()

      // println("----------------------")

      val stat2 = cp.startSubjectTo() {
        cp.add(new MinAssignment(x, w, cost),Weak)
      }

      cp.obj(cost).relax()

      val stat3 = cp.startSubjectTo() {
        cp.add(new MinAssignment(x, w, cost),Strong)
      }



      assert(stat1.nSols == stat2.nSols)
      assert(stat1.nSols == stat3.nSols)

      assert(stat2.nFails <= stat1.nFails)
      assert(stat3.nFails <= stat2.nFails)


    }

  }



}
