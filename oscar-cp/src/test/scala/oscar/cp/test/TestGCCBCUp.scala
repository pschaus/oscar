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
 * Testing of GCC

 *
 */

class TestGCCBCUp extends TestSuite {


    


  def nbSol(domX: Array[Set[Int]], values: Range, max: Array[Int], decomp: Boolean): (Int, Int, Int) = {
    var nbSol = 0
    val cp = CPSolver()

    val X = Array.tabulate(domX.size)(i => CPIntVar(domX(i))(cp))

    if (decomp) {

      cp.add(gcc(X,values,max.map(i => 0),max),Strong)

    } else {
      cp.add(new GCCUpperBC(X, values.min,max))
    }
    cp.search {
      binaryStatic(X)
    } onSolution {
      //             for (x <- X) print(x.mkString(","))
      //       println
      nbSol += 1
    }
    val stat = cp.start()
    (nbSol, stat.nFails, stat.nNodes)
  }

  val rand = new scala.util.Random(0)
  def randomDom(size: Int) = Array.fill(size)(rand.nextInt(size)).toSet

  test("GccBC2") {
    var nbWins = 0
    for (i <- 1 to 100) {
      val nbVars = 6

      val domVars = Array.fill(nbVars)(randomDom(size = nbVars))

      val min = domVars.flatten.min
      val max = domVars.flatten.max

      var bounds = (for (v <- min to max) yield rand.nextInt(nbVars - 4) +1).toArray

      var (nSol1, nSol2) = (0, 0)
      var (bkt1, bkt2) = (0, 0)
      var (nNode1, nNode2) = (0, 0)
      val t1 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, min to max, bounds, false)
        nSol1 = a
        bkt1 = b
        nNode1 = c
      }
      val t2 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, min to max, bounds, true)
        nSol2 = a
        bkt2 = b
        nNode2 = c
      }
      //println(nSol1)
      nSol1 should equal(nSol2)

        nbWins += 1
        

    }
  }

  test("GCCUpperBC 3") {
    implicit val cp = CPSolver()
    val domains = Array((0,1),
      (1,2),
      (2,2))
    val minval = 0
    val upperBound = Array(0,1,1,0)
    val x = domains.map(i => CPIntVar(i._1, i._2))
    val constraint = new GCCUpperBC(x, minval, upperBound)
    //Should not infinitely loop
  }

  test("GCCUpperBC 4") {
    implicit val cp = CPSolver()
    val domains = Array((0,1),
      (0,1),
      (1,2),
      (2,2))
    val minval = 0
    val upperBound = Array(0,1,1)
    val x = domains.map(i => CPIntVar(i._1, i._2))
    val constraint = new GCCUpperBC(x, minval, upperBound)
    //Should fail, but not infinite loop.
    isInconsistent(cp.post(constraint)) should equal(true)
  }

  test("GCCUpperBC 5") {
    implicit val cp = CPSolver()
    val domains = Array(
      (0,0),
      (0,2),
      (0,2),
      (2,2))
    val minval = 0
    val upperBound = Array(1,0,1)     //10-17
    val x = domains.map(i => CPIntVar(i._1, i._2))
    val constraint = new GCCUpperBC(x, minval, upperBound)
    //Should fail, but not infinite loop.
    isInconsistent(cp.post(constraint)) should equal(true)
  }

  test("GCCUpperBC 9") {
    implicit val cp = CPSolver()
    val domains = Array(
      (2,6),
      (0,0),
      (2,2),
      (3,3),
      (0,6),
      (3,6),
      (3,5),
      (0,2),
      (0,2),
      (0,2))
    val minval = 0
    val upperBound = Array(1,0,1,2,0,1,1)
    val x = domains.map(i => CPIntVar(i._1, i._2))
    val constraint = new GCCUpperBC(x, minval, upperBound)
    //Should fail, but not infinite loop.
    isInconsistent(cp.post(constraint)) should equal(true)
  }

  test("GCCUpperBC 6") {
    implicit val cp = CPSolver()
    val domains = Array(
      (2,6),
      (0,0),
      (6,6),
      (5,6),
      (2,2),
      (3,6),
      (3,3),
      (0,6),
      (3,6),
      (3,6),
      (3,5),
      (0,2),
      (0,2),
      (0,2))
    val minval = 0
    val upperBound = Array(1,0,1,2,0,1,1)
    val x = domains.map(i => CPIntVar(i._1, i._2))
    val constraint = new GCCUpperBC(x, minval, upperBound)
    //Should fail, but not infinite loop.
    isInconsistent(cp.post(constraint)) should equal(true)
  }

  test("GCCUpperBC 7") {
    implicit val cp = CPSolver()
    val domains = Array(
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,0),
      (0,0),
      (44,99),
      (0,50),
      (0,0),
      (44,99),
      (0,50),
      (0,0),
      (44,65),
      (32,32),
      (0,0),
      (44,65),
      (31,31),
      (0,0),
      (44,65),
      (31,31),
      (0,0),
      (41,60),
      (40,50),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (41,60),
      (0,0),
      (0,0),
      (40,56),
      (34,34),
      (0,0),
      (35,53),
      (35,35),
      (0,0),
      (35,53),
      (0,50),
      (0,0),
      (35,53),
      (35,50),
      (0,0),
      (35,53),
      (30,30),
      (0,0),
      (0,53),
      (0,0),
      (0,0),
      (0,53),
      (0,0),
      (0,0),
      (0,53),
      (0,0),
      (0,0),
      (0,53),
      (0,0),
      (0,0))
    val minval = 0
    val upperBound = Array(
      70,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      1,2,1,0,1,2,0,0,0,0,
      3,2,0,0,1,0,0,1,0,2,
      2,1,1,2,0,0,2,0,0,0,
      1,0,0,2,2,1,0,0,0,1,
      0,0,1,0,0,0,1,1,0,1,
      2,0,0,0,3,1,0,0,2,2,
      0,1,1,1,0,1,0,0,0,1)
    val x = domains.map(i => CPIntVar(i._1, i._2))
    val constraint = new GCCUpperBC(x, minval, upperBound)
    //Should fail, but not infinite loop.
    isInconsistent(cp.post(constraint)) should equal(true)
  }

  test("GCCUpperBC 8") {
    implicit val cp = CPSolver()
    val domains = Array(
      (0,0),
      (2,4),
      (3,3))
    val minval = 0
    val upperBound = Array(1,0,1,1,1)
    val x = domains.map(i => CPIntVar(i._1, i._2))
    val constraint = new GCCUpperBC(x, minval, upperBound)
    //Should not fail, not infinitely loop.
    isInconsistent(cp.post(constraint)) should equal(false)
  }
}
