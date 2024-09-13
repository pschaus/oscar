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

import oscar.cp._
import oscar.cp.constraints.AtMostNValue
import oscar.cp.testUtils._

class TestAtMostNValue extends TestSuite {
  
  val rand = new scala.util.Random(0)

  // test with minVal != 0
  def randomDom: Set[Int] = Array.fill(10)(3+rand.nextInt(8)).toSet
  
  def atMostNValueDecomp(cp: CPSolver, X: Array[CPIntVar], N: CPIntVar) = {
    val minVal = X.map(_.min).min
    val maxVal = X.map(_.max).max

    val present: Array[CPBoolVar] = (minVal to maxVal).toArray.map(v => isOr(X.map(_ ?=== v)))

    cp.add(sum(present) <= N)
  }



  def nbSol(domx: Array[Set[Int]],Nmin: Int, Nmax: Int, decomp: Boolean = false): Int = {
	  implicit val cp = CPSolver()
	  val N = CPIntVar(Nmin to Nmax)
	  val X = Array.tabulate(domx.size)(i => CPIntVar(domx(i)))
	  if (decomp) atMostNValueDecomp(cp,X,N)
	  else cp.add(new AtMostNValue(X,N))
	  search(binaryStatic(X :+ N))
	  val stat = start()
    stat.nSols
  }


  test("atMost1") {
	  implicit val cp = CPSolver()
	  val N = CPIntVar(2)
	  val X = Array.fill(6)(CPIntVar(0 to 10))
	  add(new AtMostNValue(X,CPIntVar(4)))
	  search { binaryStatic(X) }
    onSolution { assert(X.map(_.value).toSet.size <= 4) }
    start()
  }
  

  for (i <- 0 to 100) {
    test("atMost2"+i) {
      val X = Array.fill(6)(randomDom)
      nbSol(X, 2, 3, false) should equal(nbSol(X, 2, 3, true))
    }
  }

  test("AtMost5-AlreadyOver") {
    implicit val cp = CPSolver()
    val N = CPIntVar(5)
    val X = Array.tabulate(6)(i => CPIntVar(i))
    intercept[NoSolutionException] {
      cp.add(new AtMostNValue(X,N))
    }
  }
}
