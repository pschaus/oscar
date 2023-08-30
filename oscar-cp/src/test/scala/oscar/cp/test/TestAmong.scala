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
import oscar.cp.testUtils._
import oscar.cp.constraints.Among

class TestAmong extends TestSuite {
  
  val rand = new scala.util.Random(0)
  
  def randomDom: Set[Int] = Array.fill(10)(rand.nextInt(10)).toSet
  
  def amongDecomp(cp: CPSolver, N: CPIntVar, X: Array[CPIntVar], S: Set[Int]) = {
    val counts = Array.fill(S.size)(CPIntVar(0 to X.size)(cp))
    cp.add(gcc(X,S zip counts))
    cp.add(sum(counts) === N)
  }
  
  def nbSol(nmin: Int, nmax: Int, domx: Array[Set[Int]], S: Set[Int], decomp: Boolean = false): Int = {
	  implicit val cp = CPSolver()
	  val N = CPIntVar(nmin to nmax)
	  val X = Array.tabulate(domx.size)(i => CPIntVar(domx(i)))
	  if (decomp) amongDecomp(cp,N,X,S)
	  else cp.add(new Among(N,X,S))
	  search(binaryStatic(X :+ N))
	  start().nSols
  }
   
  test("among1") { 
	  implicit val cp = CPSolver()
	  val S = Set(1,2,3)
	  val N = CPIntVar(2)
	  val X = Array.fill(5)(CPIntVar(0 to 5))
	  add(new Among(N,X,S))
	  search { binaryStatic(X) } 
    onSolution { X.map(_.value).count(v => S.contains(v)) should equal(N.value) } 
    start() 
  }
  
  test("among2") {
    for (i <- 0 to 5) {
      val X = Array.fill(6)(randomDom)
      val S = Set(1, 4, 5)
      nbSol(4, 5, X, S, false) should equal(nbSol(4, 5, X, S, true))
    }
  }
}
