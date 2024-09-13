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

class TestSpread extends TestSuite {

  test("Spread 1") {
    val cp = CPSolver()
    var x = Array.fill(5)(CPIntVar(0 to 4)(cp))
    var s2 = CPIntVar(0 to 1000)(cp)
    cp.add(new Spread(x, 10, s2))
    s2.min should be(20)
  }

  test("Spread 2") {
    val cp = CPSolver()
    var x = Array.fill(5)(CPIntVar(0 to 4)(cp))
    var s2 = CPIntVar(0 to 1000)(cp)
    cp.add(new Spread(x, 11, s2))
    s2.min should be(25)
  }

  test("Spread 3") {
    val cp = CPSolver()
    var x = Array(CPIntVar(-1 to 0)(cp),
      CPIntVar(-2 to -1)(cp),
      CPIntVar(-3 to -2)(cp))

    var s2 = CPIntVar(0 to 1000)(cp)
    cp.add(new Spread(x, -4, s2))

    s2.min should be(6)
  }

  test("Spread 4") {
    val cp = CPSolver()
    var x = Array(CPIntVar(-1 to 0)(cp),
      CPIntVar(-3 to -2)(cp),
      CPIntVar(-3 to -2)(cp))

    var s2 = CPIntVar(0 to 1000)(cp)
    cp.add(new Spread(x, -5, s2))
    s2.min should be(9)
  }

  test("Spread 5") {
    val cp = CPSolver()

    var x = Array(CPIntVar(103)(cp),
      CPIntVar(99)(cp),
      CPIntVar(90 to 98)(cp),
      CPIntVar(72 to 82)(cp),
      CPIntVar(78)(cp),
      CPIntVar(65)(cp),
      CPIntVar(73)(cp))

    var s2 = CPIntVar(0 to x.map(y => y.max*y.max).sum)(cp)
    cp.add(new Spread(x, 591, s2))
    cp.isFailed should be(false)
  }

  test("Spread 6") {
    val cp = CPSolver()

    // 4 vars, with dom 0 to 100
    // sum should be 8 (2 on average)

    var x = Array(CPIntVar(0 to 100)(cp),
      CPIntVar(0 to 100)(cp),
      CPIntVar(0 to 100)(cp),
      CPIntVar(0 to 100)(cp))

    var s2 = CPIntVar(0 to 16)(cp)
    cp.add(new Spread(x, 8, s2))
    cp.isFailed should be(false)
    s2.min should be(16)
    for (y <- x) {
      y.max should be(2)
    }
  }

  test("Spread 7") {
    val cp = CPSolver()

    var x = Array(CPIntVar(0 to 0)(cp),
      CPIntVar(0 to 100)(cp),
      CPIntVar(0 to 100)(cp),
      CPIntVar(0 to 100)(cp))
    // optimal assignment will be 0,2,3,3                   
    var s2 = CPIntVar(0 to 22)(cp)
    cp.add(new Spread(x, 8, s2))
    cp.isFailed should be(false)
    s2.min should be(22)
    x(1).max should be(3)
    x(2).max should be(3)
    x(3).max should be(3)

  }

  test("Spread 8") {
    val cp = CPSolver()

    var x = Array(CPIntVar(0 to 0)(cp),
      CPIntVar(1 to 2)(cp),
      CPIntVar(0 to 100)(cp),
      CPIntVar(0 to 100)(cp))
    // optimal assignment will be 0,2,3,3                   
    var s2 = CPIntVar(0 to 22)(cp)
    cp.add(new Spread(x, 8, s2))
    cp.isFailed should be(false)
    s2.min should be(22)
    x(1).min should be(2)
    x(2).min should be(3)
    x(3).min should be(3)
    x(2).max should be(3)
    x(3).max should be(3)
  }

  test("Spread 9") {
    val cp = CPSolver()

    var x = Array(CPIntVar(0 to 0)(cp),
      CPIntVar(1 to 2)(cp),
      CPIntVar(4 to 100)(cp),
      CPIntVar(0 to 100)(cp))
    // optimal assignment will be 0,2,4,2                   
    var s2 = CPIntVar(0 to 24)(cp)
    cp.add(new Spread(x, 8, s2))
    cp.isFailed should be(false)
    s2.min should be(24)
    x(1).min should be(2)
    x(2).min should be(4)
    x(3).min should be(2)
    x(2).max should be(4)
    x(3).max should be(2)
  }

  test("Spread 10") {
    val cp = CPSolver()

    var x = Array(CPIntVar(0 to 0)(cp),
      CPIntVar(1 to 2)(cp),
      CPIntVar(4 to 100)(cp),
      CPIntVar(0 to 100)(cp))
    // optimal assignment will be 0,2,4,2 => let's allow this one 0,1,4,3 with s2=26                   
    var s2 = CPIntVar(0 to 26)(cp)
    cp.add(new Spread(x, 8, s2))
    cp.isFailed should be(false)
    s2.min should be(24)
    x(1).min should be(1)
    x(2).max should be(4)
    x(3).max should be(3)
  }

  test("Spread 11") {
    val cp = CPSolver()

    var x = Array(CPIntVar(0 to 0)(cp),
      CPIntVar(-2 to -1)(cp),
      CPIntVar(-100 to -4)(cp),
      CPIntVar(-100 to 0)(cp))
    // optimal assignment will be 0,-2,-4,-2 => let's allow this one 0,-1,-4,-3 with s2=26                   
    var s2 = CPIntVar(0 to 26)(cp)
    cp.add(new Spread(x, -8, s2))
    cp.isFailed should be(false)
    s2.min should be(24)
    x(1).min should be(-2)
    x(2).min should be(-4)
    x(3).min should be(-3)
  }

  test("Spread 12") {
    val cp = CPSolver()

    var x = Array(CPIntVar(0 to 0)(cp),
      CPIntVar(1 to 3)(cp),
      CPIntVar(4 to 100)(cp),
      CPIntVar(0 to 100)(cp))
    // optimal assignment will be 0,3,4,3 s2=18+16=34                    
    var s2 = CPIntVar(0 to 34)(cp)
    cp.add(new Spread(x, 10, s2))
    cp.isFailed should be(false)
    s2.min should be(34)

    x(1).min should be(3)
    x(2).max should be(4)
    x(3).min should be(3)
    x(3).max should be(3)
  }

  test("Spread 13") {
    val cp = CPSolver()

    var x = Array(CPIntVar(0 to 0)(cp),
      CPIntVar(1 to 3)(cp),
      CPIntVar(4 to 100)(cp),
      CPIntVar(0 to 100)(cp))
    // optimal assignment will be 0,3,4,3, we want to allow this one 0,2,4,4 with s2 = 36                     
    var s2 = CPIntVar(0 to 36)(cp)
    cp.add(new Spread(x, 10, s2))
    cp.isFailed should be(false)
    s2.min should be(34)

    x(1).min should be(2)
    x(2).max should be(4)
    x(3).min should be(3)
    x(3).max should be(4)
  }

  test("Spread 14") {

    val doms = Array((1, 3), (4, 100), (0, 2), (-2, -2), (10, 20))

    nbSol(doms, 16, 130, false) should be(nbSol(doms, 16, 130, true))
    nbSol(doms, 16, 140, false) should be(nbSol(doms, 16, 140, true))
    nbSol(doms, 16, 150, false) should be(nbSol(doms, 16, 150, true))

    nbSol(doms, 14, 130, false) should be(nbSol(doms, 14, 130, true))
    nbSol(doms, 14, 140, false) should be(nbSol(doms, 14, 140, true))
    nbSol(doms, 14, 150, false) should be(nbSol(doms, 14, 150, true))

    nbSol(doms, 13, 130, false) should be(nbSol(doms, 13, 130, true))
    nbSol(doms, 13, 140, false) should be(nbSol(doms, 13, 140, true))
    nbSol(doms, 13, 150, false) should be(nbSol(doms, 13, 150, true))
    
    val doms2 = Array.fill(4)((-100,100))
    nbSol(doms2, 0, 192, false) should be(nbSol(doms2, 0, 192, true))
    
    val doms3 = Array.fill(5)((-100,100))
    nbSol(doms3, 0, 18, false) should be(nbSol(doms3, 0, 18, true))
    
    val doms4 = Array.fill(6)((-100,100))
    nbSol(doms4, 0, 14, false) should be(nbSol(doms4, 0, 14, true))
    
    val doms5 = Array.fill(7)((-100,100))
    nbSol(doms5, 0, 10, false) should be(nbSol(doms5, 0, 10, true))  
    
    val doms6 = Array.fill(8)((-100,100))
    nbSol(doms6, 0, 8, false) should be(nbSol(doms6, 0, 8, true))   
 
    val doms7 = Array.fill(10)((-100,100))
    nbSol(doms7, 0, 6, false) should be(nbSol(doms7, 0, 6, true)) 
    
    nbSol(doms7, 0, 8, false) should be(nbSol(doms7, 0, 8, true)) 

  }

  test("Spread 15") {
    // submitted by Janho

    val cp = CPSolver();
    val nd = CPIntVar(0 to 5)(cp)
    val rng = 0 until 5;
    val x = for (i <- rng) yield CPIntVar(-10 to 10)(cp)
    cp.add(new Spread(x, 0, nd))
    for (y <- x) {
      y.max should be(1)
      y.min should be(-1)
    }
  }



  def decomposition(x: Array[CPIntVar], sum: Int, sum2: CPIntVar): Unit = {
    sum2.store.post(new Sum(x.map(i => i * i), sum2))
    sum2.store.post(new Sum(x, CPIntVar(sum)(sum2.store)))
  }

  def nbSol(doms: Array[(Int, Int)], sum: Int, sum2Max: Int, decomp: Boolean): Int = {
    var nbSol = 0
    val cp = CPSolver()

    val x = doms.map { case (min, max) => CPIntVar(min to max)(cp) }
    val sum2 = CPIntVar(0 to sum2Max)(cp)

    if (decomp) {
      decomposition(x, sum, sum2)
    } else {
      cp.add(new Spread(x, sum, sum2))
    }
    cp.search {
      binaryFirstFail(x)
    } 
    val stat = cp.start()
    stat.nSols
  }

}
