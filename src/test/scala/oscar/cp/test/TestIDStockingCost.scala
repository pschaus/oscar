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
import oscar.cp.constraints.IDStockingCost
import oscar.cp.core.CPPropagStrength
import oscar.cp.testUtils._

import scala.collection.immutable.SortedSet


class TestIDStockingCost extends TestSuite  {


  def IDStockingCostDecomp0(cp: CPSolver, X: Array[CPIntVar], d: Array[Int], h: Array[Int], H: CPIntVar, c: Int) = {
    val max = X.map(_.max).max
    val min = X.map(_.min).min

    for (t <- min to max) {
      cp.add((sum(0 until X.size)(k => X(k) ?=== t)) <= c)
    }
    cp.add((-sum(0 until X.size)(k => (X(k) - d(k)) * h(k))) ?<= H)
  }

  def IDStockingCostDecomp1(cp: CPSolver, X: Array[CPIntVar], d: Array[Int], h: Array[Int], H: CPIntVar, c: Int) = {
    cp.add(allDifferent(X),Medium)
    cp.add((-sum(0 until X.size)(k => (X(k) - d(k)) * h(k))) ?<= H)
  }

  def nbSol(domX: Array[Set[Int]], d: Array[Int], h: Array[Int], domH: Set[Int], c: Int, decomp: Int = 0): (Int, Int, Int) = {
    var nbSol = 0
    val cp = CPSolver()

    val X = Array.tabulate(domX.size)(i => CPIntVar(domX(i))(cp))
    val H = CPIntVar(domH)(cp)

    if (decomp == 0) {
      IDStockingCostDecomp0(cp, X, d, h, H, c)
    } else if (decomp == 1) {
      IDStockingCostDecomp1(cp, X, d, h, H, c)
    }
    else {
      //cp.add(allDifferent(X),Weak)
      val max = X.map(_.max).max
      val cap = Array.tabulate(max + 1)(t => 1)

      cp.add(new IDStockingCost(X, d, h, H, cap))
    }

    cp.search {
      binaryStatic(X)
    } onSolution {
      nbSol += 1
    }
    val stat = cp.start()
    (nbSol, stat.nFails, stat.nNodes)
  }

  test("IDStockingCost1") {
    val x1 = (2 to 2).toSet
    val x2 = (5 to 5).toSet
    val x3 = (4 to 4).toSet
    val x4 = (1 to 5).toSet
    val x5 = (7 to 8).toSet
    val x6 = (3 to 8).toSet
    val domX = Array(x1, x2, x3, x4, x5, x6)
    val domH = (0 to 38).toSet
    val d = Array(4, 5, 5, 5, 8, 8)
    val h = Array(3, 10, 4, 2, 2, 4)

    var (nSol1, nSol2) = (0, 0)
    var (bkt1, bkt2) = (0, 0)
    var (nNode1, nNode2) = (0, 0)

    val t1 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, h, domH, 1, 1)
      nSol1 = a
      bkt1 = b
      nNode1 = c
    }
    val t2 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, h, domH, 1, 3)
      nSol2 = a
      bkt2 = b
      nNode2 = c
    }
    nSol1 should equal(nSol2)
    //    println("nbkt1: " + bkt1 + " nbkt2: " + bkt2)
  }

  test("IDStockingCost2") {
    val x1 = (1 to 4).toSet
    val x2 = (1 to 5).toSet
    val x3 = (1 to 5).toSet
    val x4 = (1 to 5).toSet
    val x5 = (1 to 8).toSet
    val x6 = (1 to 8).toSet
    val domX = Array(x1, x2, x3, x4, x5, x6)
    val domH = (0 to 38).toSet
    val d = Array(4, 5, 5, 5, 8, 8)
    val h = Array(3, 10, 4, 2, 2, 4)

    var (nSol1, nSol2) = (0, 0)
    var (bkt1, bkt2) = (0, 0)
    var (nNode1, nNode2) = (0, 0)

    val t1 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, h, domH, 1, 1)
      nSol1 = a
      bkt1 = b
      nNode1 = c
    }
    val t2 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, h, domH, 1, 3)
      nSol2 = a
      bkt2 = b
      nNode2 = c
    }
    nSol1 should equal(nSol2)
    //    println("nbkt1: " + bkt1 + " nbkt2: " + bkt2)
  }

  test("IDStockingCost3") {
    val x1 = (3 to 4).toSet
    val x2 = (6 to 7).toSet
    val x3 = (1 to 6).toSet
    val x4 = (1 to 7).toSet
    val x5 = (1 to 8).toSet

    val domX = Array(x1, x2, x3, x4, x5)
    val domH = (0 to 24).toSet
    val d = Array(4, 7, 6, 7, 8)
    val h = Array(5, 5, 7, 9, 9)

    var (nSol1, nSol2) = (0, 0)
    var (bkt1, bkt2) = (0, 0)
    var (nNode1, nNode2) = (0, 0)

    val t1 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, h, domH, 1, 1)
      nSol1 = a
      bkt1 = b
      nNode1 = c
    }
    val t2 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, h, domH, 1, 3)
      nSol2 = a
      bkt2 = b
      nNode2 = c
    }
    nSol1 should equal(nSol2)
    //    println("nbkt1: " + bkt1 + " nbkt2: " + bkt2)
  }

  test("IDStockingCost4") {
    val x1 = (2 to 2).toSet
    val x2 = (5 to 5).toSet
    val x3 = (3 to 3).toSet
    val x4 = (1 to 4).toSet
    val x5 = (8 to 8).toSet
    val x6 = (3 to 8).toSet
    val domX = Array(x1, x2, x3, x4, x5, x6)
    val domH = (0 to 38).toSet
    val d = Array(4, 5, 5, 5, 8, 8)
    val h = Array(3, 10, 4, 2, 2, 4)

    var (nSol1, nSol2) = (0, 0)
    var (bkt1, bkt2) = (0, 0)
    var (nNode1, nNode2) = (0, 0)

    val t1 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, h, domH, 1, 1)
      nSol1 = a
      bkt1 = b
      nNode1 = c
    }
    val t2 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, h, domH, 1, 3)
      nSol2 = a
      bkt2 = b
      nNode2 = c
    }
    nSol1 should equal(nSol2)
    //    println("nbkt1: " + bkt1 + " nbkt2: " + bkt2)
  }

  val rand = new scala.util.Random(0)
  def randomDom(size: Int) = (1 to (rand.nextInt(size) + 4)).toSet
  test("IDStockingCost5") {
    var nbWins = 0
    for (i <- 1 to 300) {

      val nbVars = 5
      val domVars = Array.fill(nbVars)(randomDom(size = nbVars))
      val d: Array[Int] = domVars.map(_.max)
      val h: Array[Int] = Array.tabulate(nbVars)(i => rand.nextInt(nbVars + 1) + 1)
      val domH = (0 to rand.nextInt(nbVars) + 25).toSet

      var (nSol0, nSol1, nSol2, nSol3) = (0, 0, 0, 0)
      var (bkt0, bkt1, bkt2, bkt3) = (0, 0, 0, 0)
      var (nNode0, nNode1, nNode2, nNode3) = (0, 0, 0, 0)

      val t0 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, d, h, domH, 1, 0)
        nSol0 = a
        bkt0 = b
        nNode0 = c
      }
      val t1 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, d, h, domH, 1, 1)
        nSol1 = a
        bkt1 = b
        nNode1 = c
      }
      val t2 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, d, h, domH, 1, 2)
        nSol2 = a
        bkt2 = b
        nNode2 = c
      }
      val t3 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, d, h, domH, 1, 3)
        nSol3 = a
        bkt3 = b
        nNode3 = c
      }

      nSol0 should equal(nSol1)
      nSol1 should equal(nSol2)
      nSol2 should equal(nSol3)

      //bkt2 should equal(bkt3)

    }
  }

  


}
