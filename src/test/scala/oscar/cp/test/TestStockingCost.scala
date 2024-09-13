package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp._
import oscar.cp.core.CPPropagStrength

class TestStockingCost extends TestSuite {

  def StockingCostDecomp0(cp: CPSolver, X: Array[CPIntVar], d: Array[Int], H: CPIntVar, c: Int) = {

    val max = X.map(_.max).max
    val min = X.map(_.min).min

    for (t <- min to max) {
      cp.add((sum(0 until X.size)(k => X(k) ?=== t)) <= c)
    }

    cp.add((-sum(0 until X.size)(k => X(k) - d(k))) === H)

  }

  def StockingCostDecomp1(cp: CPSolver, X: Array[CPIntVar], d: Array[Int], H: CPIntVar, c: Int) = {
    cp.add(allDifferent(X),CPPropagStrength.Medium)
    cp.add((-sum(0 until X.size)(k => X(k) - d(k))) === H)
  }

  def StockingCostDecomp2(cp: CPSolver, X: Array[CPIntVar], d: Array[Int], H: CPIntVar, c: Int) = {

    val max = X.map(_.max).max
    val costMatrixStock = Array.tabulate(X.size) { k =>
      Array.tabulate(max + 1)(t => if (t <= d(k)) ((d(k) - t)) else 1000)
    }
    cp.add(minAssignment(X, costMatrixStock, H))

  }

  def nbSol(domX: Array[Set[Int]], d: Array[Int], domH: Set[Int], c: Int, decomp: Int = 0): (Int, Int, Int) = {
    var nbSol = 0
    val cp = CPSolver()

    val X = Array.tabulate(domX.size)(i => CPIntVar(domX(i))(cp))
    val H = CPIntVar(domH)(cp)

    if (decomp == 0) {
      StockingCostDecomp0(cp, X, d, H, c)
    } else if (decomp == 1) {
      StockingCostDecomp1(cp, X, d, H, c)
    } else if (decomp == 2) {
      StockingCostDecomp2(cp, X, d, H, c)
    } else {
      cp.add(stockingCost(X, d, H))
    }
    cp.search {
      binaryStatic(X)
    } onSolution {
      nbSol += 1
    }
    val stat = cp.start()
    //    println(stat)
    (nbSol, stat.nFails, stat.nNodes)
  }

  test("StockingCost1") {
    val x1 = (1 to 3).toSet
    val x2 = (1 to 6).toSet
    val x3 = (1 to 7).toSet
    val x4 = (1 to 7).toSet
    val x5 = (1 to 8).toSet
    val domX = Array(x1, x2, x3, x4, x5)
    val domH = (0 to 4).toSet
    val d = Array(3, 6, 7, 7, 8)

    var (nSol1, nSol2) = (0, 0)
    var (bkt1, bkt2) = (0, 0)
    var (nNode1, nNode2) = (0, 0)

    val t1 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, domH, 1, 2)
      nSol1 = a
      bkt1 = b
      nNode1 = c
    }
    val t2 = oscar.util.time {
      val (a, b, c) = nbSol(domX, d, domH, 1, 3)
      nSol2 = a
      bkt2 = b
      nNode2 = c
    }
    nSol1 should equal(nSol2)
  }

  val rand = new scala.util.Random(0)

  def randomDom(size: Int) = (1 to (rand.nextInt(size) + 5)).toSet

  test("StockingCost2") {
    var nbWins = 0
    var nbLoss = 0
    for (i <- 1 to 20) {

      val nbVars = 5
      val domVars = Array.fill(nbVars)(randomDom(size = nbVars))

      val d: Array[Int] = domVars.map(_.max)
      val domH = (0 to rand.nextInt(nbVars) + 20).toSet

//                        println("test num: " + i)
//                        for (x <- domVars) println(x.mkString(","))
//                        println("d: " + d.mkString(","))
//                        println("domH: " + domH)

      var (nSol0, nSol1, nSol2, nSol3) = (0, 0, 0, 0)
      var (bkt0, bkt1, bkt2, bkt3) = (0, 0, 0, 0)
      var (nNode0, nNode1, nNode2, nNode3) = (0, 0, 0, 0)
      
      val t0 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, d, domH, 1, 0)
        nSol0 = a
        bkt0 = b
        nNode0 = c
      }
      val t1 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, d, domH, 1, 1)
        nSol1 = a
        bkt1 = b
        nNode1 = c
      }
      val t2 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, d, domH, 1, 2)
        nSol2 = a
        bkt2 = b
        nNode2 = c
      }
      val t3 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, d, domH, 1, 3)
        nSol3 = a
        bkt3 = b
        nNode3 = c
      }

      nSol0 should equal(nSol1)
      nSol1 should equal(nSol2)
      nSol2 should equal(nSol3)
      
      //            bkt1 should lessThanOrEqual(bkt2)

      //      println("test num: " + i)
      //      for (x <- domVars) println(x.mkString(","))
      //      for (x <- predSuccSeq) println(x._1 + " " + x._2)
      //      println("time: " + t1 + " " + t2)
      //      println("nbBtk: " + bkt1 + " " + bkt2)
      //      println

      if (bkt3 > bkt1) {
        nbLoss += 1
        assert(false)
        //println("------------------->test failed")
        //        println("test num: " + i)
        //        println("nbWinnings: " + nbWins)
        //        println("nbLoss: " + nbLoss)
        //        for (x <- domVars) println(x.mkString(","))
        //        println("H: " + domH)
        //        println("time: " + t1 + " " + t2)
        //        println("nbBtk: " + bkt1 + " " + bkt2)
      } else if (bkt3 < bkt1) {
        nbWins += 1
        
        /*
        println("test num: " + i)
        println("nbWinnings: " + nbWins)
        println("nbLoss: " + nbLoss)
        for (x <- domVars) println(x.mkString(","))
        println("H: " + domH)
        println("const:\tbasic \tallDif \tminass \tstockingcost")
        println("time: \t" + t0 + "\t" + t1 + "\t" + t2 + "\t" + t3)
        println("nbBtk: \t" + bkt0 + "\t" + bkt1 + "\t" + bkt2 + "\t" + bkt3)
        println("nNode:\t" + nNode0 + "\t" + nNode1 + "\t" + nNode2 + "\t" + nNode3)
        println("nbSol:\t" + nSol0 + "\t" + nSol1 + "\t" + nSol2 + "\t" + nSol3)

        println(nNode3 + " & " + bkt3 + " & " + t3 + " & " + nNode2 + " & " + bkt2 + " & " + t2 + " & " +
          nNode1 + " & " + bkt1 + " & " + t1)

        println
        */
      }
    }
  }

}