/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp.constraints._
import oscar.cp._
import oscar.algo.search.SearchStatistics
import oscar.algo.DisjointSets
import oscar.algo.RangeMinQuery
import oscar.cp.core.CPPropagStrength

class TestHeldKarp extends TestSuite {

  class MinCircuit(succ: Array[CPIntVar], distMatrix: Array[Array[Int]], cost: CPIntVar) extends ChannelTSP(succ, distMatrix) {

    override def setup(l: CPPropagStrength): Unit = {
      //s.post(circuit(succ),l)
      s.post(new HeldKarp(edgeVar, edges, cost))
      super.setup(l)
    }

  }

  def bestTourSize(distMatrix: Array[Array[Int]]): Int = {
    val cp = CPSolver()
    cp.silent = true
    val n = distMatrix.size

    val succ = Array.fill(n)(CPIntVar(0 until n)(cp))
    cp.add(circuit(succ), Strong)
    val obj = sum(0 until n)(i => distMatrix(i)(succ(i)))
    cp.minimize(obj)
    cp.search(binaryFirstFail(succ))
    var best = Int.MaxValue
    cp.onSolution { best = obj.value }
    cp.start()
    best
  }

  def stat(distMatrixSucc: Array[Array[Int]], ub: Int, minAss: Boolean, heldKarp: Boolean): SearchStatistics = {
    val n = distMatrixSucc.size
    val distMatrixPred = Array.tabulate(n, n)((i, j) => distMatrixSucc(j)(i))

    implicit val cp = CPSolver()

    val succ = Array.fill(n)(CPIntVar(0 until n))
    val pred = Array.fill(n)(CPIntVar(0 until n))

    add(circuit(pred), Strong)
    add(circuit(succ), Strong)

    add(new Inverse(pred, succ))

    val obj = sum(0 until n)(i => distMatrixSucc(i)(succ(i)))

    add(sum(0 until n)(i => distMatrixPred(i)(pred(i))) === obj)
    add(obj <= ub)

    if (minAss) {
      add(minAssignment(succ, distMatrixSucc, obj))
      add(minAssignment(pred, distMatrixPred, obj))
    }

    if (heldKarp) {
      add(new MinCircuit(succ, distMatrixSucc, obj), Strong)
      //add(new MinCircuit(pred,distMatrixPred,obj),Strong)     
    }

    cp.search(binaryStaticIdx(succ,i => succ(i).min))
    val stat = cp.start()
    stat
  }
  /*
  test("HK1") {
    println("here")

      val distMatrix = Array(Array(0,3,8),
                             Array(5,0,5),
                             Array(5,0,0))
      val b = bestTourSize(distMatrix)
      println(b)
      val s1 = stat(distMatrix, b + 1, true, false)
      val s2 = stat(distMatrix, b + 1, true, true)
      if (s1.nSols != s2.nSols) {
        distMatrix.foreach(a => println(a.mkString("\t")))
      }
      println(s1.nSols + " " + s2.nSols)
      s1.nSols should be(s2.nSols)
    
  } */

  test("HK") {
    val rand = new scala.util.Random(0)

    for (i <- 0 until 100) {
      val distMatrix = Array.fill(6, 6)(rand.nextInt(10))
      val b = bestTourSize(distMatrix)
      val s1 = stat(distMatrix, b + 1, true, false)
      val s2 = stat(distMatrix, b + 1, true, true)
      if (s1.nSols != s2.nSols) {
        //distMatrix.foreach(a => println(a.mkString("\t")))
      }
      //println(s1.nSols + " " + s2.nSols)
      s1.nSols should be(s2.nSols)
    }
  }

  test("test DisjointSet and LCA computation") {

    val sets = new DisjointSets[CCTreeNode](0, 5)
    val cctree = new CCTree(6)
    sets.resetAndSetData(i => cctree.nodes(i))

    val t0 = sets.find(0).data.get
    val t1 = sets.find(1).data.get
    val t6 = cctree.merge(t0, t1, 6)
    sets.union(0, 1, t6)

    val t2 = sets.find(2).data.get
    val t3 = sets.find(3).data.get
    val t7 = cctree.merge(t2, t3, 7)
    sets.union(2, 3, t7)

    val t4 = sets.find(4).data.get
    val t5 = sets.find(5).data.get
    val t8 = cctree.merge(t4, t5, 8)
    sets.union(4, 5, t8)

    sets.find(0).data.get should be(t6)
    sets.find(2).data.get should be(t7)
    val t9 = cctree.merge(t6, t7, 9)
    sets.union(0, 2, t9)

    sets.find(0).data.get should be(t9)
    sets.find(4).data.get should be(t8)
    val t10 = cctree.merge(t9, t8, 10)
    sets.union(0, 2, t10)

    val inorder = cctree.inorderCollect()

    inorder.map(_.value).filter(_ > 0) should be(Array(6, 9, 7, 10, 8))

    //println("inorder:"+inorder.map(_.value).mkString(","))
    //println("inorder heights:"+inorder.map(_.height).mkString(","))

    val pos = Array.fill(11)(0)
    for (i <- 0 until 10) {
      pos(inorder(i).index) = i
    }
    val heights = inorder.map(_.height)
    val rmq = new RangeMinQuery(heights)
    //println("lca of 1,3 should be 9:"+inorder(rmq(pos(1),pos(3))).value)
    inorder(rmq(pos(1), pos(3))).value should be(9)
    inorder(rmq(pos(0), pos(4))).value should be(10)
    inorder(rmq(pos(2), pos(3))).value should be(7)
    inorder(rmq(pos(2), pos(4))).value should be(10)
  }
}
