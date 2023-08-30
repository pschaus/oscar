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
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.testUtils._
import oscar.cp.constraints._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Tests for the various approaches to solving the PrefixCC problem.
 * See https://github.com/vlecomte/prefixcc-tech-report/blob/master/report.pdf for the problem statement.
 * @author Victor Lecomte
 */
class TestPrefixCC extends TestSuite {

  val MULTIPLE_GCC_AC = 0
  val MULTIPLE_GCC_FWC = 1
  val NESTED_GCC_FWC = 3
  val NESTED_GCC_DECOMP_FWC = 4


  def makeGccs(X: Array[CPIntVar], values: Range, lower: Array[Array[(Int, Int)]], upper: Array[Array[(Int, Int)]],
               mode: Int): Array[Constraint] = {
    val nVariables = X.length
    val nValues = values.length

    if (mode == MULTIPLE_GCC_AC || mode == MULTIPLE_GCC_FWC) {
      var cutoffs = Set[Int]()
      val allLower = Array.tabulate(nVariables + 1, nValues)((i, vi) => 0)
      val allUpper = Array.tabulate(nVariables + 1, nValues)((i, vi) => i)

      for (vi <- values.indices) {
        for ((i, bound) <- lower(vi)) {
          allLower(i)(vi) = bound
          cutoffs += i
        }
        for ((i, bound) <- upper(vi)) {
          allUpper(i)(vi) = bound
          cutoffs += i
        }
      }

      for (i <- cutoffs.toArray)
        yield new GCC(X.splitAt(i)._1, values.min, allLower(i), allUpper(i))

    } else if (mode == NESTED_GCC_FWC) {
      Array(new NestedGCCFWC(X, values.min, lower, upper))
    } else  { //  NESTED_GCC_DECOMP_FWC
      Array(new NestedGCCDecompFWC(X, values.min, lower, upper))
    }
  }

  def nbSol(domX: Array[Set[Int]], values: Range, lower: Array[Array[(Int, Int)]], upper: Array[Array[(Int, Int)]],
            mode: Int): (Int, Long, Int, Int) = {
    val cp = CPSolver()

    val X = domX.map(dom => CPIntVar(dom)(cp))

    val strength = {
      if (mode == MULTIPLE_GCC_AC) CPPropagStrength.Strong
      else CPPropagStrength.Weak
    }

    cp.deactivateNoSolExceptions()
    for (con <- makeGccs(X, values, lower, upper, mode))
      cp.add(con, strength)

    cp.search { binaryStatic(X) }

    val stat = cp.start(nSols = 10000000)
    (stat.nSols, stat.time, stat.nNodes, stat.nFails)
  }

  var rand: Random = null
  def randomDom(size: Int) = Array.fill(size)(rand.nextInt(size)/*-3*/).toSet
  def randomPlaces(nVariables: Int, size: Int) = Array.fill(size)(rand.nextInt(nVariables) + 1).toSet
  def occurrences(solution: Array[Int], v: Int, i: Int): Int = {
    solution.splitAt(i)._1.count(_ == v)
  }
  def ifPossibleAtLeast(max: Int, atLeast: Int): Int = {
    if (atLeast > max) max
    else max - rand.nextInt(max - atLeast + 1)
  }
  def ifPossibleAtMost(min: Int, atMost: Int): Int = {
    if (atMost < min) min
    else min + rand.nextInt(atMost - min + 1)
  }

  def lowerAtMostBy(max: Int, error: Double): Int = {
    max - rand.nextInt((max * error).toInt + 1)
  }
  def higherAtMostBy(min: Int, error: Double): Int = {
    min + rand.nextInt((min * error).toInt + 1)
  }

  // nSols on random domains

  test("Random bounds") {
    for (i <- 1 to 100) {
      //println(s"test #$i")
      rand =  new scala.util.Random(i)

      val nVariables = 9

      val domVars = Array.fill(nVariables)(randomDom(size = 6))
      /*for (i <- 0 until nVariables) {
        println(s"$i: ${domVars(i) mkString " "}")
      }*/

      val min = domVars.flatten.min
      val max = domVars.flatten.max
      val nValues = (min to max).length

      val lowerBuffer = Array.fill(nValues)(ArrayBuffer[(Int,Int)]())
      val upperBuffer = Array.fill(nValues)(ArrayBuffer[(Int,Int)]())

      randomPlaces(nVariables, size = nVariables / 3).foreach(i => {
        val vi = rand.nextInt(nValues)
        lowerBuffer(vi) += ((i, 1 + rand.nextInt(1 + i / 2)))
      })
      randomPlaces(nVariables, size = nVariables / 3).foreach(i => {
        val vi = rand.nextInt(nValues)
        upperBuffer(vi) += ((i, i - 1 - rand.nextInt(1 + i / 2)))
      })

      val lower = lowerBuffer.map(_.toArray)
      val upper = upperBuffer.map(_.toArray)

      val (nSols1, time1, nNodes1, nFails1) = nbSol(domVars, min to max, lower, upper, MULTIPLE_GCC_FWC)
      val (nSols2, time2, nNodes2, nFails2) = nbSol(domVars, min to max, lower, upper, NESTED_GCC_FWC)
      val (nSols3, time3, nNodes3, nFails3) = nbSol(domVars, min to max, lower, upper, NESTED_GCC_DECOMP_FWC)

      nFails3 should be(nFails2)
      nSols1 should be(nSols2)
      nSols1 should be(nSols3)
    }
  }


/*
  test("Test Example Paper") {
    val cp = CPSolver()
    val x = Array.fill(10)(CPIntVar(0 to 3)(cp))
    val blue = 0
    val green = 1
    val red = 2
    val orange = 3



    val lowerBlue: Array[(Int,Int)] = Array()
    val lowerGreen: Array[(Int,Int)] = Array()
    val lowerRed : Array[(Int,Int)] = Array() // Array((0,0),(1,0),(2,1),(3,0),(4,2),(5,1),(6,2),(7,4),(8,4),(9,0))
    val lowerOrange: Array[(Int,Int)] = Array()

    val upperBlue: Array[(Int,Int)] = Array()
    val upperGreen: Array[(Int,Int)] = Array()
    val upperRed: Array[(Int,Int)] = Array((0,2),(1,1),(2,3),(3,1),(4,2),(5,4),(6,5),(7,3),(8,5),(9,5))
    val upperOrange: Array[(Int,Int)] = Array()

    val lower = Array(lowerBlue,lowerGreen,lowerRed,lowerOrange)
    val upper = Array(upperBlue,upperGreen,upperRed,upperOrange)


    //cp.add(new PrefixCC(x,0,lower,upper))
    cp.add(new PrefixCCSegments2(x,0,lower,upper))

  }*/

}
