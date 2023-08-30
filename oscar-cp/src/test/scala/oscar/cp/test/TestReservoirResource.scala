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
import org.scalatest.Matchers
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.util.RandomGenerator

/**
 * Created on 04/06/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */
class TestReservoirResource extends FunSuite with Matchers {

  // Decomp checker on solutions
  def checkSolOk(starts: Array[Int], durations: Array[Int], ends: Array[Int], prods: Array[Int], cons: Array[Int], minCapa: Int, maxCapa: Int, initialAmount: Int): Boolean = {
    val horizon = ends.max
    var reservoirAmount = initialAmount
    for (t <- 0 until horizon) {
      for (i <- starts.indices) {
        if (t == starts(i) && cons(i) > 0) reservoirAmount -= cons(i)
        else if (t == ends(i) && prods(i) > 0) reservoirAmount += prods(i)
      }
      if (reservoirAmount < minCapa || reservoirAmount > maxCapa)
        return false
    }
    true
  }

  test("Reservoir max capacity 1 with 1 producer and 1 consumer; initial amount of 0") {
    implicit val cp = CPSolver()
    cp.silent = true
    val startVars = Array(CPIntVar(0 to 10), CPIntVar(0 to 10))
    val durationVars = Array(CPIntVar(10), CPIntVar(10))
    val endVars = Array(CPIntVar(10 to 20), CPIntVar(10 to 20))
    val productionVars = Array(CPIntVar(1), CPIntVar(0))
    val consumptionVars = Array(CPIntVar(0), CPIntVar(1))

    val minCapacity = 0
    val maxCapacity = 1
    val initialAmount = 0

    for (i <- startVars.indices) {
      add(startVars(i) + durationVars(i) === endVars(i))
    }
    add(reservoirResource(startVars, durationVars, endVars, productionVars, consumptionVars, minCapacity, maxCapacity, initialAmount))

    startVars(0).min should be(0)
    startVars(1).min should be(10)
    startVars(0).max should be(0)
    startVars(1).max should be(10)
    endVars(0).min should be(10)
    endVars(1).min should be(20)
    endVars(0).max should be(10)
    endVars(1).max should be(20)

    cp.isFailed should be(false)
  }

  test("Reservoir max capacity 1 with 1 producer and 1 consumer; initial amount of 1") {
    implicit val cp = CPSolver()
    cp.silent = true
    val startVars = Array(CPIntVar(0 to 15), CPIntVar(6 to 10))
    val durationVars = Array(CPIntVar(5), CPIntVar(10))
    val endVars = Array(CPIntVar(5 to 20), CPIntVar(16 to 20))
    val productionVars = Array(CPIntVar(1), CPIntVar(0))
    val consumptionVars = Array(CPIntVar(0), CPIntVar(1))

    val minCapacity = 0
    val maxCapacity = 1
    val initialAmount = 1

    for (i <- startVars.indices) {
      add(startVars(i) + durationVars(i) === endVars(i))
    }
    add(reservoirResource(startVars, durationVars, endVars, productionVars, consumptionVars, minCapacity, maxCapacity, initialAmount))

    startVars(0).min should be(1)
    startVars(1).min should be(6)
    startVars(0).max should be(15)
    startVars(1).max should be(10)
    endVars(0).min should be(6)
    endVars(1).min should be(16)
    endVars(0).max should be(20)
    endVars(1).max should be(20)

    cp.isFailed should be(false)
  }

  test("Random instances with a total of 4 activities") {
    val nActivities = 4
    val minDuration = 1
    val maxDuration = 10
    val horizon = nActivities * maxDuration
    for (iter <- 1 to 100) {
      implicit val cp = CPSolver()
      cp.silent = true
      val durationVars = Array.fill(nActivities)(CPIntVar(minDuration + RandomGenerator.nextInt(maxDuration - minDuration)))
      val startVars = Array.tabulate(nActivities)(i => CPIntVar(0 to horizon - durationVars(i).min))
      val endVars = Array.tabulate(nActivities)(i => CPIntVar(durationVars(i).min to horizon))
      val prod = Array.fill(nActivities)(RandomGenerator.nextBoolean())
      val productionVars = Array.tabulate(nActivities)(i => if (prod(i)) CPIntVar(1 + RandomGenerator.nextInt(2)) else CPIntVar(0))
      val consumptionVars = Array.tabulate(nActivities)(i => if (!prod(i)) CPIntVar(1 + RandomGenerator.nextInt(2)) else CPIntVar(0))
      val makespan = maximum(endVars)
      val minCapacity = 1
      val maxCapacity = minCapacity + RandomGenerator.nextInt(nActivities - minCapacity)
      val initialAmount = minCapacity + math.min(1, maxCapacity)

      for (i <- startVars.indices) {
        add(startVars(i) + durationVars(i) === endVars(i))
      }

      try {
        add(reservoirResource(startVars, durationVars, endVars, productionVars, consumptionVars, minCapacity, maxCapacity, initialAmount))
      }
      catch {
        case e: NoSolutionException =>
        case _: Throwable => println("A problem has occured")
      }

      onSolution{
        checkSolOk(startVars.map(_.value), durationVars.map(_.value), endVars.map(_.value),
          productionVars.map(_.value), consumptionVars.map(_.value), minCapacity, maxCapacity, initialAmount) shouldBe true
      }

      search(binaryFirstFail(startVars))

      minimize(makespan)
      start()
    }
  }

  test("Random instances with a total of 6 activities") {
    val nActivities = 6
    val minDuration = 1
    val maxDuration = 10
    val horizon = nActivities * maxDuration
    for (iter <- 1 to 20) {
      implicit val cp = CPSolver()
      cp.silent = true
      val durationVars = Array.fill(nActivities)(CPIntVar(minDuration + RandomGenerator.nextInt(maxDuration - minDuration)))
      val startVars = Array.tabulate(nActivities)(i => CPIntVar(0 to horizon - durationVars(i).min))
      val endVars = Array.tabulate(nActivities)(i => CPIntVar(durationVars(i).min to horizon))
      val prod = Array.fill(nActivities)(RandomGenerator.nextBoolean())
      val productionVars = Array.tabulate(nActivities)(i => if (prod(i)) CPIntVar(1 + RandomGenerator.nextInt(2)) else CPIntVar(0))
      val consumptionVars = Array.tabulate(nActivities)(i => if (!prod(i)) CPIntVar(1 + RandomGenerator.nextInt(2)) else CPIntVar(0))
      val makespan = maximum(endVars)
      val minCapacity = 1
      val maxCapacity = minCapacity + RandomGenerator.nextInt(nActivities - minCapacity)
      val initialAmount = minCapacity + math.min(1, maxCapacity)

      for (i <- startVars.indices) {
        add(startVars(i) + durationVars(i) === endVars(i))
      }
      try {
        add(reservoirResource(startVars, durationVars, endVars, productionVars, consumptionVars, minCapacity, maxCapacity, initialAmount))
      } catch {
        case e: NoSolutionException => // The instance is not solvable
        case _: Throwable => println("A problem has occured")
      }

      onSolution{
        checkSolOk(startVars.map(_.value), durationVars.map(_.value), endVars.map(_.value),
          productionVars.map(_.value), consumptionVars.map(_.value), minCapacity, maxCapacity, initialAmount) shouldBe true
      }

      search(binaryFirstFail(startVars))

      minimize(makespan)
      start()
    }
  }
}
