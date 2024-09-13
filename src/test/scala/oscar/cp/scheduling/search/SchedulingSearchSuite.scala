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

package oscar.cp.scheduling.search

import oscar.algo.branchings.{LCSearchSimplePhaseAssign, SplitLastConflict}
import oscar.cp.testUtils._
import oscar.cp._

import scala.util.Random
import oscar.algo.search.Branching
import oscar.util.RandomGenerator

import scala.collection.mutable.ArrayBuffer
/**
 *  @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 *  @author Renaud Hartert ren.hartert@gmail.com
 *  @author Pierre Schaus pschaus@gmail.com
 */
class SetTimesBranchingSuite extends SchedulingSearchSuite(seed = 0, scalable = true) {
  override def searchHeuristic(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], tieBreaker: Int => Int): Branching = {
    setTimes(starts, durations, ends, tieBreaker)
  }
}

class NewSetTimesSuite extends SchedulingSearchSuite(seed = 0, scalable = true) {
  override def searchHeuristic(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], tieBreaker: Int => Int): Branching = {
    new NewSetTimes(starts, ends, tieBreaker)
  }
}

class SplitLastConflictMinMinSuite extends SchedulingSearchSuite(seed = 0, scalable = false) {
  override def searchHeuristic(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], tieBreaker: Int => Int): Branching = {
    new SplitLastConflict(starts, starts(_).size, starts(_).min)
  }
}

abstract class SchedulingSearchSuite(seed: Int, scalable: Boolean) extends TestSuite {

  /** To implement with the search heuristic to be tested */
  def searchHeuristic(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], tieBreaker: Int => Int): Branching

  private[this] val rng = new Random(seed)

  /**
   * randomly split the given rectangle into n smaller rectangles  
   */
  def splitRectangle(dur: Int, height: Int, n: Int): List[(Int,Int)] = {
    var rects = ArrayBuffer((dur,height))
    while (rects.size < n) {
      //val (_,i) = rects.zipWithIndex.map { case ((d, h),i) => (d * h,i) }.max
      val i = RandomGenerator.nextInt(rects.size)
      val (d,h) = rects.remove(i)
      if (h >= 2 && RandomGenerator.nextBoolean()) { // horizontal split
        val hr = RandomGenerator.nextInt(h)
        rects.append((d,hr),(d,h-hr))
      } else if (d > 5) { // vertical split
        val dr = RandomGenerator.nextInt(d-2)
        rects.append((dr,h),(d-dr,h))
      } else {
        rects.append((d,h))
      }
    }
    rects.map{case(d,h)=> d*h}.sum shouldEqual (dur*height)
    return rects.toList
  }

  test("SetTimes test on a dense rectangle of height 4 and width 50") {
    val minWidth = 40
    val optimalMakespan = 50
    val capacity = 4
    val maxRecursiveSplits = 3

    for (i <- 1 to 10) {
      //val activitySolution = Array.tabulate(capacity)(i => splitRectangle(0, optimalMakespan, minWidth, maxRecursiveSplits)).flatten
      val activitySolution = splitRectangle(optimalMakespan, capacity,5).toArray

      val nActivities = activitySolution.length
      val durations = activitySolution.map(a => a._1)
      val demands = activitySolution.map(a => a._2)

      val cp = CPSolver()
      cp.silent = true
      val startVars = Array.tabulate(nActivities)(i => CPIntVar(0 to optimalMakespan - durations(i))(cp))
      val endVars = Array.tabulate(nActivities)(i => startVars(i)+durations(i))
      val durationVars = durations.map(d => CPIntVar(d)(cp))
      val demandVars = demands.map(c => CPIntVar(c)(cp))
      val makespan = maximum(endVars)

      cp.add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)(cp)),Medium)

      cp.minimize(makespan)
      cp.search{
        setTimes(startVars, durationVars, endVars,i => startVars(i).min)
      }

      var bestSol = 0
      cp.onSolution{
        bestSol = makespan.value
      }

      cp.start()
      bestSol shouldEqual optimalMakespan
    }
  }

  test("SetTimes test on a dense rectangle of height 10 and width 200") {
    val optimalMakespan = 200
    val capacity = 10

    for (i <- 1 to 10) {
      val activitySolution = splitRectangle(optimalMakespan, capacity,5).toArray
      val nActivities = activitySolution.length
      val durations = activitySolution.map(a => a._1)
      val demands = activitySolution.map(a => a._2)

      val cp = CPSolver()
      cp.silent = true
      val startVars = Array.tabulate(nActivities)(i => CPIntVar(0 to optimalMakespan - durations(i))(cp))
      val endVars = Array.tabulate(nActivities)(i => startVars(i)+durations(i))
      val durationVars = durations.map(d => CPIntVar(d)(cp))
      val demandVars = demands.map(c => CPIntVar(c)(cp))
      val makespan = maximum(endVars)

      cp.add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)(cp)),Medium)

      cp.minimize(makespan)
      cp.search{
        setTimes(startVars, durationVars, endVars,i => startVars(i).min)
      }

      var bestSol = 0
      cp.onSolution{
        bestSol = makespan.value
      }

      cp.start()
      bestSol shouldEqual optimalMakespan
    }
  }

  test("Scheduling search should solve Steven's example.") {

    val durations = Array(2, 4, 3, 3)
    val demands = Array(1, 1, 2, 2)
    val horizon = 6
    val capa = 3

    new CPModel {

      val nTasks = durations.length
      val startVars = Array.tabulate(nTasks)(i => CPIntVar(0, horizon - durations(i)))
      val endVars = Array.tabulate(nTasks)(i => startVars(i) + durations(i))
      val durationVars = Array.tabulate(nTasks)(i => CPIntVar(durations(i)))
      val demandVars = Array.tabulate(nTasks)(i => CPIntVar(demands(i)))

      solver.silent = true

      add(startVars(0) === 0)
      add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capa)), Strong)
      search { searchHeuristic(startVars, durationVars, endVars, i => i) }

      val stats = start(nSols = 1)
      assert(stats.nSols == 1)
    }
  }

  test(scalable, "Scaling should not impact the number of nodes.") {

    val scaling1 = 6
    val scaling2 = 100

    val minWidth = 10
    val optimalMakespan = 100
    val capacity = 4
    val maxRecursiveSplits = 5

    for (i <- 1 to 10) {

      // Data
      val activitySolution = splitRectangle(optimalMakespan, capacity,5).toArray
      val nActivities = activitySolution.length
      val durations = activitySolution.map(a => a._1)
      val demands = activitySolution.map(a => a._2)      
      val nTasks = activitySolution.length
      val durationsData = activitySolution.map(a => a._2 - a._1)
      
      var nNodes1 = 0
      var nNodes2 = 0
      var nNodes3 = 0

      new CPModel {
        solver.silent = true
        val horizon = optimalMakespan
        val durations = durationsData
        val startVars = Array.tabulate(nTasks)(i => CPIntVar(0, optimalMakespan - durations(i)))
        val endVars = Array.tabulate(nTasks)(i => startVars(i) + durations(i))
        val durationVars = Array.tabulate(nTasks)(i => CPIntVar(durations(i)))
        val demandVars = Array.fill(nTasks)(CPIntVar(1))
        val makespan = maximum(endVars)

        add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)), Medium)

        minimize(makespan)
        search { searchHeuristic(startVars, durationVars, endVars, i => -endVars(i).min) }

        val stats = start()
        nNodes1 = stats.nNodes
      }

      new CPModel {
        solver.silent = true
        val horizon = optimalMakespan * scaling1
        val durations = durationsData.map(_ * scaling1)
        val startVars = Array.tabulate(nTasks)(i => CPIntVar(0, horizon - durations(i)))
        val endVars = Array.tabulate(nTasks)(i => startVars(i) + durations(i))
        val durationVars = Array.tabulate(nTasks)(i => CPIntVar(durations(i)))
        val demandVars = Array.fill(nTasks)(CPIntVar(1))
        val makespan = maximum(endVars)

        add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)), Strong)

        minimize(makespan)
        search { searchHeuristic(startVars, durationVars, endVars, i => -endVars(i).min) }

        val stats = start()
        nNodes2 = stats.nNodes
      }
      
      assert(nNodes1 == nNodes2, s"scaling by $scaling1 increases the number of nodes from $nNodes1 to $nNodes2") 

      new CPModel {
        solver.silent = true
        val horizon = optimalMakespan * scaling2
        val durations = durationsData.map(_ * scaling2)
        val startVars = Array.tabulate(nTasks)(i => CPIntVar(0, horizon - durations(i)))
        val endVars = Array.tabulate(nTasks)(i => startVars(i) + durations(i))
        val durationVars = Array.tabulate(nTasks)(i => CPIntVar(durations(i)))
        val demandVars = Array.fill(nTasks)(CPIntVar(1))
        val makespan = maximum(endVars)

        add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)), Strong)

        minimize(makespan)
        search { searchHeuristic(startVars, durationVars, endVars, i => -endVars(i).min) }

        val stats = start()
        nNodes3 = stats.nNodes
      }
      
      assert(nNodes1 == nNodes3, s"scaling by $scaling2 increases the number of backtracks from $nNodes1 to $nNodes3")
    }
  }
}