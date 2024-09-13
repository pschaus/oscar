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

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.testUtils._
import oscar.util.RandomGenerator

import scala.collection.mutable.ArrayBuffer

/**
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 * @author Pierre Schaus (pschaus@gmail.com)
 */
class TestSetTimesBranching extends TestSuite {


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

  test("SetTimes with transitions and precedences") {
    // (duration, consumption)
    
    


    def solve(seed: Int, withSetTimes: Boolean): Int = {
      
      val rand = new scala.util.Random(seed)
      val nTasks = 6
      val instance = Array.tabulate(nTasks)(t => (rand.nextInt(20)+1,rand.nextInt(4)+1))
      val durationsData = instance.map(_._1)
      val demandsData = instance.map(_._2)
      val capa = 5
      val horizon = instance.map(_._1).sum
      val Times = 0 to horizon
      
      
      
      implicit val cp = CPSolver()
      cp.silent = true

      val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
      val starts = Array.tabulate(nTasks)(t => CPIntVar(0, horizon - durations(t).min))
      val ends = Array.tabulate(nTasks)(t => starts(t) + durationsData(t))
      val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)))
      val makespan = maximum(ends)

      try {
        add(maxCumulativeResource(starts, durations, ends, demands, CPIntVar(capa)), Medium)

        val possPrec = for (i <- 0 until nTasks; j <- i + 1 until nTasks) yield (i, j)

        val prec = for (i <- 0 until 20) yield {
          val (a, b) = possPrec(rand.nextInt(possPrec.size))
          (a, b, rand.nextInt(10))
        }

        
        add(starts(0) >= 5)
        add(ends(0) <=  ends(0).min +4)
        
        for ((i, j, d) <- prec) {
          add(ends(i)+d <= starts(j))
          //add(ends(i) <= starts(j)+d)
          //add(starts(i) + d <= starts(j)) // settimes failw with this
        }
      } catch {
        case e: NoSolutionException => return 0
      }

      minimize(makespan)

      var best = Int.MaxValue

      onSolution {
        best = makespan.min
      }

      search {
        if (withSetTimes) setTimes(starts, durations, ends)
        else binaryStaticIdx(starts,i => starts(i).min)

      }

      val stats = start()
      //println(stats)
      //println("obj:"+best)
      best

    }
    for (i <- 0 until 200) {
      val opt1 = solve(i, true)
      val opt2 = solve(i, false)
      assert(opt1 == opt2)
    }


  }
  
  
  test("SetTimes Test Dominance") {
    // (duration, consumption)
    
    


    def solve(seed: Int, withSetTimes: Boolean): Int = {
      
      val rand = new scala.util.Random(seed)
      val nTasks = 4
      val instance = Array.tabulate(nTasks)(t => (5,1))
      val durationsData = instance.map(_._1)
      val demandsData = instance.map(_._2)
      val capa = 1
      val horizon = 50
      val Times = 0 to horizon
      
      
      
      implicit val cp = CPSolver()
      cp.silent = true

      val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
      val starts = Array.tabulate(nTasks)(t => CPIntVar(0, horizon - durations(t).min))
      val ends = Array.tabulate(nTasks)(t => starts(t) + durationsData(t))
      val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)))
      val makespan = maximum(ends)

      try {
        add(maxCumulativeResource(starts, durations, ends, demands, CPIntVar(capa)), Medium)


        add(ends(0) <=  10)
        for (i <- 1 until nTasks) {
          add(starts(i) >= 8)
        }

      } catch {
        case e: Inconsistency => return 0
      }

      minimize(makespan)

      var best = Int.MaxValue

      onSolution {
        best = makespan.min
      }

      search {
        if (withSetTimes) setTimes(starts, durations, ends)
        else binaryStaticIdx(starts,i => starts(i).min)

      }

      val stats = start()
      //println(stats)
      //println("obj:"+best)
      best

    }
    for (i <- 0 until 200) {
      val opt1 = solve(i, true)
      val opt2 = solve(i, false)
      assert(opt1 == opt2)
    }


  }  
  
    

}
