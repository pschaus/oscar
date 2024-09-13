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
import oscar.algo.branchings.ConflictOrderingSearch
import oscar.cp._
import oscar.cp.testUtils.TestSuite

import scala.util.Random
import oscar.algo.search.DFSearch
import oscar.cp.constraints.Or
import oscar.cp.scheduling.constraints._
import oscar.algo.search.Branching

import scala.collection.mutable.TreeSet

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Steven Gay stevenGay@uclouvain.be
 */
abstract class TestUnary(name: String, nTests: Int) extends TestSuite {
  // pairwise decomposition
  def decomp(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],resources: Array[CPIntVar], id: Int)(implicit store: CPStore): Array[Constraint] = {
    val n = starts.length
    val constraints = for (i <- 0 until n; j <- i + 1 until n) yield {
      new Or(Array(ends(i) ?<= starts(j), ends(j) ?<= starts(i), resources(i) ?!== id, resources(j) ?!== id)) //, durations(i) === 0, durations(j) === 0))
    }
    constraints.toArray
  }

  type Vars = Array[CPIntVar]
  def unary(starts: Vars, durations: Vars, ends: Vars, resources: Vars, id: Int)(implicit store: CPStore): Array[Constraint]
  
  
  case class UnaryProblem(starts: Vars, durations: Vars, ends: Vars, resources: Vars, store: CPStore)  
  
  case class Solution(ss: Array[Int], es: Array[Int], rs: Array[Int])
  
  implicit val solOrdering = new Ordering[Solution] {
    import scala.math.Ordering.Implicits._
    def compare(x: Solution, y: Solution): Int = {
      val n = x.ss.length
      
      var p = 0
      while (p < n) {
        if (x.ss(p) != y.ss(p)) return x.ss(p) - y.ss(p)
        p += 1
      }
      
      p = 0
      while (p < n) {
        if (x.es(p) != y.es(p)) return x.es(p) - y.es(p)
        p += 1
      }

      p = 0
      while (p < n) {
        if (x.rs(p) != y.rs(p)) return x.rs(p) - y.rs(p)
        p += 1
      }
      
      0
    }    
  }
  
  val nResources = 3
  
  def makeRandomInstance(nTasks: Int): UnaryProblem = {
    implicit val S = new CPStore()

    val begTimes = Random.nextInt(60) - 30
    val endTimes = begTimes + 5 + Random.nextInt(60)
    val durTimes = endTimes - begTimes
    
    val durations = Array.fill[CPIntVar](nTasks) {
      val dmin = 1 + Random.nextInt(durTimes / 4)
      val dmax = dmin + Random.nextInt(durTimes / 4)
      CPIntVar(dmin, dmax)
    }
    
    val starts = Array.fill[CPIntVar](nTasks) {
      val smin = begTimes + Random.nextInt(durTimes)
      val smax = smin + Random.nextInt(durTimes)
      CPIntVar(smin, smax)
    }
    
    val ends = Array.tabulate[CPIntVar](nTasks) { i =>
      // unary does not have to take care of start + duration == end
      try {
        val e = starts(i) + durations(i)
        S.add(e <= endTimes)
        e
      }
      catch {
        case _: Inconsistency =>
          //just ignore Inconsistency, will simply set fail()
          CPIntVar(0,0)
      }
    }
    
    val resources = Array.fill(nTasks){
      val r = CPIntVar.sparse(1, nResources)
      
      // make some holes
      try {
        for (i <- 0 until nResources - 1)
          r.removeValue(Random.nextInt(nResources + 1))
      }
      catch {
        case _: Inconsistency => //just ignore Inconsistency, will simply set fail()
      }
      r
    }
    
    UnaryProblem(starts, durations, ends, resources, S)
  }
  
  def testConstrainer(problem: UnaryProblem, dfs: DFSearch, br: Branching, constrain: (Vars, Vars, Vars, Vars, Int) => Array[Constraint])(implicit S: CPStore): Int = {
    S.pushState()

    try {
      for (id <- 1 until nResources)
        S.add(constrain(problem.starts, problem.durations, problem.ends, problem.resources, id))
    }
    catch {
      case _: Inconsistency => //just ignore Inconsistency, will simply set fail()
    }

    dfs.start(br, node => node.nSolutions >= 30000)
    val nSolutionsUnary = dfs.nSolutions
    
    S.pop()
    nSolutionsUnary
  }

  test("Testing " + name + " for all solutions ") {
    for (t <- 0 until nTests) {
      val problem = makeRandomInstance(6)
      implicit val S = problem.store
      val dfs = new DFSearch(S)
      val br = binaryFirstFail(problem.starts ++ problem.ends ++ problem.resources)
      
      val nSolutionsDecomp = testConstrainer(problem, dfs, br, decomp)
      val nSolutionsUnary  = testConstrainer(problem, dfs, br, unary)

      
      //println(s"nSolutionsDecomp = $nSolutionsDecomp, nSolutionsUnary = $nSolutionsUnary")
      val ok = nSolutionsDecomp == nSolutionsUnary
      
      val debug = false
      
      if (!ok) {
        val nTasks = problem.starts.length
        if (debug) println("Test failed, tasks were:")
        for (i <- 0 until nTasks) {
          if (debug) println(s"$i: start = ${problem.starts(i)} -- duration = ${problem.durations(i)} --> end = ${problem.ends(i)} in resource = ${problem.resources(i)}")
        }
        
        // get the set of solutions
        val dfsDecomp = new DFSearch(S)
        val decompSolutions = new TreeSet[Solution]()(solOrdering)
        dfsDecomp.onSolution {
          val sol = Solution(problem.starts.map(_.value), problem.ends.map(_.value), problem.resources.map(_.value))
          decompSolutions.add(sol)
        }
        val nDecomp = testConstrainer(problem, dfsDecomp, br, decomp)
        if (debug) println(s"decomp has $nDecomp solutions on recount")
        
        val dfsUnary = new DFSearch(S)
        val unarySolutions = new TreeSet[Solution]()(solOrdering)
        dfsUnary.onSolution {
          val sol = Solution(problem.starts.map(_.value), problem.ends.map(_.value), problem.resources.map(_.value))
          unarySolutions.add(sol)
        }
        val nUnary = testConstrainer(problem, dfsUnary, br, unary)
        if (debug) println(s"unary has $nUnary solutions on recount")
        
        val instanceOption = if (nDecomp > nUnary) {
          val du = decompSolutions -- unarySolutions
          if (debug) println(s"${du.size} solutions in decomp not in unary, example:")
          Some(du.head)          
        }
        else if (nUnary > nDecomp) {
          val ud = unarySolutions -- decompSolutions
          if (debug) println(s"${ud.size} solutions in unary not in decomp, example:")
          Some(ud.head)
        }
        else None
        
        instanceOption foreach { instance =>
          for (i <- 0 until nTasks) {
            if (debug) println(s"$i: [ ${instance.ss(i)} ; ${instance.es(i)} [  in resource ${instance.rs(i)}")
          }
        }
      }
      
      ok should be(true)
    }
  }
}


class TestUnaryDPSweep extends TestUnary("UnaryDPSweep, correctness", 100) {
  override def unary(starts: Vars, durations: Vars, ends: Vars,resources: Vars, id: Int)(implicit store: CPStore) =
    Array(UnaryDPSweep(starts, durations, ends, resources, id))
}


class TestUnaryStack extends TestUnary("Unary, correctness", 100) {
  override def unary(starts: Vars, durations: Vars, ends: Vars,resources: Vars, id: Int)(implicit store: CPStore) =
    Array(Unary(starts, durations, ends, resources, id))
}
