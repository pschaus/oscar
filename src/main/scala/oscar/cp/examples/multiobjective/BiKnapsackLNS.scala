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


package oscar.cp.examples.multiobjective

import oscar.cp._
import oscar.algo.selectMin
import oscar.cp.examples.util.reader.KnapsackReader._
import oscar.cp.multiobjective.SolSelect._

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object BiKnapsackLNS extends App {

  val dataFile = "data/mo-knapsack/2KP100A.txt"
  val solutionFile = "data/mo-knapsack/solution/2KP100A.txt"

  // Data
  // ----
  val (nItems: Int, capa1: Int, capa2: Int, items1: Array[(Int, Int)], items2: Array[(Int, Int)]) = read(dataFile)

  val Items = 0 until nItems
  val nObjs = 2
  val Objs = 0 until nObjs

  val weight = Array(items1.map(_._1), items2.map(_._1))
  val profit = Array(items1.map(_._2), items2.map(_._2))
  val ratio = for (o <- Objs) yield (Items.map(i => profit(o)(i).toDouble / weight(o)(i)).toArray)


  // Model
  // -----
  implicit val cp = CPSolver()
  cp.silent = true

  val x: Array[CPBoolVar] = Array.fill(nItems)(CPBoolVar()(cp))
  val capaVar1 = CPIntVar(0 to capa1)(cp)
  val capaVar2 = CPIntVar(0 to capa2)(cp)
  val profitVar1 = CPIntVar(0 to profit(0).sum)(cp)
  val profitVar2 = CPIntVar(0 to profit(1).sum)(cp)

  val knapsack1 = binaryKnapsack(x, items1.map(_._2), items1.map(_._1), profitVar1, capaVar1)
  val knapsack2 = binaryKnapsack(x, items2.map(_._2), items2.map(_._1), profitVar2, capaVar2)

  cp.addDecisionVariables(x)
  cp.addDecisionVariables(Array(capaVar1, capaVar2))

  var obj = 0
  cp.paretoMaximize(profitVar1, profitVar2) 
  
  cp.add(knapsack1)
  cp.add(knapsack2)
  search {
    selectMin(0 until x.size)(!x(_).isBound)(-ratio(obj)(_)) match {
      case None => noAlternative
      case Some(i) => branch(cp.post(x(i) === 1))(cp.post(x(i) === 0))
    }
  }

  
  start(1)


  val rand = new scala.util.Random()
  // MO LNS PARAMETERS
  val maxRestarts = 1000000 // number of restarts
  val maxFailures = 200 // max number of failures at each restart
  val relaxSize = 5 // number of relaxed variables at each restart
  val probaIntensify = 30 // probability (%) of intensification 
  // MO LNS FRAMEWORK
  for (restart <- 1 to maxRestarts) {
    // next solution to restart from
    val sols = cp.nonDominatedSolutions
    val r = rand.nextInt(sols.size)
    val sol = sols(r)
    // random selection between intensification or diversification
    if (rand.nextInt(100) < probaIntensify) cp.objective.intensify(sol)
    else cp.objective.diversify()
    // search
    cp.startSubjectTo(failureLimit = maxFailures) {
      relaxRandomly(x, sol, relaxSize)
    }
  }

}
