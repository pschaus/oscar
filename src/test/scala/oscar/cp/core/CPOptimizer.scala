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

package oscar.cp.core

import oscar.algo.Inconsistency
import oscar.cp.constraints.CPObjectiveUnit
import oscar.cp.constraints.ParetoConstraint
import oscar.cp.multiobjective.Pareto
import oscar.cp.multiobjective.ListPareto
import oscar.cp.constraints.CPObjectiveUnitMaximize
import oscar.cp.constraints.CPObjective
import oscar.cp.constraints.CPObjectiveUnitMinimize
import oscar.cp.TightenType
import oscar.cp.constraints.CPObjectiveGeometricMinimize
import oscar.cp.constraints.CPObjectiveUnit
import oscar.cp.core.variables.CPIntVar

class CPOptimizer(propagStrength: CPPropagStrength) extends CPStore(propagStrength) {
  
  def this() = this(CPPropagStrength.Weak)
  
  var objective = new CPObjective(this, Array[CPObjectiveUnit]());
  
  private val decVariables = scala.collection.mutable.Set[CPIntVar]()
  
  var paretoSet: Pareto[CPSol] = new ListPareto[CPSol](Array())
  var recordNonDominatedSolutions = false
  
  def nonDominatedSolutions: Seq[CPSol] = paretoSet.toList
  def nonDominatedSolutionsObjs: Seq[IndexedSeq[Int]] = paretoSet.objectiveSols 
  
  def obj(objVar: CPIntVar): CPObjectiveUnit = objective(objVar)

  def optimize(obj: CPObjective): CPOptimizer = {
    objective = obj
    try {
      postCut(obj)
    }
    catch {
      case _: Inconsistency => fail()
    }
    this
  }

  def minimize(objective: CPIntVar): CPOptimizer = minimize(Seq(objective): _*)
  
  def minimize(objectives: CPIntVar*): CPOptimizer = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMinimize(_)): _*))
  
  def minimize(objective: CPIntVar, ratio: Double): CPOptimizer = {
    val o = new CPObjectiveGeometricMinimize(objective, "GeometricMinimize", ratio): CPObjectiveUnit
    optimize(new CPObjective(this, Array(o)))
  }
  
  def maximize(objective: CPIntVar): CPOptimizer = maximize(Seq(objective): _*)

  def maximize(objectives: CPIntVar*): CPOptimizer = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMaximize(_)): _*))
  
  def paretoMinimize(objective: CPIntVar): CPOptimizer = paretoOptimize((objective, false))  
  
  def paretoMinimize(objectives: CPIntVar*): CPOptimizer = paretoOptimize(objectives.map((_, false)): _*)
  
  def paretoMaximize(objective: CPIntVar): CPOptimizer = paretoOptimize((objective, true))
  
  def paretoMaximize(objectives: CPIntVar*): CPOptimizer = paretoOptimize(objectives.map((_, true)): _*)
  
  def paretoOptimize(objVarModes: (CPIntVar, Boolean)): CPOptimizer = paretoOptimize(Seq(objVarModes): _*)
  
  def paretoOptimize(objVarMode: (CPIntVar, Boolean)*): CPOptimizer = {
    
    // objVar of each objective
    val objectives = objVarMode.map(_._1).toArray
    // true if objective i has to be maximized
    val isMax = objVarMode.map(_._2).toArray

    recordNonDominatedSolutions = true
    objective = new CPObjective(this, (for (i <- 0 until isMax.size) yield {
      if (isMax(i)) new CPObjectiveUnitMaximize(objectives(i))
      else new CPObjectiveUnitMinimize(objectives(i))
    }): _*)
    postCut(objective)
    objective.objs.foreach(_.tightenMode = TightenType.NoTighten)
    
    paretoSet = new ListPareto[CPSol](isMax)
    // Adds a dominance constraint with all objectives in minimization mode
    addCut(new ParetoConstraint(paretoSet, isMax, objectives.toArray))
    this
  }
  
  def update(): Unit = propagate()
}