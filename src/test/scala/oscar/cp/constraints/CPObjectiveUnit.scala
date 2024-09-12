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

package oscar.cp.constraints

import oscar.algo.search.isInconsistent
import oscar.cp.TightenType
import oscar.cp.TightenType._
import oscar.cp.core.variables.CPIntVar
import oscar.algo.search.Objective

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class CPObjectiveUnit(val objVar: CPIntVar, val n: String = "") extends Objective {
  
  // Upper bound of the objective
  protected val lb = objVar.min - 1 // Avoids to remove the min value in the first propagate
  // Lower bound of the objective
  protected val ub = objVar.max + 1 // Avoids to remove the max value in the first propagate
  // Tightening mode of the objective
  protected var tightenType = StrongTighten
  // Best so far value of the objective (the one recorded on the last tighten)
  var best = 0
  
  /** Returns the best value in the current domain of the objective */
  def domBest: Int
  /** Returns the worst value in the current domain of the objective */
  def domWorst: Int
  /** Returns true if the objective has to be maximized, false otherwise */
  def isMax: Boolean
  /** Returns true if the objective has to be minimized, false otherwise */
  def isMin: Boolean
  /** Tries to adjust the worst bound of the domain to newBound with delta */
  def updateWorstBound(newBound: Int, delta: Int = 0): Unit
  /** Tries to adjust the best bound of the domain to newBound with delta */
  def updateBestBound(newBound: Int, delta: Int = 0): Unit
  /** Returns the value of the worst bound of the objective */
  def worstBound: Int
  /** Returns the value of the best bound of the objective */
  def bestBound: Int

  /** Sets the tightening mode of the objective */
  def tightenMode_=(t: TightenType.Value) = {
    tightenType = t
  }
  
  /** Returns the tightening mode of the objective */
  def tightenMode = tightenType
  
  /** Tightens the objective according to its tightening mode */
  def tighten(): Unit = {
    if (!objVar.isBound) {
      if (tightenType != NoTighten) throw new RuntimeException("objective"+n+" not bound:" + objVar)
    }
    else {
      best = objVar.min // Sets new best value
      if (!objVar.store.silent && tightenType != NoTighten) {
        println("objective"+n+" tightened to " + best + " lb:"+  lb)
      }
    }
  }

  /** Returns true if the objective has reached is optimal value, false otherwise */
  def isOptimum: Boolean = (best == bestBound)

  /** Returns true if the objective is consistent according to its model */
  def isOK(): Boolean = !isInconsistent(ensureBest())
  
  /** Restores the lower and upper bounds of the objective as well as its best so far value */
  def relax(): Unit = {
    best = worstBound
  }
  
  /** Adjusts the bounds of the objective according to the best so far value and to the 
   *  tightening mode */
  def ensureBest(): Unit = {
    if (tightenType != NoTighten) {
      val delta = if (tightenType == StrongTighten) 1 else 0
      updateWorstBound(best, delta)
    }
  }
  
  override def toString: String = "best value: "+best+" tightening: "+tightenType
}

/** Best  : smallest value
 *  Worst : largest value
 */
class CPObjectiveUnitMinimize(objVar: CPIntVar, n: String = "") extends CPObjectiveUnit(objVar, n) {

  def domBest: Int = objVar.min
  def domWorst: Int = objVar.max 
  def isMax: Boolean = false
  def isMin: Boolean = true 
  def updateWorstBound(newBound: Int, delta: Int = 0): Unit = {
    objVar.updateMax(newBound-delta) 
  }
  def updateBestBound(newBound: Int, delta: Int = 0): Unit = objVar.updateMin(newBound+delta)
  def worstBound: Int = ub
  def bestBound: Int = lb
  
  // Init best
  best = ub
}

/** Best  : largest value
 *  Worst : smallest value
 */
class CPObjectiveUnitMaximize(objVar: CPIntVar, n: String = "") extends CPObjectiveUnit(objVar, n) {

  def domBest: Int = objVar.max
  def domWorst: Int = objVar.min 
  def isMax: Boolean = true
  def isMin: Boolean = false
  def updateWorstBound(newBound: Int, delta: Int = 0): Unit = objVar.updateMin(newBound+delta)
  def updateBestBound(newBound: Int, delta: Int = 0): Unit = objVar.updateMax(newBound-delta)
  def worstBound: Int = lb
  def bestBound: Int = ub
  
  // Init best
  best = lb
}
