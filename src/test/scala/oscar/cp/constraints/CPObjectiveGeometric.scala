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

import oscar.cp.core.variables.CPIntVar
import oscar.cp._

abstract class CPObjectiveGeometric(objVar: CPIntVar, name: String, ratio: Double)
extends CPObjectiveUnit(objVar, name) {
  require(0 < ratio && ratio < 1)
  
  override protected val lb = math.floor(objVar.min * (1 / (1 + ratio))).toInt
  override protected val ub = math.ceil( objVar.max * (1 / (1 - ratio))).toInt
}


/** Best  : smallest value
 *  Worst : largest value
 */
class CPObjectiveGeometricMinimize(objVar: CPIntVar, name: String = "", ratio: Double = 0.001)
extends CPObjectiveGeometric(objVar, name, ratio) {
  def domBest: Int = objVar.min
  def domWorst: Int = objVar.max 
  def isMax: Boolean = false
  def isMin: Boolean = true 
  
  def updateWorstBound(newBound: Int, delta: Int = 0): Unit =
    objVar.updateMax(newBound - math.ceil(ratio * newBound * delta).toInt) 
  
  def updateBestBound(newBound: Int, delta: Int = 0): Unit =
    objVar.updateMin(newBound + math.ceil(ratio * newBound * delta).toInt) 
    
  def worstBound: Int = ub
  def bestBound: Int = lb
  
  // Init best
  best = ub
}

/** Best  : largest value
 *  Worst : smallest value
 */
class CPObjectiveGeometricMaximize(objVar: CPIntVar, name: String = "", ratio: Double = 0.001)
extends CPObjectiveGeometric(objVar, name, ratio) {

  def domBest: Int = objVar.max
  def domWorst: Int = objVar.min 
  def isMax: Boolean = true
  def isMin: Boolean = false
  def updateWorstBound(newBound: Int, delta: Int = 0): Unit =
    objVar.updateMin(newBound + math.ceil(ratio * newBound * delta).toInt) 
  
  def updateBestBound(newBound: Int, delta: Int = 0): Unit =
    objVar.updateMax(newBound - math.ceil(ratio * newBound * delta).toInt)
    
  def worstBound: Int = lb
  def bestBound: Int = ub
  
  // Init best
  best = lb
}
