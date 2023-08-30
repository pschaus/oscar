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
import oscar.cp.modeling.Constraints
import oscar.cp.core.Constraint

/**
 * Provides "syntactic sugar" for declaring modulo constraints. CPIntVar has 
 * an overloaded % operator so that it'll return this case class representing the
 * Left Hand Side of a modulo constraint. The case class provides two overloaded
 * equality operators that use the existing modulo constraint wrapper. 
 * @author Alastair Andrew
 */
case class ModuloLHS(x: CPIntVar, v: Int) extends Constraints {
	  def ==(y: CPIntVar): Constraint = modulo(x, v, y)
	  def ==(y: Int): Constraint = modulo(x, v, y)
}