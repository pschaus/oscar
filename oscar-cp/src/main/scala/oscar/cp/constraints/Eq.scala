/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.cp.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPPropagStrength._
import oscar.cp.core.CPStore

class EqCons(x: CPIntVar, v: Int) extends Constraint(x.store, "Equality") {
  override def associatedVars(): Iterable[CPVar] = Array(x)

  final override def setup(l: CPPropagStrength): Unit = {
    x.assign(v)
  }
}

class Eq(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "Equality") {

  override def associatedVars(): Iterable[CPVar] = Array(x, y)

  final override def setup(l: CPPropagStrength): Unit = {
    if (l == Strong) s.post(equalityStrong(x, y))
    else if (l == Medium) s.post(new EqualityBC(x, y))
    else if (l == Weak || l == Automatic) s.post(new EqualityBC(x, y))
    else sys.error("unknown propagation level") 
  }
  
  private def equalityStrong(x: CPIntVar, y: CPIntVar): Constraint = {
    if (x.size == 2 || y.size == 2) new EqualityBC(x, y)
    else new EqualityDC(x, y)
  }
}