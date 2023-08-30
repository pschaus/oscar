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

package oscar.cp.constraints;

import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.algo.reversible.ReversibleInt

import scala.math.min
import scala.math.max
import oscar.cp.core._
import oscar.cp.core.CPSolver
import oscar.algo.reversible.ReversibleInt
import java.security.InvalidParameterException



/**
 * n = x.size-1 = y.size-1
 * x is a permutation of {0 ... n-1} and y also
 * x and y are such that x(y(i)) = i i.e. y(i) is the position of number i in x
 *
 * @author Pierre Schaus - pschaus@gmail.com
 */
class Permutation(x: Array[CPIntVar], y: Array[CPIntVar]) extends Constraint(y(0).store, "Permutation") {

  override def associatedVars(): Iterable[CPVar] = x ++ y

  val n = x.size-1
  if (x.size != y.size) throw new InvalidParameterException("x and y must have the same size")
  
  override def setup(l: CPPropagStrength): Unit = {
    
    for (i <- 0 to n) {
      x(i).updateMin(0)
      y(i).updateMin(0)
      x(i).updateMax(n)
      y(i).updateMax(n)
    }
    s.post(new AllDifferent(x:_*),l)
    s.post(new AllDifferent(y:_*),l)
    
    for(i <- 0 to n; v <- 0 to n) {
      if (!x(i).hasValue(v)) {
        y(v).removeValue(i)
      }
      if (!y(i).hasValue(v)) {
        x(v).removeValue(i)
      }
    }
    for(i <- 0 to n) {
      x(i).callValRemoveIdxWhenValueIsRemoved(this, i)
      y(i).callValRemoveIdxWhenValueIsRemoved(this, n+1+i)
    }
  }


  override def valRemoveIdx(cpvar: CPIntVar, i: Int, v: Int): Unit = {
    if (i <= n) {
      // x(i) lost the value v
      y(v).removeValue(i)
    } else {
      // y(i-n-1) lost the value v
      x(v).removeValue(i-n-1)
    }
  }


}
