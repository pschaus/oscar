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

import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp.core.variables.{CPBoolVar, CPIntVar, CPVar}

/**
 * Implementation of sum_i a(i).x(i) == c <--> b
 * @author Pierre Schaus pschaus@gmail.com
 */
class WeightedSumReif(val a: Array[Int], val x: Array[CPIntVar], val c: Int, val b: CPBoolVar) extends Constraint(b.store, "WeightedSumReif") {

  override def associatedVars(): Iterable[CPVar] = x ++ Array(b)

  override def setup(l: CPPropagStrength): Unit = {
    x.foreach(_.callPropagateWhenDomainChanges(this))
    b.callPropagateWhenBind(this)
  }

  override def propagate(): Unit = {
    if (b.isBoundTo(1)) {
      s.post(new oscar.cp.constraints.WeightedSum(a,x,CPIntVar(c)(s)))
      deactivate()
    }
    else {
      val m = a.zip(x).map{case(ai,xi) => if (ai < 0) ai*xi.max else ai*xi.min}.sum
      if (m > c) {
        b.assign(0)
        deactivate()
      }
      val M = a.zip(x).map{case(ai,xi) => if (ai < 0) ai*xi.min else ai*xi.max}.sum
      if (M < c) {
        b.assign(0)
        deactivate()
      }
      if (m == M) {
        b.assign(1)
        deactivate()
      }
    }
  }
}


