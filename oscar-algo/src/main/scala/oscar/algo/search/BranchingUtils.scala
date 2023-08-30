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

package oscar.algo.search

import oscar.algo.Inconsistency

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
trait BranchingUtils {
  
  type Alternative = Function0[Unit]
  
  def branch(left: => Unit)(right: => Unit): Seq[Alternative] = Seq(() => left,() => right)
  
  def branchOne(action: => Unit) = Seq(() => action)
  
  def branchAll[A](indexes: Seq[A])(f: A => Unit): Seq[Alternative] = {
    indexes.map(i => () => f(i))
  }
  
  val noAlternative = Seq[Alternative]()

  final def isInconsistent(clo: => Unit): Boolean = {
    try {
      clo
      false
    }
    catch {
      case _: Inconsistency => true
    }
  }
}
