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

import java.security.InvalidParameterException


class DiscrepancyBranching(branching: Branching, maxDiscrepancy: Int) extends Branching {

  if (maxDiscrepancy < 0) throw new InvalidParameterException("discrepancy should be >= 0")

  private[this] var discrepancy = 0

  final override def alternatives: Seq[Alternative] = {
    val alternatives = branching.alternatives
    if (alternatives.isEmpty) noAlternative
    else {
      val k = math.min(maxDiscrepancy - discrepancy + 1, alternatives.length)
      // Mapped alternative
      val mappedAlternatives = new Array[Alternative](k)
      var i = 0
      while (i < k) {
        val alternative = alternatives(i)
        val newDiscrepancy = discrepancy + i
        mappedAlternatives(i) = () => {
          discrepancy = newDiscrepancy
          alternative.apply()
        }
        i += 1
      }
      mappedAlternatives
    }
  }
}