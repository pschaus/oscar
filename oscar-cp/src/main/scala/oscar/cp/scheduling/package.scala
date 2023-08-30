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

package oscar.cp

import oscar.cp.core._
package object scheduling {

  /**
   * prune such that activity 1 << activity 2
   */
  def precedes(s1: CPIntVar, d1: CPIntVar, e1: CPIntVar, s2: CPIntVar, d2: CPIntVar, e2: CPIntVar): Unit = {
      s2.updateMin(e1.min)
      e1.updateMax(s2.max)
  }

  /**
   * ensure s+d = e
   */
  def update(s: CPIntVar, d: CPIntVar, e: CPIntVar): Unit = {
    // end <= start
    e.updateMin(s.min)
    s.updateMax(e.max)

    // end = start + dur
    e.updateMax(s.max + d.max)
    e.updateMin(s.min + d.min)

    // start = end - dur
    s.updateMax(e.max - d.min)
    s.updateMin(e.min - d.max)

    // dur = end - start
    d.updateMax(e.max - s.min)
    d.updateMin(e.min - s.max)
  }
}