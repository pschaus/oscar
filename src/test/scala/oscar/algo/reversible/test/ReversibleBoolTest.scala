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
package oscar.algo.reversible.test


import org.scalatest.funsuite.AnyFunSuite
import oscar.algo.reversible.{ReversibleBoolean, ReversibleContext}

class ReversibleBoolTest extends AnyFunSuite {

  test("test reversibility bool") {

    val r = ReversibleContext()
    val a = new ReversibleBoolean(r, true)

    r.pushState()

    a.value = false
    a.value = true

    r.pop()

    assert(a.value)





  }
  

}

