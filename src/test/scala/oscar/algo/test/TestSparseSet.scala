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
package oscar.algo.test

import oscar.algo.reversible.SparseSet

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertions
class TestSparseSet extends AnyFunSuite {

  test("test ds1") {
    val set = new SparseSet(5,10,true) // empty sparse-set
    val tmp = Array.ofDim[Int](200)

    assert(set.isEmpty)
    assert(set.getSize == 0)
    var s = set.fillArray(tmp)
    assert(s == 0)
    set.insert(6)
    set.insert(8)
    set.insert(10)
    set.removeValue(8)

    s = set.fillArray(tmp)
    assert(tmp.take(s).toSet == Set(6,10))
    assert(s == 2)

    set.empty()
    assert(set.getSize() == 0)

  }
  



}

