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

import org.scalatest.FunSuite
import oscar.algo.reversible._

/*
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleSetSparsetSetJavaTest extends FunSuite {

  test("test reversible set 1") {

    val rc = new ReversibleContextImpl()

    val s = new ReversibleSparseSetJava(rc,0,10,true)

    rc.pushState()

    s.insert(5)
    s.insert(2)
    s.insert(5)

    assert(s.getSize == 2)

    assert(s.toArray.toSet == s.getValues.toSet)


    rc.pushState()

    s.insert(5)
    s.insert(10)
    s.insert(3)
    s.insert(4)
    s.insert(0)

    assert(s.toArray.toSet == s.getValues.toSet)
    assert(s.toArray.toSet == Set(0,2,5,3,4,10))

    rc.pop()

    assert(s.toArray.toSet == s.getValues.toSet)
    assert(s.toArray.toSet == Set(2,5))



  }
  

}

