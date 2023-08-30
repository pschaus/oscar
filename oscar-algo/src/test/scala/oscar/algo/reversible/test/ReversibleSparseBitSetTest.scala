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

import java.util

import org.scalatest.FunSuite
import oscar.algo.reversible._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleSparseBitSetTest extends FunSuite {

  test("test reversible sparse bitset 1") {

    val r = new ReversibleContextImpl()

    val rset = new ReversibleSparseBitSet(r, 40,Set(0,2,3,4))

    val set1: rset.BitSet = new rset.BitSet(Set(1,3,4))
    val set2 = new rset.BitSet(Set(0,1,2))

    r.pushState()

    rset.clearCollected()
    rset.collect(set1)
    rset.collect(set2)

    rset.removeCollected()

    assert(!rset.intersect(set2))


    r.pop()
    // rset = {0,2,3,4}

    assert(rset.intersect(set2))

    rset.clearCollected()
    rset.collect(set1)

    rset.intersectCollected()
    // rset = {0,2,3,4} - {1,3,4} = {3,4}
    assert(rset.intersect(set1))
    rset.removeCollected()
    // rset =  {3,4} - {1,3,4} = phi
    assert(!rset.intersect(set1))

  }

  test("test reversible sparse bitset 2") {

    val r = new ReversibleContextImpl()

    val rset = new ReversibleSparseBitSet(r, 600,Set(0,200,300,400))

    val set1 = new rset.BitSet(Set(100,300,400))
    val set2 = new rset.BitSet(Set(0,100,200))

    r.pushState()

    rset.clearCollected()
    rset.collect(set1)
    rset.collect(set2)

    rset.removeCollected()

    assert(!rset.intersect(set2))


    r.pop()
    // rset = {0,2,3,4}

    assert(rset.intersect(set2))

    rset.clearCollected()
    rset.collect(set1)

    rset.intersectCollected()
    // rset = {0,2,3,4} - {1,3,4} = {3,4}

    assert(rset.intersect(set1))
    rset.removeCollected()
    // rset =  {3,4} - {1,3,4} = phi
    assert(!rset.intersect(set1))

  }


  test("test3") {


    val r = new ReversibleContextImpl()
    val b = new ReversibleSparseBitSet(r,65,Set(1,2,63,64))//null
    val set1 = new b.BitSet(Set(1,2))

    val set2 = new b.BitSet(Set(2,5))
    val set3 = new b.BitSet(Set(63))
    val set4 = new b.BitSet(Set(64))

    b.clearCollected()
    b.collect(set1)

    b.removeCollected()

    assert(!b.isEmpty())
    assert(b.intersect(set3))
    assert(b.intersect(set4))
    assert(!b.intersect(set2))

    b.collect(set3)
    b.removeCollected()
    assert(!b.intersect(set3))
    assert(b.intersect(set4))
    assert(!b.isEmpty())
    b.collect(set4)
    b.removeCollected()
    assert(!b.intersect(set4))
    assert(b.isEmpty())


    //println(set1)

    //val variableValueSupport = Array.tabulate(4,3){case(i,j) => new rset.BitSet(Set(1))}
  }


  test("test4") {



    val r = new ReversibleContextImpl()
    val b = new ReversibleSparseBitSet(r,65,Set(1,2,63,64))

    val set1 = new b.BitSet(Set(1,2))
    val set2 = new b.BitSet(Set(2,5))
    val set3 = new b.BitSet(Set(63))
    val set4 = new b.BitSet(Set(64))

    b.collect(set1)
    var changed = false
    changed = b.removeCollected()
    assert(changed)
    changed = b.removeCollected()
    assert(!changed)
    assert(!b.isEmpty())
    changed = b.intersectCollected()
    assert(changed)
    assert(b.isEmpty())

  }

  test("test5") {

    val r = new ReversibleContextImpl()
    val b = new ReversibleSparseBitSet(r,65,Set(1,2,63,64))

    val set1 = new b.BitSet(Set(1,2))
    val set2 = new b.BitSet(Set(2,5))
    val set3 = new b.BitSet(Set(63))
    val set4 = new b.BitSet(Set(64))
    val set5 = new b.BitSet(Set(1,2,20,63,64))
    var changed = false

    b.collect(set4)
    changed = b.removeCollected()
    assert(changed)
    b.clearCollected()
    b.collect(set5)
    changed = b.intersectCollected()
    assert(!changed)


  }


}

