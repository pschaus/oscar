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

import org.scalatest.FunSuite
import oscar.algo.MutableIntMap


class TestMutableIntMap extends FunSuite {

  def generateBaseMap = {
    val mutableIntMap = new MutableIntMap[Integer]()
    mutableIntMap.put(0, 0)
    mutableIntMap.put(1, 1)
    mutableIntMap.put(2, 2)
    mutableIntMap.put(3, 3)
    mutableIntMap.put(4, 4)
    mutableIntMap.put(10, 10)
    mutableIntMap.put(100, 100)
    mutableIntMap.put(-1, -1)
    mutableIntMap.put(-2, -2)

    mutableIntMap
  }

  test("test mutableIntMap put and get") {
    val mutableIntMap = generateBaseMap
    assert(mutableIntMap.get(-2) == -2)
    assert(mutableIntMap.get(-1) == -1)
    assert(mutableIntMap.get(0) == 0)
    assert(mutableIntMap.get(1) == 1)
    assert(mutableIntMap.get(2) == 2)
    assert(mutableIntMap.get(3) == 3)
    assert(mutableIntMap.get(4) == 4)
    assert(mutableIntMap.get(10) == 10)
    assert(mutableIntMap.get(100) == 100)

    assert(mutableIntMap.size == 9)
  }

  test("test MutableIntMap floor/ceil/lower/higher") {
    val mutableIntMap = generateBaseMap

    assert(mutableIntMap.firstEntry().getValue == -2)
    assert(mutableIntMap.lastEntry().getValue == 100)

    assert(mutableIntMap.lowerEntry(-100) == null)
    assert(mutableIntMap.floorEntry(-100) == null)
    assert(mutableIntMap.ceilingEntry(-100).getValue == -2)
    assert(mutableIntMap.higherEntry(-100).getValue == -2)

    assert(mutableIntMap.lowerEntry(-2) == null)
    assert(mutableIntMap.floorEntry(-2).getValue == -2)
    assert(mutableIntMap.ceilingEntry(-2).getValue == -2)
    assert(mutableIntMap.higherEntry(-2).getValue == -1)

    assert(mutableIntMap.lowerEntry(-1).getValue == -2)
    assert(mutableIntMap.floorEntry(-1).getValue == -1)
    assert(mutableIntMap.ceilingEntry(-1).getValue == -1)
    assert(mutableIntMap.higherEntry(-1).getValue == 0)

    assert(mutableIntMap.lowerEntry(0).getValue == -1)
    assert(mutableIntMap.floorEntry(0).getValue == 0)
    assert(mutableIntMap.ceilingEntry(0).getValue == 0)
    assert(mutableIntMap.higherEntry(0).getValue == 1)

    assert(mutableIntMap.lowerEntry(1).getValue == 0)
    assert(mutableIntMap.floorEntry(1).getValue == 1)
    assert(mutableIntMap.ceilingEntry(1).getValue == 1)
    assert(mutableIntMap.higherEntry(1).getValue == 2)

    assert(mutableIntMap.lowerEntry(2).getValue == 1)
    assert(mutableIntMap.floorEntry(2).getValue == 2)
    assert(mutableIntMap.ceilingEntry(2).getValue == 2)
    assert(mutableIntMap.higherEntry(2).getValue == 3)

    assert(mutableIntMap.lowerEntry(3).getValue == 2)
    assert(mutableIntMap.floorEntry(3).getValue == 3)
    assert(mutableIntMap.ceilingEntry(3).getValue == 3)
    assert(mutableIntMap.higherEntry(3).getValue == 4)

    assert(mutableIntMap.lowerEntry(4).getValue == 3)
    assert(mutableIntMap.floorEntry(4).getValue == 4)
    assert(mutableIntMap.ceilingEntry(4).getValue == 4)
    assert(mutableIntMap.higherEntry(4).getValue == 10)

    assert(mutableIntMap.lowerEntry(10).getValue == 4)
    assert(mutableIntMap.floorEntry(10).getValue == 10)
    assert(mutableIntMap.ceilingEntry(10).getValue == 10)
    assert(mutableIntMap.higherEntry(10).getValue == 100)

    assert(mutableIntMap.lowerEntry(100).getValue == 10)
    assert(mutableIntMap.floorEntry(100).getValue == 100)
    assert(mutableIntMap.ceilingEntry(100).getValue == 100)
    assert(mutableIntMap.higherEntry(100) == null)

    assert(mutableIntMap.lowerEntry(50).getValue == 10)
    assert(mutableIntMap.floorEntry(50).getValue == 10)
    assert(mutableIntMap.ceilingEntry(50).getValue == 100)
    assert(mutableIntMap.higherEntry(50).getValue == 100)

    assert(mutableIntMap.lowerEntry(200).getValue == 100)
    assert(mutableIntMap.floorEntry(200).getValue == 100)
    assert(mutableIntMap.ceilingEntry(200) == null)
    assert(mutableIntMap.higherEntry(200) == null)
  }

  test("test MutableIntMap remove") {
    val mutableIntMap = generateBaseMap

    // Let's now delete a key and check the integrity of the tree
    assert(mutableIntMap.remove(3) == 3)
    assert(mutableIntMap.remove(4) == 4)
    assert(mutableIntMap.remove(5) == null)

    assert(mutableIntMap.size == 7)

    assert(mutableIntMap.get(2) == 2)
    assert(mutableIntMap.get(3) == null)
    assert(mutableIntMap.get(4) == null)
    assert(mutableIntMap.get(10) == 10)

    assert(mutableIntMap.lowerEntry(2).getValue == 1)
    assert(mutableIntMap.floorEntry(2).getValue == 2)
    assert(mutableIntMap.ceilingEntry(2).getValue == 2)
    assert(mutableIntMap.higherEntry(2).getValue == 10)

    assert(mutableIntMap.lowerEntry(3).getValue == 2)
    assert(mutableIntMap.floorEntry(3).getValue == 2)
    assert(mutableIntMap.ceilingEntry(3).getValue == 10)
    assert(mutableIntMap.higherEntry(3).getValue == 10)

    assert(mutableIntMap.lowerEntry(4).getValue == 2)
    assert(mutableIntMap.floorEntry(4).getValue == 2)
    assert(mutableIntMap.ceilingEntry(4).getValue == 10)
    assert(mutableIntMap.higherEntry(4).getValue == 10)

    assert(mutableIntMap.lowerEntry(5).getValue == 2)
    assert(mutableIntMap.floorEntry(5).getValue == 2)
    assert(mutableIntMap.ceilingEntry(5).getValue == 10)
    assert(mutableIntMap.higherEntry(5).getValue == 10)

    assert(mutableIntMap.lowerEntry(10).getValue == 2)
    assert(mutableIntMap.floorEntry(10).getValue == 10)
    assert(mutableIntMap.ceilingEntry(10).getValue == 10)
    assert(mutableIntMap.higherEntry(10).getValue == 100)
  }

  test("test MutableIntMap pollFirst/LastEntry") {
    val mutableIntMap = generateBaseMap
    assert(mutableIntMap.pollFirstEntry().getValue == -2)
    assert(mutableIntMap.pollLastEntry().getValue == 100)
    assert(mutableIntMap.pollFirstEntry().getValue == -1)
    assert(mutableIntMap.pollLastEntry().getValue == 10)
    assert(mutableIntMap.pollFirstEntry().getValue == 0)
    assert(mutableIntMap.pollLastEntry().getValue == 4)
    assert(mutableIntMap.pollFirstEntry().getValue == 1)
    assert(mutableIntMap.pollLastEntry().getValue == 3)
    assert(mutableIntMap.pollFirstEntry().getValue == 2)
    assert(mutableIntMap.pollLastEntry() == null)
  }

  test("test MutableIntMap entrySet") {
    val mutableIntMap = generateBaseMap
    val entrySet = mutableIntMap.entrySet()

    val itr1 = entrySet.iterator()
    assert(itr1.next().getValue == -2)
    assert(itr1.next().getValue == -1)
    assert(itr1.next().getValue == 0)
    assert(itr1.next().getValue == 1)
    assert(itr1.next().getValue == 2)
    assert(itr1.next().getValue == 3)
    assert(itr1.next().getValue == 4)
    assert(itr1.next().getValue == 10)
    assert(itr1.next().getValue == 100)

    val itr2 = entrySet.iterator()
    assert(itr2.next().getValue == -2)
    assert(itr2.next().getValue == -1)
    assert(itr2.next().getValue == 0)
    itr2.remove()
    assert(itr2.next().getValue == 1)
    assert(itr2.next().getValue == 2)
    assert(itr2.next().getValue == 3)
    assert(itr2.next().getValue == 4)
    assert(itr2.next().getValue == 10)
    assert(itr2.next().getValue == 100)

    val itr3 = entrySet.iterator()
    assert(itr3.next().getValue == -2)
    assert(itr3.next().getValue == -1)
    assert(itr3.next().getValue == 1)
    assert(itr3.next().getValue == 2)
    assert(itr3.next().getValue == 3)
    assert(itr3.next().getValue == 4)
    assert(itr3.next().getValue == 10)
    assert(itr3.next().getValue == 100)

    assert(!mutableIntMap.containsKey(0))
  }
}