package oscar.algo.test


import oscar.algo.MutableIntMap
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertions

class TestMutableIntMap extends AnyFunSuite {

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
    assert(mutableIntMap.get(-2).contains(-2))
    assert(mutableIntMap.get(-1).contains(-1))
    assert(mutableIntMap.get(0).contains(0))
    assert(mutableIntMap.get(1).contains(1))
    assert(mutableIntMap.get(2).contains(2))
    assert(mutableIntMap.get(3).contains(3))
    assert(mutableIntMap.get(4).contains(4))
    assert(mutableIntMap.get(10).contains(10))
    assert(mutableIntMap.get(100).contains(100))

    assert(mutableIntMap.size == 9)
  }

  test("test MutableIntMap floor/ceil/lower/higher") {
    val mutableIntMap = generateBaseMap

    assert(mutableIntMap.firstEntry().get.getValue == -2)
    assert(mutableIntMap.lastEntry().get.getValue == 100)

    assert(mutableIntMap.lowerEntry(-100).isEmpty)
    assert(mutableIntMap.floorEntry(-100).isEmpty)
    assert(mutableIntMap.ceilingEntry(-100).get.getValue == -2)
    assert(mutableIntMap.higherEntry(-100).get.getValue == -2)

    assert(mutableIntMap.lowerEntry(-2).isEmpty)
    assert(mutableIntMap.floorEntry(-2).get.getValue == -2)
    assert(mutableIntMap.ceilingEntry(-2).get.getValue == -2)
    assert(mutableIntMap.higherEntry(-2).get.getValue == -1)

    assert(mutableIntMap.lowerEntry(-1).get.getValue == -2)
    assert(mutableIntMap.floorEntry(-1).get.getValue == -1)
    assert(mutableIntMap.ceilingEntry(-1).get.getValue == -1)
    assert(mutableIntMap.higherEntry(-1).get.getValue == 0)

    assert(mutableIntMap.lowerEntry(0).get.getValue == -1)
    assert(mutableIntMap.floorEntry(0).get.getValue == 0)
    assert(mutableIntMap.ceilingEntry(0).get.getValue == 0)
    assert(mutableIntMap.higherEntry(0).get.getValue == 1)

    assert(mutableIntMap.lowerEntry(1).get.getValue == 0)
    assert(mutableIntMap.floorEntry(1).get.getValue == 1)
    assert(mutableIntMap.ceilingEntry(1).get.getValue == 1)
    assert(mutableIntMap.higherEntry(1).get.getValue == 2)

    assert(mutableIntMap.lowerEntry(2).get.getValue == 1)
    assert(mutableIntMap.floorEntry(2).get.getValue == 2)
    assert(mutableIntMap.ceilingEntry(2).get.getValue == 2)
    assert(mutableIntMap.higherEntry(2).get.getValue == 3)

    assert(mutableIntMap.lowerEntry(3).get.getValue == 2)
    assert(mutableIntMap.floorEntry(3).get.getValue == 3)
    assert(mutableIntMap.ceilingEntry(3).get.getValue == 3)
    assert(mutableIntMap.higherEntry(3).get.getValue == 4)

    assert(mutableIntMap.lowerEntry(4).get.getValue == 3)
    assert(mutableIntMap.floorEntry(4).get.getValue == 4)
    assert(mutableIntMap.ceilingEntry(4).get.getValue == 4)
    assert(mutableIntMap.higherEntry(4).get.getValue == 10)

    assert(mutableIntMap.lowerEntry(10).get.getValue == 4)
    assert(mutableIntMap.floorEntry(10).get.getValue == 10)
    assert(mutableIntMap.ceilingEntry(10).get.getValue == 10)
    assert(mutableIntMap.higherEntry(10).get.getValue == 100)

    assert(mutableIntMap.lowerEntry(100).get.getValue == 10)
    assert(mutableIntMap.floorEntry(100).get.getValue == 100)
    assert(mutableIntMap.ceilingEntry(100).get.getValue == 100)
    assert(mutableIntMap.higherEntry(100).isEmpty)

    assert(mutableIntMap.lowerEntry(50).get.getValue == 10)
    assert(mutableIntMap.floorEntry(50).get.getValue == 10)
    assert(mutableIntMap.ceilingEntry(50).get.getValue == 100)
    assert(mutableIntMap.higherEntry(50).get.getValue == 100)

    assert(mutableIntMap.lowerEntry(200).get.getValue == 100)
    assert(mutableIntMap.floorEntry(200).get.getValue == 100)
    assert(mutableIntMap.ceilingEntry(200).isEmpty)
    assert(mutableIntMap.higherEntry(200).isEmpty)
  }

  test("test MutableIntMap remove") {
    val mutableIntMap = generateBaseMap

    // Let's now delete a key and check the integrity of the tree
    assert(mutableIntMap.remove(3).contains(3))
    assert(mutableIntMap.remove(4).contains(4))
    assert(mutableIntMap.remove(5).isEmpty)

    assert(mutableIntMap.size == 7)

    assert(mutableIntMap.get(2).contains(2))
    assert(mutableIntMap.get(3).isEmpty)
    assert(mutableIntMap.get(4).isEmpty)
    assert(mutableIntMap.get(10).contains(10))

    assert(mutableIntMap.lowerEntry(2).get.getValue == 1)
    assert(mutableIntMap.floorEntry(2).get.getValue == 2)
    assert(mutableIntMap.ceilingEntry(2).get.getValue == 2)
    assert(mutableIntMap.higherEntry(2).get.getValue == 10)

    assert(mutableIntMap.lowerEntry(3).get.getValue == 2)
    assert(mutableIntMap.floorEntry(3).get.getValue == 2)
    assert(mutableIntMap.ceilingEntry(3).get.getValue == 10)
    assert(mutableIntMap.higherEntry(3).get.getValue == 10)

    assert(mutableIntMap.lowerEntry(4).get.getValue == 2)
    assert(mutableIntMap.floorEntry(4).get.getValue == 2)
    assert(mutableIntMap.ceilingEntry(4).get.getValue == 10)
    assert(mutableIntMap.higherEntry(4).get.getValue == 10)

    assert(mutableIntMap.lowerEntry(5).get.getValue == 2)
    assert(mutableIntMap.floorEntry(5).get.getValue == 2)
    assert(mutableIntMap.ceilingEntry(5).get.getValue == 10)
    assert(mutableIntMap.higherEntry(5).get.getValue == 10)

    assert(mutableIntMap.lowerEntry(10).get.getValue == 2)
    assert(mutableIntMap.floorEntry(10).get.getValue == 10)
    assert(mutableIntMap.ceilingEntry(10).get.getValue == 10)
    assert(mutableIntMap.higherEntry(10).get.getValue == 100)
  }

  test("test MutableIntMap pollFirst/LastEntry") {
    val mutableIntMap = generateBaseMap
    assert(mutableIntMap.pollFirstEntry().get.getValue == -2)
    assert(mutableIntMap.pollLastEntry().get.getValue == 100)
    assert(mutableIntMap.pollFirstEntry().get.getValue == -1)
    assert(mutableIntMap.pollLastEntry().get.getValue == 10)
    assert(mutableIntMap.pollFirstEntry().get.getValue == 0)
    assert(mutableIntMap.pollLastEntry().get.getValue == 4)
    assert(mutableIntMap.pollFirstEntry().get.getValue == 1)
    assert(mutableIntMap.pollLastEntry().get.getValue == 3)
    assert(mutableIntMap.pollFirstEntry().get.getValue == 2)
    assert(mutableIntMap.pollLastEntry().isEmpty)
  }

  test("test MutableIntMap entrySet") {
    val mutableIntMap = generateBaseMap
    val entrySet = mutableIntMap

    val itr1 = entrySet.iterator
    assert(itr1.next()._2 == -2)
    assert(itr1.next()._2 == -1)
    assert(itr1.next()._2 == 0)
    assert(itr1.next()._2 == 1)
    assert(itr1.next()._2 == 2)
    assert(itr1.next()._2 == 3)
    assert(itr1.next()._2 == 4)
    assert(itr1.next()._2 == 10)
    assert(itr1.next()._2 == 100)

    val itr2 = entrySet.iterator
    assert(itr2.next()._2 == -2)
    assert(itr2.next()._2 == -1)
    assert(itr2.next()._2 == 0)
    mutableIntMap.remove(0)
    assert(itr2.next()._2 == 1)
    assert(itr2.next()._2 == 2)
    assert(itr2.next()._2 == 3)
    assert(itr2.next()._2 == 4)
    assert(itr2.next()._2 == 10)
    assert(itr2.next()._2 == 100)

    val itr3 = entrySet.iterator
    assert(itr3.next()._2 == -2)
    assert(itr3.next()._2 == -1)
    assert(itr3.next()._2 == 1)
    assert(itr3.next()._2 == 2)
    assert(itr3.next()._2 == 3)
    assert(itr3.next()._2 == 4)
    assert(itr3.next()._2 == 10)
    assert(itr3.next()._2 == 100)

    assert(!mutableIntMap.contains(0))
  }
}