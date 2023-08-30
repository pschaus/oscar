package oscar.cp.test

import org.scalatest.{FunSuite, Matchers}
import oscar.algo.reversible.{BitSetOp, ReversibleSparseBitSet}
import oscar.cp._

/**
 * Created by helene on 1/08/17.
 */
class TestReversibleSparseBitSet extends FunSuite with Matchers {

  test("Test BitSetOp 1 : bitLength") {
    for {
      off <- 0 until 1024
      i <- 0 until 64
      if off != 0 && i != 0
    } {
      BitSetOp.bitLength((off << 6) + i) shouldBe (off + 1)
    }
  }
  test("Test BitSetOp 2 : oneBitLong") {
    for {
      pos <- 0 until 1000
    } {
      val k = BitSetOp.oneBitLong(pos)
      java.lang.Long.bitCount(k) shouldBe 1
      k shouldBe (1L << (pos % 64))
      java.lang.Long.numberOfTrailingZeros(k) shouldBe (pos % 64)
    }
  }
  test("Test BitSetOp 3 : bitOffset") {
    for {
      off <- 0 until 1024
      i <- 0 until 64
      if off != 0 && i != 0
    } {
      BitSetOp.bitOffset((off << 6) + i) shouldBe off
    }
  }
  test("Test BitSetOp 4 : setBit") {
    for {
      off <- 0 until 1024
      i <- 0 until 64
      if off != 0 && i != 0
    } {
      BitSetOp.bitOffset((off << 6) + i) shouldBe off
    }
  }

  test("Test ReversibleSparseBitSet 1") {
    val cp = CPSolver()

    val s = new ReversibleSparseBitSet(cp, 300, 0 until 300)

    s.isEmpty() shouldBe false

    val bs1 = new s.BitSet(0 until 100)
    val bs2 = new s.BitSet(100 until 200)
    val bs3 = new s.BitSet(200 until 300)

    s.clearCollected()
    s.collect(bs1)
    s.removeCollected()

    s.isEmpty() shouldBe false

    s.clearCollected()
    s.collect(bs2)
    s.reverseCollected()
    s.intersectCollected()

    s.isEmpty() shouldBe false

    s.clearCollected()
    s.collect(bs3)
    s.intersectCollected()

    s.isEmpty() shouldBe false

    s.clearCollected()
    s.collect(bs3)
    s.removeCollected()

    s.isEmpty() shouldBe true

  }

  test("Test ReversibleSparseBitSet 2 : intersect") {
    val cp = CPSolver()

    val s = new ReversibleSparseBitSet(cp, 300, 0 until 300)

    s.isEmpty() shouldBe false

    val bs0 = new s.BitSet(List())
    val bs1 = new s.BitSet(0 until 100)
    val bs2 = new s.BitSet(100 until 200)
    val bs3 = new s.BitSet(200 until 300)

    s.intersect(bs0) shouldBe false
    s.intersect(bs1) shouldBe true
    s.intersect(bs2) shouldBe true
    s.intersect(bs3) shouldBe true
  }

  test("Test ReversibleSparseBitSet 3 : intersectCount") {
    val cp = CPSolver()

    val s = new ReversibleSparseBitSet(cp, 300, 0 until 300)

    s.isEmpty() shouldBe false

    val bs0 = new s.BitSet(List())
    val bs1 = new s.BitSet(0 until 100)
    val bs2 = new s.BitSet(100 until 200)
    val bs3 = new s.BitSet(200 until 300)

    s.intersectCount(bs0) shouldBe 0
    s.intersectCount(bs1) shouldBe 100
    s.intersectCount(bs2) shouldBe 100
    s.intersectCount(bs3) shouldBe 100
  }
}
