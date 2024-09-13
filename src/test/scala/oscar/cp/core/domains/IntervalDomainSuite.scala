package oscar.cp.core.domains

import oscar.cp.testUtils.TestSuite
import oscar.cp.isInconsistent
import oscar.algo.reversible.{ReversibleContext, ReversibleContextImpl}

import scala.util.Random

/**
 *  Test the bound implementation of an interval domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class BoundDomainSuite extends IntervalDomainSuite {
  override def intervalDomain(context: ReversibleContext, minValue: Int, maxValue: Int): IntervalDomain = {
    new BoundDomain(context, minValue, maxValue)
  }
}

/**
 *  Generic class to test implementations of an interval domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class IntervalDomainSuite extends TestSuite {
  
  /** Implement this method to test an implementation of `IntervalDomain` */
  def intervalDomain(context: ReversibleContext, minValue: Int, maxValue: Int): IntervalDomain
  
  // Returns true if the domain contains all the values between min and max
  private def containsAll(domain: IntervalDomain): Boolean = {
    val min = domain.min
    val max = domain.max
    (min to max).forall(domain.hasValue)
  }
  
  test("All values should be contained in the initial domain") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 10)
    assert(domain.size == 6)
    assert((5 to 10).forall(domain.hasValue))
  }
  
  test("HasValue should return true if value is in the domain") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    assert(domain.hasValue(5))
    assert(domain.hasValue(10))
    assert(domain.hasValue(15))
  }
  
  test("HasValue should return false if value is not in the domain") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    assert(!domain.hasValue(-1000))
    assert(!domain.hasValue(-10))
    assert(!domain.hasValue(4))
    assert(!domain.hasValue(16))
    assert(!domain.hasValue(20))
    assert(!domain.hasValue(1000))
  }
  
  test("UpdateMin should adjust the minimum value and the size") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    assert(domain.size == 11)
    domain.updateMin(10)
    assert(domain.size == 6)
    assert(domain.min == 10)
  }
  
  test("UpdateMin should remove all values lesser than min") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    domain.updateMin(10)
    assert(containsAll(domain))
  }
  
  test("UpdateMin with a lesser or equal value than min should not impact the domain") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    domain.updateMin(4)
    assert(domain.size == 11)
    assert(domain.min == 5)
    assert(domain.max == 15)
    domain.updateMin(5)
    assert(domain.size == 11)
    assert(domain.min == 5)
    assert(domain.max == 15)
  }
  
  test("UpdateMin to max should assign max") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    domain.updateMin(15)
    assert(domain.size == 1)
    assert(domain.isBound)
    assert(domain.hasValue(15))
  }
  
  test("UpdateMin greater than max should fail") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    assert(isInconsistent(domain.updateMin(20)))
    assert(domain.size == 0)
  }
  
  test("UpdateMax should adjust the maximum value and the size") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    assert(domain.size == 11)
    domain.updateMax(10)
    assert(domain.size == 6)
    assert(domain.max == 10)
  }
  
  test("UpdateMax should remove all values greater than max") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    domain.updateMax(10)
    assert(containsAll(domain))
  }
  
  test("UpdateMax with a greater or equal value than max should not impact the domain") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    domain.updateMax(20)
    assert(domain.size == 11)
    assert(domain.min == 5)
    assert(domain.max == 15)
    domain.updateMax(15)
    assert(domain.size == 11)
    assert(domain.min == 5)
    assert(domain.max == 15)
  }
  
  test("UpdateMax to min should assign min") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    domain.updateMax(5)
    assert(domain.size == 1)
    assert(domain.isBound)
    assert(domain.hasValue(5))
  }
  
  test("UpdateMax lesser than min should fail") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    assert(isInconsistent(domain.updateMax(0)))
    assert(domain.size == 0)
  }
  
  test("Bounds should be restored when a backtrack occurs") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    context.pushState()
    domain.updateMax(10)
    assert(domain.min == 5)
    assert(domain.max == 10)
    assert(domain.size == 6)
    assert(containsAll(domain))
    context.pushState()
    domain.updateMax(9)
    domain.updateMin(6)
    assert(domain.min == 6)
    assert(domain.max == 9)
    assert(domain.size == 4)
    assert(containsAll(domain))
    context.pushState()
    context.pop()
    assert(domain.min == 6)
    assert(domain.max == 9)
    assert(domain.size == 4)
    assert(containsAll(domain))
    context.pop()
    assert(domain.min == 5)
    assert(domain.max == 10)
    assert(domain.size == 6)
    assert(containsAll(domain))
    context.pop()
    assert(domain.min == 5)
    assert(domain.max == 15)
    assert(domain.size == 11)
    assert(containsAll(domain))
  }
  
  test("Assign should make min equal to max") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    domain.assign(10)
    assert(domain.hasValue(10))
    assert(domain.min == 10)
    assert(domain.max == 10)
  }
  
  test("Assign should reduce the size to 1") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    domain.assign(10)
    assert(domain.size == 1)
  }
  
  test("Assign an out of bounds value should fail") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    assert(isInconsistent(domain.assign(20)))
    assert(domain.size == 0)
  }
  
  test("Random values should be contained in the domain") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 10, 30)
    for (seed <- 1 to 10) {
      val rand = new Random(seed)
      for (i <- 1 to 20) {
        val value = domain.randomValue(rand)
        assert(domain.hasValue(value), s"$value is not in the domain")
      }
    }
  }
  
  test("Random values should always be the assigned value when the size is 1") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 10, 10)
    for (seed <- 1 to 10) {
      val rand = new Random(seed)
      for (i <- 1 to 20) {
        val value = domain.randomValue(rand)
        assert(value == 10, s"$value is not the assigned value")
      }
    }
  }
  
  test("PrevValue of a greater than max value should be max") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    assert(domain.prevValue(20) == 15)
  } 
  
  test("PrevValue of a value in the domain should be that value") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    (5 to 15).foreach(v => assert(domain.prevValue(v) == v))
  }
  
  test("NextValue of lesser than min should be min") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    assert(domain.nextValue(0) == 5)
  }
  
  test("NextValue of a value in the domain should be that value") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    (5 to 15).foreach(v => assert(domain.nextValue(v) == v))
  }

  test("Iterator should iterate on all the values") {
    val context = new ReversibleContextImpl()
    val domain = intervalDomain(context, 5, 15)
    val values1 = (5 to 15).toSet
    assert(domain.iterator.forall(domain.hasValue(_)))
    assert(domain.iterator.forall(values1.contains(_)))
    assert(domain.iterator.size == 11)
    val values2 = (7 to 10).toSet
    domain.updateMin(7)
    domain.updateMax(10)
    assert(domain.size == 4)    
    assert(domain.iterator.forall(domain.hasValue(_)))
    assert(domain.iterator.forall(values2.contains(_)))
    assert(domain.iterator.size == 4)
  }
}