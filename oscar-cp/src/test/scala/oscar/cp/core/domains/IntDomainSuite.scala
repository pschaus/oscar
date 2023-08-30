package oscar.cp.core.domains

import oscar.algo.reversible.{ReversibleContext, ReversibleContextImpl}
import oscar.cp.isInconsistent

/**
 *  Tests the sparse set implementation of a sparse domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
// Test the sparse set implementation of a sparse domain
class SparseSetDomainSuite extends IntDomainSuite {
  override def sparseDomain(context: ReversibleContext, minValue: Int, maxValue: Int): IntDomain = {
    new SparseSetDomain(context, minValue, maxValue)
  }
}

/**
 *  Test the bit vector implementation of a sparse domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class SingleBitVectorDomainSuite extends IntDomainSuite {
  override def sparseDomain(context: ReversibleContext, minValue: Int, maxValue: Int): IntDomain = {
    new SingleBitVectorDomain(context, minValue, maxValue)
  }
}

/**
 *  Generic class to test implementations of a sparse domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class IntDomainSuite extends IntervalDomainSuite {

  // Implement this method to test your implementation of sparse domain
  def sparseDomain(context: ReversibleContext, minValue: Int, maxValue: Int): IntDomain

  // Used to test interval functions
  override def intervalDomain(context: ReversibleContext, minValue: Int, maxValue: Int): IntervalDomain = {
    sparseDomain(context, minValue, maxValue)
  }

  test("Removed values should not be contained in the domain anymore") {
    val context = new ReversibleContextImpl()
    val domain = sparseDomain(context, 5, 10)
    domain.removeValue(5)
    assert(!domain.hasValue(5))
    domain.removeValue(7)
    assert(!domain.hasValue(7))
    domain.removeValue(8)
    assert(!domain.hasValue(8))
  }

  test("Remove a value should reduce the size") {
    val context = new ReversibleContextImpl()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    domain.removeValue(5)
    assert(domain.size == size - 1)
    domain.removeValue(5)
    assert(domain.size == size - 1)
    domain.removeValue(6)
    assert(domain.size == size - 2)
  }

  test("Remove a removed value should not impact the domain") {
    val context = new ReversibleContextImpl()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    domain.removeValue(4)
    assert(domain.size == size)
    domain.removeValue(11)
    assert(domain.size == size)

  }

  test("Remove the minimal value should change the minimum value") {
    val context = new ReversibleContextImpl()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    domain.removeValue(5)
    assert(domain.min == 6)
    domain.removeValue(6)
    domain.removeValue(7)
    assert(domain.min == 8)
    domain.removeValue(10)
    assert(domain.min == 8)
  }

  test("Remove all but one value should assign that value") {
    val context = new ReversibleContextImpl()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    domain.removeValue(5)
    assert(domain.hasValue(7))
    domain.removeValue(6)
    assert(domain.hasValue(7))
    domain.removeValue(9)
    assert(domain.hasValue(7))
    domain.removeValue(10)
    assert(domain.hasValue(7))
    domain.removeValue(8)
    assert(domain.hasValue(7))
    assert(domain.isBound)
  }

  test("Removed values should be restored when a backtrack occurs") {
    val context = new ReversibleContextImpl()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    context.pushState()
    domain.removeValue(5)
    domain.removeValue(6)
    context.pushState()
    domain.removeValue(9)
    context.pushState()
    domain.removeValue(8)
    assert(!domain.hasValue(5))
    assert(!domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(!domain.hasValue(8))
    assert(!domain.hasValue(9))
    assert(domain.hasValue(10))
    context.pop()
    assert(!domain.hasValue(5))
    assert(!domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(domain.hasValue(8))
    assert(!domain.hasValue(9))
    assert(domain.hasValue(10))
    context.pop()
    assert(!domain.hasValue(5))
    assert(!domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(domain.hasValue(8))
    assert(domain.hasValue(9))
    assert(domain.hasValue(10))
    context.pop()
    assert(domain.hasValue(5))
    assert(domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(domain.hasValue(8))
    assert(domain.hasValue(9))
    assert(domain.hasValue(10))
  }
  
  test("Remove the assigned value should fail") {
    val context = new ReversibleContextImpl()
    val domain = sparseDomain(context, 10, 10)
    assert(isInconsistent(domain.removeValue(10)))
    assert(domain.size == 0)
  }
  
  test("Iterator should iterate on all the values (sparse)") {
    val context = new ReversibleContextImpl()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = sparseDomain(context, 10, 25)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    assert(domain.iterator.size == 8)
    assert(domain.iterator.min == 10)
    assert(domain.iterator.max == 25)
    assert(domain.iterator.forall(values.contains))
  }
  
  test("UpdateMin should adjust the minimum value and the size (sparse)") {
    val context = new ReversibleContextImpl()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = sparseDomain(context, 10, 25)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    domain.updateMin(12)
    assert(domain.size == 6)
    assert(domain.min == 15)
  }
  
  test("UpdateMin should remove all values lesser than min (sparse)") {
    val context = new ReversibleContextImpl()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = sparseDomain(context, 10, 25)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    domain.updateMin(16)
    assert(!domain.hasValue(10))
    assert(!domain.hasValue(11))
    assert(!domain.hasValue(15))
  }
  
  test("UpdateMax should adjust the maximum value and the size (sparse)") {
    val context = new ReversibleContextImpl()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = sparseDomain(context, 10, 25)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    domain.updateMax(19)
    assert(domain.size == 5)
    assert(domain.max == 17)
  }
  
  test("UpdateMax should remove all values greater than max (sparse)") {
    val context = new ReversibleContextImpl()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = sparseDomain(context, 10, 25)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    domain.updateMax(17)
    assert(!domain.hasValue(20))
    assert(!domain.hasValue(21))
    assert(!domain.hasValue(25))
  }
  
  test("PrevValue of a value not in the domain should be the previous value in that domain") {
    val context = new ReversibleContextImpl()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = sparseDomain(context, 10, 25)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    assert(domain.prevValue(13) == 11)
    assert(domain.prevValue(19) == 17)
    assert(domain.prevValue(27) == 25)
  }
  
  test("NextValue of a value not in the domain should be the next value in that domain") {
    val context = new ReversibleContextImpl()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = sparseDomain(context, 10, 25)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    assert(domain.nextValue(0) == 10)
    assert(domain.nextValue(13) == 15)
    assert(domain.nextValue(18) == 20)
  }
}