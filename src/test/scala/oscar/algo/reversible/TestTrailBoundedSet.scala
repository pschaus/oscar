package oscar.algo.reversible

import oscar.cp.core.CPStore
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestTrailBoundedSet extends AnyFunSuite {

  test("testEquals") {
    val s = new CPStore()
    
    // a = null, b = null
    s.pushState()
    
    val set = new ReversibleBoundedSet(s, 10)
    
    s.pushState()
    
    set.insert(5)
    set.insert(10)
    set.insert(3)
    set.remove(10)
    
    s.pushState()
    
    set.insert(6)
    
    assert(set.getSize() == 3)
    
    set.remove(5)
    
    s.pop()
    
    assert(set.contains(3))
    assert(set.contains(5))
    assert(set.getSize() == 2)
  }

}
