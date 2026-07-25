package oscar.algo.reversible

import oscar.cp.core.CPStore
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestTrailQueue extends AnyFunSuite {

  test("testEquals") {
    val s = new CPStore()
    val a = new ReversibleQueue[Integer](s)
    val b = new ReversibleQueue[String](s)

    assert(a.getValue() == null)
    assert(b.getValue() == null)

    // a = null, b = null
    s.pushState()
    a.setValue(new Queue[Integer](a.getValue(), 1))
    a.setValue(new Queue[Integer](a.getValue(), 2))
    a.setValue(new Queue[Integer](a.getValue(), 3))
    b.setValue(new Queue[String](b.getValue(), "a"))
    b.setValue(new Queue[String](b.getValue(), "b"))
    b.setValue(new Queue[String](b.getValue(), "c"))

    // a = 3->2->1    b = c->b->a
    s.pushState()
    b.setValue(new Queue[String](b.getValue(), "d"))

    // a = 3->2->1    b = d->c->b->a
    s.pushState()
    a.setValue(new Queue[Integer](a.getValue(), 4))
    a.setValue(new Queue[Integer](a.getValue(), 5))

    // a = 5->4->3->2->1    b= d->c->b->a
    s.pushState()

    s.pop()
    assert(a.getValue().toString == "5->4->3->2->1")
    assert(b.getValue().toString == "d->c->b->a")

    s.pop()
    assert(a.getValue().toString == "3->2->1")
    assert(b.getValue().toString == "d->c->b->a")

    s.pop()
    assert(a.getValue().toString == "3->2->1")
    assert(b.getValue().toString == "c->b->a")

    s.pop()
    assert(a.getValue() == null)
    assert(b.getValue() == null)
  }

}
