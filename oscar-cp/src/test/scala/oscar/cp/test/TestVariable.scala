package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp.core.CPStore
import oscar.cp._
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.CPPropagStrength

class TestVariable extends TestSuite {

  trait StoreAndVariables {
    val store = new CPStore()
    val a = CPIntVar(1, 6)(store)
    val b = CPIntVar(0, 31)(store)
    val c = CPIntVar(1, 10)(store)
    val d = CPIntVar(1 to 5)(store)
  }

  test("test a") {
    new StoreAndVariables {
      a.removeValue(2)
      a.removeValue(4)
      store.pushState()
      a.removeValue(2)
      a.removeValue(10)
      a.removeValue(6)
      store.pushState()
      a.removeValue(-2)
      a.removeValue(4)
      a.removeValue(1)
      a.removeValue(4)
      store.pop()
      a.removeValue(1)
      a.removeValue(0)
      store.pop()
      //[1, 3, 5, 6] card:4 min:1 max:6
      assert(a.min == 1)
      assert(a.max == 6)
      assert(a.size == 4)
      assert(a.hasValue(1))
      assert(a.hasValue(3))
      assert(a.hasValue(5))
      assert(a.hasValue(6))
      assert(!a.hasValue(2))
    }
  }

  test("test b") {
    new StoreAndVariables {
      b.assign(0)
      assert(b.isBound && b.min == 0)
    }
  }

  test("test c") {
    new StoreAndVariables {
      c.removeValue(5)
      c.updateMax(7)
      //1..4,6..7
      var values = List(1, 2, 3, 4, 6, 7)
      for (v <- c.min until c.max) {
        if (c.hasValue(v)) {
          assert(v == values.head)
          values = values.tail
        }
      }
    }
  }

  test("test d") {
    val cp = new CPStore()
    val x = CPIntVar(1, 6)(cp)

    assert(x.valueAfter(5) == 6)
    assert(x.valueAfter(-10) == 1)
    val v: Int = x.valueAfter(6)
    assert(v == 6)

    val y = CPIntVar(-100, 100)(cp)
    y.removeValue(0)

    assert(y.valueAfter(-1) == 1)
    assert(y.valueAfter(0) == 1)
    assert(y.valueBefore(-1) == -2)
    assert(y.valueBefore(1) == -1)
    assert(y.valueBefore(0) == -1)
    assert(y.valueBefore(1000) == 100)

    y.removeValue(30)
    y.removeValue(31)
    y.removeValue(32)
    y.removeValue(33)

    assert(y.valueBefore(31) == 29)
    assert(y.valueBefore(34) == 29)
    assert(y.valueAfter(31) == 34)
    assert(y.valueAfter(34) == 35)
  }

  // Non determinist test !
  test("test e") {
    val cp = new CPStore()
    val freq = Array.fill(4)(0)
    val x = CPIntVar(Set(0, 1, 2, 3))(cp)
    for (i <- 0 until 200) {
      freq(x.randomValue) += 1
    }
    for (i <- 0 until 4) {
      assert(freq(i) > 0)
    }
    //println(freq.mkString(", "))
  }

  test("test f") {
    val cp = new CPStore()
    val x = CPIntVar(Set(1, 5, 9, 10))(cp)
    val y = CPIntVar(Set(5, 9, 11))(cp)
    val z = CPIntVar(Set(6, 7, 11))(cp)
    val w = CPIntVar(14)(cp)

    assert(x.intersectionSize(y) == 2)
    assert(y.intersectionSize(x) == 2)

    assert(z.intersectionSize(y) == 1)
    assert(y.intersectionSize(z) == 1)

    assert(w.intersectionSize(x) == 0)
    assert(x.intersectionSize(w) == 0)
  }
  
  test("test g: toArray and fillArray") {
    new StoreAndVariables {
      d.removeValue(3)
      val values  = d.toArray
      assert(values.toSet == Set(1,2,4,5))
      val empty = Array.ofDim[Int](10)
      val s = d.fillArray(empty)
      assert(s == 4)
      assert(d.take(4).toSet == Set(1,2,4,5))
    }
  }

  test("test AC3 bind sparse") {
    var propagCalled = 0
    class FooValBind(val x: CPIntVar) extends Constraint(x.store, "Foo") {
      override def setup(l: oscar.cp.core.CPPropagStrength): Unit = {
        x.callPropagateWhenBind(this)
        propagate()
      }
      override def propagate(): Unit = {
        propagCalled += 1
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }
    val cp = CPSolver()
    val x = CPIntVar(Set(1,4,6))(cp)
    cp.add(new FooValBind(x))
    
    cp.pushState()
    
    
    propagCalled = 0
    cp.add(x !== 1)
    assert(propagCalled == 0)
    cp.add(x !== 4)
    assert(propagCalled == 1)
    assert(x.isBound)
    
    
    cp.pop()
    cp.pushState()
    
    propagCalled = 0
    cp.add(x <=3)
    assert(propagCalled == 1)
    assert(x.isBound)

  }
  
  test("test AC3 bind continuous") {
    var propagCalled = 0
    class FooValBind(val x: CPIntVar) extends Constraint(x.store, "Foo") {
      override def setup(l: oscar.cp.core.CPPropagStrength): Unit = {
        x.callPropagateWhenBind(this)
        return propagate()
      }
      override def propagate(): Unit = {
        propagCalled += 1
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }
    val cp = CPSolver()
    val x = CPIntVar(1 to 6)(cp)
    cp.add(new FooValBind(x))
    
    cp.pushState()
    
    
    propagCalled = 0
    cp.add(x < 5 )
    assert(propagCalled == 0)
    cp.add(x <= 1)
    assert(propagCalled == 1)
    assert(x.isBound)
    
    
    cp.pop()
    cp.pushState()
    
    propagCalled = 0
    cp.add(x >= 6)
    assert(propagCalled == 1)
    assert(x.isBound)

  }  
  
  
  
 
}
