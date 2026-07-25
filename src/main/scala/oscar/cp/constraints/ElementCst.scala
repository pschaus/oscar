package oscar.cp.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import scala.collection.mutable

class ElementCst(val y: Array[Int], val x: CPIntVar, val z: CPIntVar) extends Constraint(x.store, "ElementCst") {
  
  private val sortedPerm: Array[Integer] = Array.tabulate(y.length)(i => Integer.valueOf(i))
  java.util.Arrays.sort(sortedPerm, new java.util.Comparator[Integer] {
    override def compare(i1: Integer, i2: Integer): Int = y(i1) - y(i2)
  })

  private val minIndSupp = new ReversibleInt(s, 0)
  minIndSupp.setValue(0)
  private val maxIndSupp = new ReversibleInt(s, 0)
  maxIndSupp.setValue(y.length - 1)

  private var counters: mutable.HashMap[Int, ReversibleInt] = _

  override def associatedVars(): Iterable[CPVar] = List(x, z)

  override def setup(l: CPPropagStrength): Unit = {
    x.updateMin(0)
    x.updateMax(y.length - 1)
    
    propagate()
    if (l == CPPropagStrength.Strong) {
      initCounters()
      x.callValRemoveWhenValueIsRemoved(this)
      z.callValRemoveWhenValueIsRemoved(this)
    }
    z.callPropagateWhenBoundsChange(this)
    x.callPropagateWhenDomainChanges(this)
    x.callValBindWhenBind(this)
  }

  private def initCounters(): Unit = {
    counters = mutable.HashMap.empty[Int, ReversibleInt]
    for (i <- y.indices) {
      counters.get(y(i)) match {
        case None => counters.put(y(i), new ReversibleInt(s, 1))
        case Some(counter) => counter.incr()
      }
    }
  }

  override def valRemove(var_ : CPIntVar, `val`: Int): Unit = {
    if (var_ == z) {
      for (i <- y.indices) {
        if (y(i) == `val`) {
          x.removeValue(i)
        }
      }
    } else {
      assert(var_ == x)
      val counter = counters(y(`val`))
      counter.decr()
      if (counter.getValue() == 0) {
        z.removeValue(y(`val`))
      }
    }
  }

  override def propagate(): Unit = {
    var i = minIndSupp.getValue()
    while (i < y.length && (y(sortedPerm(i)) < z.getMin || !x.hasValue(sortedPerm(i)))) {
      x.removeValue(sortedPerm(i))
      i += 1
    }
    minIndSupp.setValue(i)
    z.updateMin(y(sortedPerm(i)))
    
    i = maxIndSupp.getValue()
    while (i >= 0 && (y(sortedPerm(i)) > z.getMax || !x.hasValue(sortedPerm(i)))) {
      x.removeValue(sortedPerm(i))
      i -= 1
    }
    maxIndSupp.setValue(i)
    z.updateMax(y(sortedPerm(i)))
  }

  override def valBind(xvar: CPIntVar): Unit = {
    if (xvar == x) {
      z.assign(y(xvar.min))
      deactivate()
    }
  }
}
