package oscar.cp.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class BinaryKnapsackWithCardinality(b: Array[CPBoolVar], weights: Array[Int], val c: CPIntVar, val n: Int) extends Constraint(b(0).store, "BinaryKnapsackWithCardinality") {
  require(b.length == weights.length)
  
  private val perm = b.indices.sortBy(i => -weights(i)).toArray
  val w = perm.map(weights)
  val x = perm.map(b)
  
  private var packed: ReversibleInt = _
  private var nPacked: ReversibleInt = _

  override def associatedVars(): Iterable[CPVar] = x.toSeq :+ c

  override def setup(l: CPPropagStrength): Unit = {
    packed = new ReversibleInt(s, 0)
    nPacked = new ReversibleInt(s, 0)
    for (i <- x.indices) {
      if (x(i).isBound) {
        packed.setValue(packed.getValue() + w(i))
        nPacked.incr()
      } else {
        x(i).callValBindIdxWhenBind(this, i)
        x(i).callPropagateWhenBind(this)
      }
    }
  }

  override def valBindIdx(var_ : CPIntVar, idx: Int): Unit = {
    if (var_.getMin == 1) {
      nPacked.incr()
      packed.setValue(packed.getValue() + w(idx))
    }
  }

  override def propagate(): Unit = {
    var curn = nPacked.getValue()
    var curw = packed.getValue()
    var i = 0
    while (i < x.length && curn < n) {
      if (!x(i).isBound) {
        curw += w(i)
        curn += 1
      }
      i += 1
    }
    c.updateMax(curw)

    curn = nPacked.getValue()
    curw = packed.getValue()
    i = x.length - 1
    while (i >= 0 && curn < n) {
      if (!x(i).isBound) {
        curw += w(i)
        curn += 1
      }
      i -= 1
    }
    c.updateMin(curw)
  }
}
