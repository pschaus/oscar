package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import oscar.cp.util.ArrayUtils

class BinPackingFlow(val x: Array[CPIntVar], val sizes: Array[Int], val l: Array[CPIntVar], val c: Array[CPIntVar]) extends Constraint(x(0).store, "BinPackingFlow") {

  private val l_t = new Array[ReversibleInt](sizes.length)
  private val c_t = new Array[ReversibleInt](sizes.length)
  private val perm = ArrayUtils.sortPerm(sizes)

  for (i <- l_t.indices) {
    l_t(i) = new ReversibleInt(s, 0)
    c_t(i) = new ReversibleInt(s, 0)
  }

  override def associatedVars(): Iterable[CPVar] = x.toSeq ++ l.toSeq ++ c.toSeq

  override def setup(strength: CPPropagStrength): Unit = {
    for (var_ <- x) {
      var_.updateMax(l.length - 1)
      var_.updateMin(0)
    }
    s.post(new GCCVarAC(x, 0, c), CPPropagStrength.Strong)
    for (j <- l.indices) {
      l(j).callPropagateWhenBoundsChange(this)
      c(j).callPropagateWhenBoundsChange(this)
    }
    for (i <- sizes.indices) {
      if (x(i).isBound) {
        val j = x(i).min
        l_t(j).setValue(l_t(j).getValue() + sizes(i))
        c_t(j).incr()
      } else {
        x(i).callValBindIdxWhenBind(this, i)
        x(i).callPropagateWhenBind(this)
      }
    }
    propagate()
  }

  override def valBindIdx(xVar: CPIntVar, idx: Int): Unit = {
    val j = xVar.min
    val size = sizes(idx)
    l_t(j).setValue(l_t(j).getValue() + size)
    c_t(j).incr()
  }

  override def propagate(): Unit = {
    for (j <- l.indices) {
      setCardinality(j)
    }
  }

  private def setCardinality(j: Int): Unit = {
    val minVal = l(j).getMin
    val maxVal = l(j).getMax

    var v = l_t(j).getValue()
    var i = x.length - 1
    var nbAdded = 0
    while (v < minVal && i >= 0) {
      if (!x(perm(i)).isBound && x(perm(i)).hasValue(j)) {
        v += sizes(perm(i))
        nbAdded += 1
      }
      i -= 1
    }
    if (v < minVal) {
      throw oscar.algo.Inconsistency
    }
    val nbMin = nbAdded + c_t(j).getValue()
    c(j).updateMin(nbMin)

    v = l_t(j).getValue()
    i = 0
    nbAdded = 0
    while (i < x.length && v + sizes(perm(i)) <= maxVal) {
      if (!x(perm(i)).isBound && x(perm(i)).hasValue(j)) {
        v += sizes(perm(i))
        nbAdded += 1
      }
      i += 1
    }
    val nbMax = nbAdded + c_t(j).getValue()
    c(j).updateMax(nbMax)
  }
}
