package oscar.cp.constraints.binpacking

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.constraints.GCCVarAC
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import oscar.cp.util.ArrayUtils

/**
 * Redundant Bin-Packing + Card Constraint
 * @author pschaus@gmail.com
 */
class SimpleBinPackingCard(val x: Array[CPIntVar], val sizes: Array[Int], val l: Array[CPIntVar], val c: Array[CPIntVar])
    extends Constraint(x(0).store, "BinPackingFlow") {

  private val perm = ArrayUtils.sortPerm(sizes)
  private val l_t = Array.fill(l.length)(new ReversibleInt(s, 0))
  private val c_t = Array.fill(l.length)(new ReversibleInt(s, 0))

  override def associatedVars(): Iterable[CPVar] = x ++ l ++ c

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

  private def printDebug(): Unit = {
    for (i <- x.indices) {
      println(s"x$i=${x(i)} w$i=${sizes(i)}")
    }
    for (j <- l.indices) {
      println(s"load$j=${l(j)} card:${c(j)} packedload=${l_t(j)} packedcard=${c_t(j)}")
    }
  }

  override def propagate(): Unit = {
    for (j <- l.indices) {
      setCardinality(j)
    }
  }

  /**
   * Adapt the cardinality of bin j
   * @param j is the bin index
   * @return Failure if fail detected when adapting cards, or Suspend otherwise
   */
  private def setCardinality(j: Int): Unit = {
    val minVal = l(j).min
    val maxVal = l(j).max
    // how many items do I need at least to reach minVal ?
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
      throw Inconsistency.get // not possible to reach the minimum level
    }
    val nbMin = nbAdded + c_t(j).getValue()
    c(j).updateMin(nbMin)
    
    // how many items can I use at most before reaching maxVal ?
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
