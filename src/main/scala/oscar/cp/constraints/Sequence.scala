package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.SparseSet
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

/**
 * Sequence constraint specifying that in any sequence of length q in x, there 
 * are at least min and most max occurrences from a value in set values. 
 * @author Pierre Schaus pschaus@gmail.com
 */
class Sequence(val xinit: Array[CPIntVar], val values: SparseSet, val len: Int, val min: Int, val max: Int) extends Constraint(xinit(0).store, "Sequence") {
  
  assert(values.getSize() != 0)
  assert(len < xinit.length)
  assert(len > 0)
  assert(min <= max)
  assert(min >= 0)
  assert(max <= len)

  private var x: Array[CPBoolVar] = _
  private var cumulatedCounters: Array[CPIntVar] = _
  private var P: Array[Array[CPIntVar]] = _

  override def associatedVars(): Iterable[CPVar] = xinit.toSeq

  override def setup(cl: CPPropagStrength): Unit = {
    x = Array.tabulate(xinit.length)(_ => CPBoolVar()(s))
    
    for (i <- x.indices) {
      s.post(new MemberReif(xinit(i), values, x(i)))
    }
    
    cumulatedCounters = new Array[CPIntVar](x.length)
    cumulatedCounters(0) = x(0)
    for (i <- 1 until x.length) {
      cumulatedCounters(i) = oscar.cp.modeling.constraint.plus(cumulatedCounters(i - 1), x(i))
    }
    
    P = Array.ofDim[CPIntVar](x.length, x.length)
    for (i <- x.indices) {
      P(i)(i) = x(i)
      for (j <- i + 1 until math.min(x.length, i + len)) {
        if (i > 0) {
          P(i)(j) = oscar.cp.modeling.constraint.minus(cumulatedCounters(j), cumulatedCounters(i - 1))
        } else {
          P(i)(j) = cumulatedCounters(j)
        }
      }
    }
    
    for (i <- x.indices) {
      for (j <- i + 1 until math.min(x.length, i + len)) {
        for (m <- i until j) {
          s.post(new Sum(Array(P(i)(m), P(m + 1)(j)), P(i)(j)))
        }
      }

      if (i <= x.length - len) {
        s.post(new GrEq(P(i)(i + len - 1), min))
        s.post(new LeEq(P(i)(i + len - 1), max))
      }
    }
  }
}
