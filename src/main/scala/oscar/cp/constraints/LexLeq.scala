package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

/**
 * Lexicographic Less or Equal (LexLeq) Constraint
 * DX TX refer to states name from the paper of M. Carlson & N Beldiceanu
 * and variable names as well.
 * @author Pierre Schaus pschaus@gmail.com
 */
class LexLeq(val x: Array[CPIntVar], val y: Array[CPIntVar]) extends Constraint(x(0).store, "LexLeq") {

  if (x.length != y.length) {
    throw new RuntimeException("LexLeq: x and y must have the same length")
  }

  private val q = new ReversibleInt(s, 0)
  private val r = new ReversibleInt(s, 0)
  private val sv = new ReversibleInt(s, 0)
  private val u = new ReversibleInt(s, 0)
  u.setValue(0)

  private var i: Int = 0
  private var posted: Boolean = false

  override def associatedVars(): Iterable[CPVar] = x.toSeq ++ y.toSeq

  override def setup(l: CPPropagStrength): Unit = {
    mySetup(l)
    posted = true
  }

  private def mySetup(l: CPPropagStrength): Unit = {
    i = 0
    q.setValue(0)
    r.setValue(0)
    sv.setValue(0)
    state1()
  }

  private def setupFrom(p: Int): Unit = {
    if (posted) return
    for (idx <- p until x.length) {
      if (!x(idx).isBound) {
        x(idx).callUpdateBoundsIdxWhenBoundsChange(this, idx)
      }
      if (!y(idx).isBound) {
        y(idx).callUpdateBoundsIdxWhenBoundsChange(this, idx)
      }
    }
  }

  override def updateBoundsIdx(var_ : CPIntVar, idx: Int): Unit = {
    i = idx
    if (i == q.getValue()) state1()
    else if (i == r.getValue()) state2()
    else if (u.getValue() == 3 && (i == sv.getValue() || (i < sv.getValue() && x(i).getMax != y(i).getMin))) state3()
    else if (u.getValue() == 4 && (i == sv.getValue() || (i < sv.getValue() && x(i).getMin != y(i).getMax))) state4()
  }

  private def state1(): Unit = {
    while (i < x.length && x(i).getMin == y(i).getMax) {
      val value = x(i).getMin
      x(i).assign(value)
      y(i).assign(value)
      i += 1
      q.setValue(i)
    }
    if (i >= x.length || x(i).getMax < y(i).getMin) {
      if (posted) deactivate()
      return
    }
    x(i).updateMax(y(i).getMax)
    y(i).updateMin(x(i).getMin)

    i = if (i + 1 > r.getValue()) i + 1 else r.getValue()
    r.setValue(i)

    state2()
  }

  private def state2(): Unit = {
    while (i < x.length && x(i).isBound && y(i).isBound && x(i).getMin == y(i).getMin) {
      i += 1
      r.setValue(i)
    }

    if (i >= x.length || x(i).getMax < y(i).getMin) {
      if (posted) deactivate()
      s.post(new LeEq(x(q.getValue()), y(q.getValue())))
      return
    }

    if (x(i).getMin > y(i).getMax) {
      if (posted) deactivate()
      s.post(new Le(x(q.getValue()), y(q.getValue())))
      return
    }

    if (x(i).getMax == y(i).getMin && x(i).getMin < y(i).getMax) {
      i = if (i + 1 > sv.getValue()) i + 1 else sv.getValue()
      sv.setValue(i)
      state3()
      return
    }

    if (x(i).getMin == y(i).getMax && x(i).getMax > y(i).getMin) {
      i = if (i + 1 > sv.getValue()) i + 1 else sv.getValue()
      sv.setValue(i)
      state4()
      return
    }
    setupFrom(q.getValue())
    u.setValue(2)
  }

  private def state3(): Unit = {
    while (i < x.length && x(i).getMax == y(i).getMin) {
      i += 1
    }
    sv.setValue(i)
    if (i >= x.length || x(i).getMax < y(i).getMin) {
      if (posted) deactivate()
      s.post(new LeEq(x(q.getValue()), y(q.getValue())))
      return
    }
    setupFrom(q.getValue())
    u.setValue(3)
  }

  private def state4(): Unit = {
    while (i < x.length && x(i).getMin == y(i).getMax) {
      i += 1
    }
    sv.setValue(i)
    if (i < x.length && x(i).getMin > y(i).getMax) {
      if (posted) deactivate()
      s.post(new Le(x(q.getValue()), y(q.getValue())))
      return
    }
    setupFrom(q.getValue())
    u.setValue(4)
  }
}
