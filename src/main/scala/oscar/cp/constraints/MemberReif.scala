package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.SparseSet
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class MemberReif(val x: CPIntVar, val set: SparseSet, val b: CPBoolVar) extends Constraint(x.store, "MemberReif") {

  private var inter: ReversibleInt = _
  private var xsize: ReversibleInt = _

  override def associatedVars(): Iterable[CPVar] = List(x, b)

  override def setup(l: CPPropagStrength): Unit = {
    if (b.isBound) {
      valBind(b)
      return
    }
    if (x.isBound) {
      valBind(x)
      return
    }

    var interSize = 0
    for (v <- set.toArray) {
      if (x.hasValue(v)) interSize += 1
    }

    if (interSize == 0) {
      emptyIntersection()
      return
    }
    if (interSize >= x.getSize) {
      fullIntersection()
      return
    }
    inter = new ReversibleInt(s, interSize)
    xsize = new ReversibleInt(s, x.getSize)

    x.callValBindWhenBind(this)
    b.callValBindWhenBind(this)
    x.callValRemoveWhenValueIsRemoved(this)
  }

  override def valRemove(var_ : CPIntVar, `val`: Int): Unit = {
    xsize.decr()
    if (set.hasValue(`val`)) {
      inter.decr()
    }

    if (inter.getValue() == 0) {
      emptyIntersection()
    } else if (inter.getValue() == xsize.getValue()) {
      fullIntersection()
    }
  }

  override def valBind(var_ : CPIntVar): Unit = {
    assert(var_.isBound)
    if (var_ == x) {
      if (set.hasValue(x.min)) {
        b.assign(1)
      } else {
        b.assign(0)
      }
      deactivate()
    } else {
      assert(var_ == b)
      if (b.min == 1) {
        removeValues(false)
      } else {
        removeValues(true)
      }
    }
  }

  private def emptyIntersection(): Unit = {
    b.assign(0)
    deactivate()
  }

  private def fullIntersection(): Unit = {
    b.assign(1)
    deactivate()
  }

  private def removeValues(memberValue: Boolean): Unit = {
    assert(b.isBound)
    var `val` = x.min
    val maxVal = x.max
    while (`val` <= maxVal) {
      if (x.hasValue(`val`)) {
        if ((memberValue && set.hasValue(`val`)) || (!memberValue && !set.hasValue(`val`))) {
          x.removeValue(`val`)
        }
      }
      `val` += 1
    }
    deactivate()
  }
}
