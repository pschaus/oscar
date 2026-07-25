package oscar.cp.constraints

import oscar.algo.reversible.ReversibleSparseSetJava
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class Modulo(val x: CPIntVar, val v: Int, val y: CPIntVar) extends Constraint(x.store, "Modulo") {
  assert(v > 0)
  if (v <= 0) throw new RuntimeException("v must be > 0")

  private var supportSet: Array[ReversibleSparseSetJava] = _

  override def associatedVars(): Iterable[CPVar] = (x :: y :: Nil)

  override def setup(l: CPPropagStrength): Unit = {
    y.updateMin(-v + 1)
    y.updateMax(v - 1)

    supportSet = new Array[ReversibleSparseSetJava](2 * v - 1)
    var i = -v + 1
    while (i < v) {
      var `val` = x.min
      val maxVal = x.max
      while (`val` <= maxVal) {
        if (x.hasValue(`val`)) {
          if ((`val` % v) == i) {
            if (supportSet(i + v - 1) == null) {
              supportSet(i + v - 1) = new ReversibleSparseSetJava(s, x.getMin, x.getMax, true)
            }
            supportSet(i + v - 1).insert(`val`)
          }
        }
        `val` += 1
      }
      if (supportSet(i + v - 1) == null || supportSet(i + v - 1).isEmpty) {
        y.removeValue(i)
      }
      i += 1
    }

    var yVal = y.getMin
    val yMax = y.getMax
    while (yVal <= yMax) {
      if (!y.hasValue(yVal)) {
        valRemovedFromY(yVal)
      }
      yVal += 1
    }

    if (!x.isBound) x.callValRemoveWhenValueIsRemoved(this)
    if (!y.isBound) y.callValRemoveWhenValueIsRemoved(this)
  }

  private def valRemovedFromY(`val`: Int): Unit = {
    assert(`val` > -v && `val` < v)
    if (supportSet(`val` + v - 1) != null && !supportSet(`val` + v - 1).isEmpty) {
      for (j <- supportSet(`val` + v - 1).toArray) {
        x.removeValue(j)
      }
    }
  }

  override def valRemove(var_ : CPIntVar, `val`: Int): Unit = {
    if (var_ eq x) {
      val i = `val` % v
      if (supportSet(i + v - 1) != null) {
        supportSet(i + v - 1).removeValue(`val`)
        if (supportSet(i + v - 1).isEmpty) {
          y.removeValue(i)
        }
      }
    } else {
      assert(`val` > -v && `val` < v)
      valRemovedFromY(`val`)
    }
  }
}
