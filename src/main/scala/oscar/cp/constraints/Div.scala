package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class Div(val x: CPIntVar, val v: Int, val y: CPIntVar) extends Constraint(x.store, "Div") {
  assert(v != 0)
  if (v == 0) throw new RuntimeException("v must be > 0")

  override def associatedVars(): Iterable[CPVar] = List(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    if (v < 0) {
      y.updateMin(x.getMax / v)
      y.updateMax(x.getMin / v)

      x.updateMin(y.getMax * v)
      x.updateMax(y.getMin * v)
    } else {
      y.updateMin(x.getMin / v)
      y.updateMax(x.getMax / v)

      x.updateMin(y.getMin * v)
      x.updateMax(y.getMax * v)
    }

    if (!x.isBound) x.callValBindWhenBind(this)
    if (!y.isBound) y.callValRemoveWhenValueIsRemoved(this)
    if (!x.isBound) x.callValRemoveWhenValueIsRemoved(this)
  }

  override def valRemove(var_ : CPIntVar, `val`: Int): Unit = {
    if (var_ eq y) {
      val sgn = if (v > 0) 1 else -1
      var i = 0
      while (i < v) {
        x.removeValue(`val` * v + i * sgn)
        i += 1
      }
    } else {
      if (v > 0) {
        if (`val` > x.getMax || `val` < x.getMin) {
          y.updateMin(x.getMin / v)
          y.updateMax(x.getMax / v)
        }
      } else {
        if (`val` > x.getMax || `val` < x.getMin) {
          y.updateMin(x.getMax / v)
          y.updateMax(x.getMin / v)
        }
      }
    }
  }

  override def valBind(var_ : CPIntVar): Unit = {
    y.assign(x.getMin / v)
  }
}
