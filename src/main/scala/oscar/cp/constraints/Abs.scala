package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class Abs(val x: CPIntVar, val y: CPIntVar) extends Constraint(x.store, "Abs") {

  override def associatedVars(): Iterable[CPVar] = List(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    y.updateMin(0)
    propagate()
    if (!x.isBound) {
      x.callPropagateWhenBoundsChange(this)
      x.callValBindWhenBind(this)
    }
    if (!y.isBound) {
      y.callPropagateWhenBoundsChange(this)
      y.callValBindWhenBind(this)
    }
  }

  override def propagate(): Unit = {
    if (x.getMin >= 0) {
      y.updateMin(x.getMin)
      y.updateMax(x.getMax)
      x.updateMin(y.getMin)
      x.updateMax(y.getMax)
    } else if (x.getMax <= 0) {
      y.updateMin(-x.getMax)
      y.updateMax(-x.getMin)
      x.updateMin(-y.getMax)
      x.updateMax(-y.getMin)
    } else {
      val maxabsy = math.max(math.abs(x.getMax), math.abs(x.getMin))
      y.updateMax(maxabsy)
      x.updateMax(y.getMax)
      x.updateMin(-y.getMax)
    }
  }

  override def valBind(var_ : CPIntVar): Unit = {
    if (x.isBound) {
      y.assign(math.abs(x.min))
      deactivate()
    } else {
      if (!x.hasValue(-y.min)) {
        x.assign(y.min)
      } else if (!x.hasValue(y.min)) {
        x.assign(-y.min)
      } else {
        var v = x.getMin
        val maxVal = x.getMax
        while (v <= maxVal) {
          if (v != y.min && v != -y.min) {
            x.removeValue(v)
          }
          v += 1
        }
      }
      deactivate()
    }
  }
}
