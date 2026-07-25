package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class Square(val x: CPIntVar, val y: CPIntVar) extends Constraint(x.store, "Square") {

  override def associatedVars(): Iterable[CPVar] = List(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    y.updateMin(0)
    propagate()
    if (isActive) {
      if (!x.isBound) {
        x.callPropagateWhenBoundsChange(this)
      }
      if (!y.isBound) {
        y.callPropagateWhenBoundsChange(this)
      }
    }
  }

  override def propagate(): Unit = {
    val mx = x.getMin
    val Mx = x.getMax
    val mx2 = mx * mx
    val Mx2 = Mx * Mx

    if (mx >= 0) {
      y.updateMin(mx2)
      y.updateMax(Mx2)
    } else if (Mx <= 0) {
      y.updateMin(Mx2)
      y.updateMax(mx2)
    } else if (x.hasValue(0)) {
      y.updateMax(Math.max(mx2, Mx2))
    } else {
      val a = x.valueBefore(0)
      val b = x.valueAfter(0)
      val a2 = a * a
      val b2 = b * b
      y.updateMin(Math.min(a2, b2))
      y.updateMax(Math.max(a2, b2))
    }

    val my = y.getMin
    val My = y.getMax
    val my2 = my * my
    val My2 = My * My

    val rootm = if (Mx <= 0) Math.ceil(Math.sqrt(my)).toInt else Math.sqrt(my).toInt
    val rootM = Math.sqrt(My).toInt

    if (mx >= 0) {
      x.updateMin(rootm)
      x.updateMax(rootM)
    } else if (Mx <= 0) {
      x.updateMax(-rootm)
      x.updateMin(-rootM)
    } else {
      x.updateMin(-rootM)
      x.updateMax(rootM)
    }
  }
}
