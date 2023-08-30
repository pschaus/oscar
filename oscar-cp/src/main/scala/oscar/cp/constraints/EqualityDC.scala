package oscar.cp.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore

final class EqualityDC(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "EqualityDC") {

  override def associatedVars(): Iterable[CPVar] = Array(x, y)

  private[this] val values = new Array[Int](Math.max(x.size, y.size))

  override def setup(l: CPPropagStrength): Unit = {
    if(!init()) { //If we don't end when doing the init
      val propagatorX = x.callOnChanges(s => removeValues(x, y, s))
      propagatorX.priority = CPStore.MaxPriorityL2
      val propagatorY = y.callOnChanges(s => removeValues(y, x, s))
      propagatorY.priority = CPStore.MaxPriorityL2
    }
  }

  /**
   * @return true if the constraint "suceeded" (should not do more filtering)
   */
  @inline private def init(): Boolean = {
    if (x.isBound) {
      y.assign(x.min)
      true
    }
    else if (y.isBound) {
      x.assign(y.min)
      true
    }
    else {
      var i = x.fillArray(values)
      while (i > 0) {
        i -= 1
        val value = values(i)
        if (!y.hasValue(value))
          x.removeValue(value)
      }
      i = y.fillArray(values)
      while (i > 0) {
        i -= 1
        val value = values(i)
        if (!x.hasValue(value))
          y.removeValue(value)
      }
      false
    }
  }

  @inline private def removeValues(from: CPIntVar, to: CPIntVar, delta: DeltaIntVar): Boolean = {
    if (from.isBound) {
      to.assign(from.min)
      true
    } else {
      val nValues = delta.fillArray(values)
      to.removeValues(values, nValues)
      false
    }
  }
}