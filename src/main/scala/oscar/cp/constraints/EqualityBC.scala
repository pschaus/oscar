package oscar.cp.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore

final class EqualityBC(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "EqualityBC") {

  override def associatedVars(): Iterable[CPVar] = Array(x, y)

  priorityL2 = CPStore.MaxPriorityL2
  idempotent = true
  
  final override def setup(l: CPPropagStrength): Unit = {
    val outcome = propagate()
    if(isActive) {
      x.callPropagateWhenBoundsChange(this)
      y.callPropagateWhenBoundsChange(this)
    }
  }

  final override def propagate(): Unit = {
    var yMin = y.min
    var yMax = y.max
    if (yMin == yMax) assignTo(x, yMin)
    else {
      var xMin = x.min
      var xMax = x.max
      if (xMin == xMax) assignTo(y, xMin)
      else {
        // Update min
        while (yMin != xMin) {
          y.updateMin(xMin)
          yMin = y.min
          if (yMin != xMin) {
            x.updateMin(yMin)
            xMin = x.min
          }
        }
        // Update max
        while (yMax != xMax) {
          y.updateMax(xMax)
          yMax = y.max
          if (yMax != xMax) {
            x.updateMax(yMax)
            xMax = x.max
          }
        }
      }
    }
  }

  @inline private def assignTo(intVar: CPIntVar, value: Int): Unit = {
    intVar.assign(value)
    deactivate()
  }
}