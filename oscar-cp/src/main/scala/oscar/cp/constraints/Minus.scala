package oscar.cp.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}

/**
 * Minus Constraint
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class Minus(x: CPIntVar, y: CPIntVar, z: CPIntVar) extends Constraint(x.store, "Minus") {

  override def associatedVars(): Iterable[CPVar] = Array(x, y, z)

  final override def setup(l: CPPropagStrength): Unit = {

    priorityL2 = CPStore.MaxPriorityL2


    propagate()
    if(isActive) {
      x.callPropagateWhenBoundsChange(this)
      y.callPropagateWhenBoundsChange(this)
      z.callPropagateWhenBoundsChange(this)
    }
  }

  final override def propagate(): Unit = {
    // Cache
    val xMin = x.min
    val xMax = x.max
    val yMin = y.min
    val yMax = y.max   
    // Prune z = x - y
    z.updateMax(xMax - yMin)
    z.updateMin(xMin - yMax)
    // Cache
    val zMin = z.min
    val zMax = z.max
    // Prune y = x - z
    y.updateMax(xMax - zMin)
    y.updateMin(xMin - zMax)
    // Prune x = z + y
    x.updateMax(zMax + yMax)
    x.updateMin(zMin + yMin)
  }
}
