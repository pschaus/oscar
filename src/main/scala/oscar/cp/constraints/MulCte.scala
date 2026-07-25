package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import oscar.cp.util.NumberUtils

class MulCte(val x: CPIntVar, val c: Int, val z: CPIntVar) extends Constraint(x.store, "MulCte") {

  override def associatedVars(): Iterable[CPVar] = List(x, z)

  override def setup(l: CPPropagStrength): Unit = {
    propagate()
    if (isActive) {
      x.callPropagateWhenBoundsChange(this)
      z.callPropagateWhenBoundsChange(this)
    }
  }

  override def propagate(): Unit = {
    if (x.isBound) {
      z.assign(NumberUtils.safeMul(c, x.min))
      deactivate()
    } else if (c == 0) {
      z.assign(0)
      deactivate()
    } else {
      z.updateMin(Math.min(NumberUtils.safeMul(c, x.getMin), NumberUtils.safeMul(c, x.getMax)))
      z.updateMax(Math.max(NumberUtils.safeMul(c, x.getMin), NumberUtils.safeMul(c, x.getMax)))
      x.updateMin(Math.min(NumberUtils.ceilDiv(z.getMin, c), NumberUtils.ceilDiv(z.getMax, c)))
      x.updateMax(Math.max(NumberUtils.floorDiv(z.getMin, c), NumberUtils.floorDiv(z.getMax, c)))
    }
  }
}
