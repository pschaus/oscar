package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import oscar.cp.util.NumberUtils

class MulCteRes(val x: CPIntVar, val y: CPIntVar, val c: Int) extends Constraint(x.store, "MulCteRes x*y=c") {

  override def associatedVars(): Iterable[CPVar] = List(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    if (x == y) {
      s.post(new Square(x, CPIntVar(s, c, c)))
      deactivate()
      return
    }

    if (c == 0 && x.hasValue(0) && y.hasValue(0)) {
      x.callPropagateWhenDomainChanges(this)
      y.callPropagateWhenDomainChanges(this)
    } else {
      x.callPropagateWhenBoundsChange(this)
      y.callPropagateWhenBoundsChange(this)
    }
    propagate()
  }

  override def propagate(): Unit = {
    if (c != 0) {
      x.removeValue(0)
      y.removeValue(0)
    }
    if (x.isBound) {
      s.post(new MulCte(y, x.min, CPIntVar(s, c, c)))
      deactivate()
    } else if (y.isBound) {
      s.post(new MulCte(x, y.min, CPIntVar(s, c, c)))
      deactivate()
    } else if (c == 0) {
      val xZero = x.hasValue(0)
      val yZero = y.hasValue(0)
      if (xZero || yZero) {
        if (xZero ^ yZero) {
          if (xZero) {
            x.assign(0)
          } else {
            y.assign(0)
          }
          deactivate()
        }
      } else {
        throw oscar.algo.Inconsistency
      }
    } else {
      propagateVar(x, y)
      propagateVar(y, x)
    }
  }

  private def propagateVar(w: CPIntVar, z: CPIntVar): Unit = {
    val a = w.getMin
    val b = w.getMax

    assert(c != 0)

    if (a > 0 || b < 0) {
      z.updateMin(NumberUtils.minCeilDiv(c, a, b))
      z.updateMax(NumberUtils.maxFloorDiv(c, a, b))
    } else if (a == 0) {
      val after0 = w.valueAfter(0)
      z.updateMin(NumberUtils.minCeilDiv(c, after0, b))
      z.updateMax(NumberUtils.maxFloorDiv(c, after0, b))
    } else if (b == 0) {
      val before0 = w.valueBefore(0)
      z.updateMin(NumberUtils.minCeilDiv(c, before0, a))
      z.updateMax(NumberUtils.maxFloorDiv(c, before0, a))
    } else {
      val before0 = w.valueBefore(0)
      val after0 = w.valueAfter(0)
      z.updateMin(NumberUtils.minCeilDiv(c, a, before0, after0, b))
      z.updateMax(NumberUtils.maxFloorDiv(c, a, before0, after0, b))
    }
  }
}
