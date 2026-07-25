package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import oscar.cp.util.ArrayUtils
import oscar.cp.util.NumberUtils

class MulVar(val x: CPIntVar, val y: CPIntVar, val z: CPIntVar) extends Constraint(x.store, "Mul x*y=z") {

  override def associatedVars(): Iterable[CPVar] = List(x, y, z)

  override def setup(l: CPPropagStrength): Unit = {
    if (x == y) {
      s.post(new Square(x, z))
      deactivate()
      return
    }
    if (z.isBound) {
      if (z.min == 0 && x.hasValue(0) && y.hasValue(0)) {
        x.callPropagateWhenDomainChanges(this)
        y.callPropagateWhenDomainChanges(this)
      } else {
        x.callPropagateWhenBoundsChange(this)
        y.callPropagateWhenBoundsChange(this)
      }
    } else {
      x.callPropagateWhenBoundsChange(this)
      y.callPropagateWhenBoundsChange(this)
      z.callPropagateWhenBoundsChange(this)
    }
    
    propagate()
  }

  override def propagate(): Unit = {
    if (!z.hasValue(0)) {
      x.removeValue(0)
      y.removeValue(0)
    }

    if (x.isBound) {
      s.post(new MulCte(y, x.min, z))
      deactivate()
    } else if (y.isBound) {
      s.post(new MulCte(x, y.min, z))
      deactivate()
    } else if (z.isBound) {
      s.post(new MulCteRes(x, y, z.min))
      deactivate()
    } else {
      assert(!x.isBound && !y.isBound)
      
      z.updateMin(ArrayUtils.min(NumberUtils.safeMul(x.getMin, y.getMin),
                                 NumberUtils.safeMul(x.getMin, y.getMax),
                                 NumberUtils.safeMul(x.getMax, y.getMin),
                                 NumberUtils.safeMul(x.getMax, y.getMax)))

      z.updateMax(ArrayUtils.max(NumberUtils.safeMul(x.getMin, y.getMin),
                                 NumberUtils.safeMul(x.getMin, y.getMax),
                                 NumberUtils.safeMul(x.getMax, y.getMin),
                                 NumberUtils.safeMul(x.getMax, y.getMax)))
      
      propagateMul(x, y, z)
      propagateMul(y, x, z)
    }
  }

  private def propagDiv(w: CPIntVar, a: Int, b: Int, c: Int, d: Int): Unit = {
    val wmin = Math.min(NumberUtils.minCeilDiv(a, c, d), NumberUtils.minCeilDiv(b, c, d))
    w.updateMin(wmin)
    val wmax = Math.max(NumberUtils.maxFloorDiv(a, c, d), NumberUtils.maxFloorDiv(b, c, d))
    w.updateMax(wmax)
  }

  private def propagateMul(u: CPIntVar, w: CPIntVar, z: CPIntVar): Unit = {
    if (w.getMin > 0 || w.getMax < 0) {
      propagDiv(u, z.getMin, z.getMax, w.getMin, w.getMax)
      return
    }
    else if (z.getMin <= 0 && z.getMax >= 0) {
      // cannot filter u because we potentially have u * 0 = 0
    } else {
      assert(!z.isBound)
      val after0 = w.valueAfter(0)
      val before0 = w.valueBefore(0)
      if (w.getMin == 0) {
        propagDiv(u, z.getMin, z.getMax, after0, w.getMax)
      } else if (w.getMax == 0) {
        propagDiv(u, z.getMin, z.getMax, w.getMin, before0)
      } else {
        val umin = Math.min(NumberUtils.minCeilDiv(z.getMin, w.getMin, w.getMax, before0, after0),
                            NumberUtils.minCeilDiv(z.getMax, w.getMin, w.getMax, before0, after0))
        u.updateMin(umin)
        val umax = Math.max(NumberUtils.maxFloorDiv(z.getMin, w.getMin, w.getMax, before0, after0),
                            NumberUtils.maxFloorDiv(z.getMax, w.getMin, w.getMax, before0, after0))
        u.updateMax(umax)
      }
    }
  }
}
