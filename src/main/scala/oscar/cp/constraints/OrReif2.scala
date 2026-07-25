package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleBoolean
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class OrReif2(val x: Array[CPBoolVar], val y: CPBoolVar) extends Constraint(x(0).store, "Or") {

  private var nbBound: ReversibleInt = _
  private var ytrue: ReversibleBoolean = _

  override def associatedVars(): Iterable[CPVar] = x.toSeq :+ y

  override def setup(l: CPPropagStrength): Unit = {
    if (x.length == 2) {
      s.post(new BinaryOr(x(0), x(1), y))
      return
    }

    nbBound = new ReversibleInt(s, 0)
    ytrue = new ReversibleBoolean(s, false)
    for (i <- x.indices) {
      if (x(i).isTrue) {
        y.assign(1)
        return
      }
    }

    for (i <- x.indices) {
      if (!x(i).isBound) {
        x(i).callValBindIdxWhenBind(this, i)
      } else {
        assert(x(i).isFalse)
        nbBound.incr()
      }
    }

    if (!y.isBound) {
      if (nbBound.getValue() == x.length) {
        y.assign(0)
      }
      y.callValBindWhenBind(this)
    } else {
      if (y.min == 0) {
        for (aX <- x) {
          aX.assign(0)
        }
        this.deactivate()
      } else {
        ytrue.setValue(true)
        if (nbBound.getValue() == x.length - 1) {
          var found = false
          for (i <- x.indices if !found) {
            if (!x(i).isBound) {
              x(i).assign(1)
              this.deactivate()
              found = true
            }
          }
        }
      }
    }
  }

  override def valBindIdx(var_ : CPIntVar, idx: Int): Unit = {
    if (var_.min == 1) {
      y.assign(1)
      this.deactivate()
    } else {
      nbBound.incr()
      if (nbBound.getValue() == x.length) {
        y.assign(0)
      } else if (nbBound.getValue() == x.length - 1 && ytrue.getValue()) {
        var found = false
        for (i <- x.indices if !found) {
          if (!x(i).isBound) {
            x(i).assign(1)
            this.deactivate()
            found = true
          }
        }
      }
    }
  }

  override def valBind(yvar: CPIntVar): Unit = {
    if (yvar.min == 0) {
      for (i <- x.indices) {
        x(i).assign(0)
      }
      this.deactivate()
    } else {
      ytrue.setValue(true)
      if (nbBound.getValue() == x.length - 1) {
        var found = false
        for (i <- x.indices if !found) {
          if (!x(i).isBound) {
            x(i).assign(1)
            this.deactivate()
            found = true
          }
        }
      }
    }
  }
}
