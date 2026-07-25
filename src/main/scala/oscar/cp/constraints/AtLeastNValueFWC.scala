package oscar.cp.constraints

import oscar.algo.reversible.ReversibleBoolean
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class AtLeastNValueFWC(val x: Array[CPIntVar], val nValueVar: CPIntVar) extends Constraint(x(0).store, "AtLeastNValueFWC") {

  private var isValueUsed: Array[ReversibleBoolean] = _
  private var nbValueUsed: ReversibleInt = _
  private var nbBound: ReversibleInt = _

  private var min: Int = 0
  private var max: Int = 0
  private var valSize: Int = 0

  override def associatedVars(): Iterable[CPVar] = x.toSeq :+ nValueVar

  override def setup(l: CPPropagStrength): Unit = {
    findValueRange()

    isValueUsed = new Array[ReversibleBoolean](valSize)
    for (v <- 0 until valSize) {
      isValueUsed(v) = new ReversibleBoolean(s)
      isValueUsed(v).setValue(false)
    }
    
    nbValueUsed = new ReversibleInt(s, 0)
    nbValueUsed.setValue(0)
    nbBound = new ReversibleInt(s, 0)
    nbBound.setValue(0)

    for (k <- x.indices) {
      if (x(k).isBound) {
        val v = x(k).min
        nbBound.incr()
        if (!isValueUsed(v - min).getValue()) {
          nbValueUsed.incr()
          isValueUsed(v - min).setValue(true)
        }
      }
    }

    nValueVar.updateMin(Math.max(nbValueUsed.getValue(), if (x.length > 0) 1 else 0))
    nValueVar.updateMax(nbValueUsed.getValue() + x.length - nbBound.getValue())

    for (k <- x.indices) {
      if (!x(k).isBound) {
        x(k).callValBindIdxWhenBind(this, k)
      }
      x(k).callPropagateWhenBind(this)
    }
    
    if (!nValueVar.isBound) {
      nValueVar.callPropagateWhenBoundsChange(this)
    }

    val ubNbValueUsed = nbValueUsed.getValue() + (x.length - nbBound.getValue())
    if (ubNbValueUsed <= nValueVar.getMin) {
      prune()
    }
  }

  override def valBindIdx(var_ : CPIntVar, idx: Int): Unit = {
    val `val` = var_.min
    nbBound.incr()
    if (!isValueUsed(`val` - min).getValue()) {
      nbValueUsed.incr()
      isValueUsed(`val` - min).setValue(true)
    }

    val ubNbValueUsed = nbValueUsed.getValue() + (x.length - nbBound.getValue())

    nValueVar.updateMin(nbValueUsed.getValue())
    nValueVar.updateMax(ubNbValueUsed)

    if (ubNbValueUsed == nValueVar.getMin) {
      prune()
    }
  }

  override def propagate(): Unit = {
    val ubNbValueUsed = nbValueUsed.getValue() + (x.length - nbBound.getValue())
    if (ubNbValueUsed == nValueVar.getMin) {
      prune()
    }
  }

  def prune(): Unit = {
    val values = new Array[Int](x.length)
    var nb = 0
    for (k <- x.indices) {
      if (x(k).isBound) {
        values(nb) = x(k).min
        nb += 1
      }
    }
    for (k <- x.indices) {
      if (!x(k).isBound) {
        for (i <- 0 until nb) {
          x(k).removeValue(values(i))
        }
      }
    }
  }

  private def findValueRange(): Unit = {
    min = Int.MaxValue
    max = Int.MinValue
    for (i <- x.indices) {
      min = Math.min(min, x(i).getMin)
      max = Math.max(max, x(i).getMax)
    }
    valSize = max - min + 1
  }
}
