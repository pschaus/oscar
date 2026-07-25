package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class BinaryKnapsack(b: Array[CPBoolVar], weights: Array[Int], val c: CPIntVar, val n: Int = -1) extends Constraint(b(0).store, "BinaryKnapsack") {
  require(b.length == weights.length)

  private val perm = b.indices.sortBy(i => -weights(i)).toArray
  val w: Array[Int] = perm.map(weights)
  val x: Array[CPBoolVar] = perm.map(b)

  private var candidate: Array[ReversibleInt] = _
  private var rcap: ReversibleInt = _
  private var pcap: ReversibleInt = _
  private var nb: ReversibleInt = _

  private var alpha_ : Int = 0
  private var beta_ : Int = 0
  private var X: Array[Int] = _

  priorityL2 = CPStore.MaxPriorityL2 - 2

  def this(b: Array[CPBoolVar], weights: Array[Int], load: Int, n: Int) = this(b, weights, CPIntVar(b(0).store, load, load), n)

  def this(b: Array[CPBoolVar], weights: Array[Int], load: Int) = this(b, weights, CPIntVar(b(0).store, load, load), -1)

  override def associatedVars(): Iterable[CPVar] = x.toSeq :+ c

  override def setup(l: CPPropagStrength): Unit = {
    if (n > 0) {
      s.post(new BinaryKnapsackWithCardinality(x, w, c, n))
    }

    s.post(new LightBinaryKnapsack(x, w, c))
    if (l == CPPropagStrength.Weak) {
      deactivate()
      return
    }

    candidate = new Array[ReversibleInt](x.length)
    for (i <- candidate.indices) {
      candidate(i) = new ReversibleInt(s, 0)
    }

    var S = 0
    for (i <- w.indices) {
      S += w(i)
      candidate(i).setValue(1)
    }

    rcap = new ReversibleInt(s, 0)
    pcap = new ReversibleInt(s, S)
    nb = new ReversibleInt(s, x.length)

    for (i <- x.indices) {
      if (x(i).isBound) {
        if (x(i).isTrue) {
          bind(i)
        } else {
          remove(i)
        }
      } else {
        x(i).callValBindIdxWhenBind(this, i)
        x(i).callPropagateWhenDomainChanges(this)
      }
    }
    if (!c.isBound) c.callPropagateWhenBoundsChange(this)

    alpha_ = 0
    beta_ = 0
    X = new Array[Int](x.length)

    propagate()
  }

  override def valBindIdx(var_ : CPIntVar, idx: Int): Unit = {
    if (var_.getMin == 1) bind(idx)
    else remove(idx)
  }

  private def bind(i: Int): Unit = {
    val wi = w(i)
    val nrcap = rcap.getValue() + wi
    c.updateMin(nrcap)
    rcap.setValue(nrcap)
    candidate(i).setValue(0)
    nb.decr()
  }

  private def remove(i: Int): Unit = {
    pcap.setValue(pcap.getValue() - w(i))
    c.updateMax(pcap.getValue())
    candidate(i).setValue(0)
    nb.decr()
  }

  override def propagate(): Unit = {
    this.alpha_ = 0
    this.beta_ = 0
    val leftover = c.getMax - rcap.getValue()
    val slack = pcap.getValue() - c.getMin
    for (k <- x.indices) {
      if (candidate(k).getValue() == 1) {
        if (w(k) > leftover) {
          x(k).removeValue(1)
          return
        }
        if (w(k) > slack) {
          x(k).assign(1)
          return
        }
      }
    }

    val pruneMore = true
    if (nb.getValue() <= 2) return
    if (noSumPossible(c.min - rcap.getValue(), c.getMax - rcap.getValue())) {
      throw oscar.algo.Inconsistency
    }

    if (pruneMore) {
      var lastsize = -1
      for (k <- x.indices) {
        if (candidate(k).getValue() == 1 && w(k) != lastsize) {
          lastsize = w(k)
          candidate(k).setValue(0)
          val toremove = noSumPossible(Math.max(c.getMin, rcap.getValue() + w(k)) - rcap.getValue() - w(k), c.getMax - rcap.getValue() - w(k))
          candidate(k).setValue(1)
          if (toremove) {
            x(k).removeValue(1)
            return
          }
        }
      }
      lastsize = -1
      for (k <- x.indices) {
        if (candidate(k).getValue() == 1 && w(k) != lastsize) {
          lastsize = w(k)
          candidate(k).setValue(0)
          val toinsert = noSumPossible(c.getMin - rcap.getValue(), Math.min(c.getMax, pcap.getValue() - w(k)) - rcap.getValue())
          candidate(k).setValue(1)
          if (toinsert) {
            x(k).assign(1)
          }
        }
      }
    }
    if (noSumPossible(c.getMin - rcap.getValue(), c.getMin - rcap.getValue())) {
      c.updateMin(rcap.getValue() + beta_)
    }
    if (noSumPossible(c.getMax - rcap.getValue(), c.getMax - rcap.getValue())) {
      c.updateMax(rcap.getValue() + alpha_)
    }
  }

  private def noSumPossible(alpha: Int, beta: Int): Boolean = {
    require(alpha <= beta)

    if (alpha <= 0 || beta >= pcap.getValue()) {
      return false
    }

    var Xs = 0
    for (i <- x.indices) {
      if (candidate(i).getValue() == 1) Xs += 1
    }

    var sumX = 0
    var l = 0
    for (i <- 0 until Xs) {
      while (candidate(l).getValue() == 0) {
        l += 1
      }
      X(i) = w(l)
      sumX += X(i)
      l += 1
    }

    if (beta >= sumX) return false

    var Sa = 0
    var Sb = 0
    var Sc = 0
    var k = 0
    var k_ = 0

    while (Sc + X(Xs - k_ - 1) < alpha) {
      Sc += X(Xs - k_ - 1)
      k_ += 1
    }
    Sb = X(Xs - k_ - 1)
    while (Sa < alpha && Sb <= beta) {
      k += 1
      Sa += X(k - 1)
      if (Sa < alpha) {
        k_ -= 1
        Sb += X(Xs - k_ - 1)
        Sc -= X(Xs - k_ - 1)
        while (Sa + Sc >= alpha) {
          k_ -= 1
          Sc -= X(Xs - k_ - 1)
          Sb += X(Xs - k_ - 1) - X(Xs - k_ - k - 1 - 1)
        }
      }
    }
    alpha_ = Sa + Sc
    beta_ = Sb
    Sa < alpha
  }
}
