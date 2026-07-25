package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}

class GCCVarAC(val x: Array[CPIntVar], val minValInit: Int, val o: Array[CPIntVar]) extends Constraint(x(0).store, "GCCVar") {

  protected val NONE: Int = Int.MinValue

  protected var minVal: Int = minValInit
  protected var maxVal: Int = minValInit + o.length - 1
  protected var nbVals: Int = maxVal - minVal + 1

  // value
  protected var low: Array[Int] = _
  protected var up: Array[Int] = _
  protected var flow: Array[Int] = _

  // flow
  protected var sizeFlow: Int = 0
  protected var varMatch: Array[Int] = _
  protected var next: Array[Int] = _
  protected var prev: Array[Int] = _
  protected var valMatch: Array[Int] = _
  protected var varSeen: Array[Int] = _
  protected var valSeen: Array[Int] = _
  protected var magic: Int = 0

  protected var dfs: Int = 0
  protected var component: Int = 0

  protected var varComponent: Array[Int] = _
  protected var varDfs: Array[Int] = _
  protected var varHigh: Array[Int] = _

  protected var valComponent: Array[Int] = _
  protected var valDfs: Array[Int] = _
  protected var valHigh: Array[Int] = _

  protected var sinkComponent: Int = 0
  protected var sinkDfs: Int = 0
  protected var sinkHigh: Int = 0

  protected var stack: Array[Int] = _
  protected var `type`: Array[Int] = _
  protected var top: Int = 0

  priorityL2 = CPStore.MaxPriorityL2 - 3

  override def associatedVars(): Iterable[CPVar] = x.toSeq ++ o.toSeq

  override def setup(l: CPPropagStrength): Unit = {
    findValueRange()
    allocateFlow()
    findInitialFlow()

    if (!findMaximalFlow()) throw Inconsistency
    if (!findFeasibleFlow()) throw Inconsistency

    allocateSCC()
    prune()
    pruneBounds()

    for (k <- x.indices) {
      if (!x(k).isBound) {
        x(k).callPropagateWhenDomainChanges(this)
      }
    }
    for (i <- o.indices) {
      o(i).callPropagateWhenBoundsChange(this)
    }
    propagate()
  }

  override def propagate(): Unit = {
    updateBounds()
    for (k <- x.indices) {
      if (varMatch(k) != NONE) {
        if (!x(k).hasValue(varMatch(k))) {
          unassign(k)
        }
      }
    }
    for (k <- minVal to maxVal) {
      while (flow(k - minVal) > up(k - minVal)) {
        unassign(valMatch(k - minVal))
      }
    }
    if (!findMaximalFlow()) throw Inconsistency
    if (!findFeasibleFlow()) throw Inconsistency
    prune()
    pruneBounds()
  }

  protected def findValueRange(): Unit = {
    val prev_minval = minVal

    for (i <- x.indices) {
      minVal = math.min(minVal, x(i).getMin)
      maxVal = math.max(maxVal, x(i).getMax)
    }
    val d = prev_minval - minVal

    nbVals = maxVal - minVal + 1

    low = new Array[Int](nbVals)
    up = new Array[Int](nbVals)
    for (k <- 0 until nbVals) {
      up(k) = x.length
    }

    for (i <- o.indices) {
      if (o(i).getMin > 0) {
        low(i + d) = o(i).getMin
      } else {
        o(i).updateMin(0)
      }
      if (o(i).getMax < x.length) {
        up(i + d) = o(i).getMax
      } else {
        o(i).updateMax(x.length)
      }
    }
  }

  protected def allocateFlow(): Unit = {
    flow = new Array[Int](nbVals)
    valMatch = new Array[Int](nbVals)
    for (k <- 0 until nbVals) {
      valMatch(k) = NONE
    }
    next = new Array[Int](x.length)
    for (k <- x.indices) {
      next(k) = NONE
    }
    prev = new Array[Int](x.length)
    for (k <- x.indices) {
      prev(k) = NONE
    }
    varMatch = new Array[Int](x.length)
    for (k <- x.indices) {
      varMatch(k) = NONE
    }
    varSeen = new Array[Int](x.length)
    valSeen = new Array[Int](nbVals)
    magic = 0
  }

  protected def assign(k: Int, v: Int): Unit = {
    sizeFlow += 1
    unassign(k)
    varMatch(k) = v
    flow(v - minVal) += 1
    val nk = valMatch(v - minVal)
    next(k) = nk
    prev(k) = NONE
    if (nk != NONE) {
      prev(nk) = k
    }
    valMatch(v - minVal) = k
  }

  protected def unassign(k: Int): Unit = {
    if (varMatch(k) != NONE) {
      sizeFlow -= 1
      val w = varMatch(k)
      flow(w - minVal) -= 1
      if (valMatch(w - minVal) == k) {
        val nk = next(k)
        valMatch(w - minVal) = nk
        if (nk != NONE) {
          prev(nk) = NONE
        }
      } else {
        val pk = prev(k)
        val nk = next(k)
        next(pk) = nk
        if (nk != NONE) {
          prev(nk) = pk
        }
      }
      varMatch(k) = NONE
    }
  }

  protected def findInitialFlow(): Unit = {
    sizeFlow = 0
    for (k <- x.indices) {
      val mx = x(k).getMin
      val Mx = x(k).getMax
      var found = false
      var i = mx
      while (i <= Mx && !found) {
        if (flow(i - minVal) < up(i - minVal)) {
          if (x(k).hasValue(i)) {
            assign(k, i)
            found = true
          }
        }
        i += 1
      }
    }
  }

  protected def findMaximalFlow(): Boolean = {
    if (sizeFlow < x.length) {
      for (k <- x.indices) {
        if (varMatch(k) == NONE) {
          magic += 1
          if (!findAugmentingPath(k)) return false
        }
      }
    }
    true
  }

  protected def findAugmentingPath(k: Int): Boolean = {
    if (varSeen(k) != magic) {
      varSeen(k) = magic
      val mx = x(k).getMin
      val Mx = x(k).getMax
      for (v <- mx to Mx) {
        if (varMatch(k) != v) {
          if (x(k).hasValue(v)) {
            if (findAugmentingPathValue(v)) {
              assign(k, v)
              return true
            }
          }
        }
      }
    }
    false
  }

  protected def findAugmentingPathValue(v: Int): Boolean = {
    val vind = v - minVal
    if (valSeen(vind) != magic) {
      valSeen(vind) = magic
      if (flow(vind) < up(vind)) {
        return true
      } else if (flow(vind) > 0) {
        var i = valMatch(vind)
        while (i != NONE) {
          if (findAugmentingPath(i)) return true
          i = next(i)
        }
      }
    }
    false
  }

  protected def findFeasibleFlow(): Boolean = {
    for (v <- minVal to maxVal) {
      while (flow(v - minVal) < low(v - minVal)) {
        if (!findFeasibleFlowTo(v)) return false
      }
    }
    true
  }

  protected def findFeasibleFlowTo(q: Int): Boolean = {
    magic += 1
    for (v <- minVal to maxVal) {
      if (flow(v - minVal) > low(v - minVal)) {
        if (findFeasibleFlowValue(v, q)) return true
      }
    }
    false
  }

  protected def findFeasibleFlowValue(v: Int, q: Int): Boolean = {
    val vind = v - minVal
    if (valSeen(vind) != magic) {
      valSeen(vind) = magic
      var i = valMatch(vind)
      while (i != NONE) {
        if (varMatch(i) != q && x(i).hasValue(q)) {
          assign(i, q)
          return true
        }
        i = next(i)
      }
      i = valMatch(vind)
      while (i != NONE) {
        if (findFeasibleFlowVar(i, q)) return true
        i = next(i)
      }
    }
    false
  }

  protected def findFeasibleFlowVar(k: Int, q: Int): Boolean = {
    if (varSeen(k) != magic) {
      varSeen(k) = magic
      val mx = x(k).getMin
      val Mx = x(k).getMax
      for (v <- mx to Mx) {
        if (q != v && varMatch(k) != v) {
          if (x(k).hasValue(v)) {
            if (findFeasibleFlowValue(v, q)) {
              assign(k, v)
              return true
            }
          }
        }
      }
    }
    false
  }

  protected def allocateSCC(): Unit = {
    varComponent = new Array[Int](x.length)
    varDfs = new Array[Int](x.length)
    varHigh = new Array[Int](x.length)

    valComponent = new Array[Int](nbVals)
    valDfs = new Array[Int](nbVals)
    valHigh = new Array[Int](nbVals)

    stack = new Array[Int](x.length + nbVals + 2)
    `type` = new Array[Int](x.length + nbVals + 2)
  }

  protected def prune(): Unit = {
    findSCC()
    for (k <- x.indices) {
      val mx = x(k).getMin
      val Mx = x(k).getMax
      for (w <- mx to Mx) {
        if (varMatch(k) != w) {
          if (varComponent(k) != valComponent(w - minVal)) {
            if (x(k).hasValue(w)) {
              x(k).removeValue(w)
            }
          }
        }
      }
    }
  }

  protected def initSCC(): Unit = {
    for (k <- x.indices) {
      varComponent(k) = 0
      varDfs(k) = 0
      varHigh(k) = 0
    }
    for (k <- minVal to maxVal) {
      valComponent(k - minVal) = 0
      valDfs(k - minVal) = 0
      valHigh(k - minVal) = 0
    }
    sinkComponent = 0
    sinkDfs = 0
    sinkHigh = 0

    top = 0
    dfs = x.length + (maxVal - minVal + 1) + 1
    component = 0
  }

  protected def findSCC(): Unit = {
    initSCC()
    for (k <- x.indices) {
      if (varDfs(k) == 0) {
        findSCCvar(k)
      }
    }
  }

  protected def findSCCvar(k: Int): Unit = {
    varDfs(k) = dfs
    dfs -= 1
    varHigh(k) = varDfs(k)
    stack(top) = k
    `type`(top) = 0
    top += 1
    val mx = x(k).getMin
    val Mx = x(k).getMax
    for (w <- mx to Mx) {
      val wind = w - minVal
      if (varMatch(k) != w) {
        if (x(k).hasValue(w)) {
          if (valDfs(wind) == 0) {
            findSCCval(w)
            if (valHigh(wind) > varHigh(k)) {
              varHigh(k) = valHigh(wind)
            }
          } else if ((valDfs(wind) > varDfs(k)) && (valComponent(wind) == 0)) {
            if (valDfs(wind) > varHigh(k)) {
              varHigh(k) = valDfs(wind)
            }
          }
        }
      }
    }
    if (varHigh(k) == varDfs(k)) {
      component += 1
      var done = false
      while (!done) {
        top -= 1
        val v = stack(top)
        val t = `type`(top)
        if (t == 0) {
          varComponent(v) = component
        } else if (t == 1) {
          valComponent(v - minVal) = component
        } else {
          sinkComponent = component
        }
        if (t == 0 && v == k) {
          done = true
        }
      }
    }
  }

  protected def findSCCval(v: Int): Unit = {
    val vind = v - minVal
    valDfs(vind) = dfs
    dfs -= 1
    valHigh(vind) = valDfs(vind)
    stack(top) = v
    `type`(top) = 1
    top += 1

    var k = valMatch(vind)
    while (k != NONE) {
      if (varDfs(k) == 0) {
        findSCCvar(k)
        if (varHigh(k) > valHigh(vind)) {
          valHigh(vind) = varHigh(k)
        }
      } else if ((varDfs(k) > valDfs(vind)) && (varComponent(k) == 0)) {
        if (varDfs(k) > valHigh(vind)) {
          valHigh(vind) = varDfs(k)
        }
      }
      k = next(k)
    }

    if (flow(vind) < up(vind)) {
      if (sinkDfs == 0) {
        findSCCsink()
        if (sinkHigh > valHigh(vind)) {
          valHigh(vind) = sinkHigh
        }
      } else if ((sinkDfs > valDfs(vind)) && (sinkComponent == 0) && (sinkDfs > valHigh(vind))) {
        valHigh(vind) = sinkDfs
      }
    }

    if (valHigh(vind) == valDfs(vind)) {
      component += 1
      var done = false
      while (!done) {
        top -= 1
        val i = stack(top)
        val t = `type`(top)
        if (t == 0) {
          varComponent(i) = component
        } else if (t == 1) {
          valComponent(i - minVal) = component
        } else {
          sinkComponent = component
        }
        if (t == 1 && i == v) {
          done = true
        }
      }
    }
  }

  protected def findSCCsink(): Unit = {
    sinkDfs = dfs
    dfs -= 1
    sinkHigh = sinkDfs
    stack(top) = NONE
    `type`(top) = 2
    top += 1
    for (i <- x.indices) {
      val w = varMatch(i)
      val wind = w - minVal
      if (flow(wind) > low(wind)) {
        if (valDfs(wind) == 0) {
          findSCCval(w)
          if (valHigh(wind) > sinkHigh) {
            sinkHigh = valHigh(wind)
          }
        } else if ((valDfs(wind) > sinkDfs) && (valComponent(wind) == 0) && (valDfs(wind) > sinkHigh)) {
          sinkHigh = valDfs(wind)
        }
      }
    }

    if (sinkHigh == sinkDfs) {
      component += 1
      var done = false
      while (!done) {
        top -= 1
        val i = stack(top)
        val t = `type`(top)
        if (t == 0) {
          varComponent(i) = component
        } else if (t == 1) {
          valComponent(i - minVal) = component
        } else {
          sinkComponent = component
        }
        if (t == 2) {
          done = true
        }
      }
    }
  }

  protected def decreaseMax(w: Int): Boolean = {
    val wind = w - minVal
    while (flow(wind) > up(wind)) {
      unassign(valMatch(wind))
    }
    if (!findMaximalFlow()) return false
    if (!findFeasibleFlow()) return false
    true
  }

  protected def increaseMin(w: Int): Boolean = {
    val wind = w - minVal
    while (flow(wind) < low(wind)) {
      if (!findFeasibleFlowTo(w)) return false
    }
    true
  }

  protected def pruneBounds(): Unit = {
    for (i <- o.indices) {
      val m = o(i).getMin
      val M = o(i).getMax
      if (m != M) {
        up(i + minValInit - minVal) = m
        while (!decreaseMax(i + minValInit)) {
          up(i + minValInit - minVal) += 1
        }
        o(i).updateMin(up(i + minValInit - minVal))
        up(i + minValInit - minVal) = M
      }
    }

    for (i <- o.indices) {
      val m = o(i).getMin
      val M = o(i).getMax
      if (m != M) {
        low(i + minValInit - minVal) = M
        while (!increaseMin(i + minValInit)) {
          low(i + minValInit - minVal) -= 1
        }
        o(i).updateMax(low(i + minValInit - minVal))
        low(i + minValInit - minVal) = m
      }
    }
  }

  protected def updateBounds(): Unit = {
    for (i <- o.indices) {
      var v = o(i).getMin
      if (v > 0) {
        low(i + minValInit - minVal) = v
      } else {
        low(i + minValInit - minVal) = 0
      }
      v = o(i).getMax
      if (v < x.length) {
        up(i + minValInit - minVal) = v
      } else {
        up(i + minValInit - minVal) = x.length
      }
    }
  }

}
