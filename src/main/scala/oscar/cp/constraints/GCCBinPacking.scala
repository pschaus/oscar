package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import scala.collection.Iterable

class GCCBinPacking(val x: Array[CPIntVar], val w: Array[Int], val l: Array[CPIntVar], val o: Array[CPIntVar]) extends Constraint(x(0).store, "GCCVar") {

  private val NONE = -Int.MaxValue

  private var minValInit: Int = 0
  private var minVal: Int = 0
  private var maxVal: Int = o.length - 1
  private var nbVals: Int = maxVal - minVal + 1

  private var low: Array[Int] = _
  private var up: Array[Int] = _
  private var flow: Array[Int] = _

  private var sizeFlow: Int = 0
  private var varMatch: Array[Int] = _
  private var next: Array[Int] = _
  private var prev: Array[Int] = _
  private var valMatch: Array[Int] = _
  private var varSeen: Array[Int] = _
  private var valSeen: Array[Int] = _
  private var magic: Int = 0

  private var dfs: Int = 0
  private var component: Int = 0

  private var varComponent: Array[Int] = _
  private var varDfs: Array[Int] = _
  private var varHigh: Array[Int] = _

  private var valComponent: Array[Int] = _
  private var valDfs: Array[Int] = _
  private var valHigh: Array[Int] = _

  private var sinkComponent: Int = 0
  private var sinkDfs: Int = 0
  private var sinkHigh: Int = 0

  private var stack: Array[Int] = _
  private var typeArray: Array[Int] = _
  private var top: Int = 0

  priorityL2 = CPStore.MaxPriorityL2 - 3

  override def setup(st: CPPropagStrength): Unit = {
    if (!findValueRange()) {
      throw oscar.algo.Inconsistency
    }
    allocateFlow()
    findInitialFlow()

    if (!findMaximalFlow()) {
      throw oscar.algo.Inconsistency
    }
    if (!findFeasibleFlow()) {
      throw oscar.algo.Inconsistency
    }

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
      l(i).callPropagateWhenBoundsChange(this)
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
    if (!findMaximalFlow()) {
      throw oscar.algo.Inconsistency
    }
    if (!findFeasibleFlow()) {
      throw oscar.algo.Inconsistency
    }

    prune()
    pruneBounds()
  }

  override def associatedVars(): Iterable[CPVar] = x.toSeq ++ l.toSeq ++ o.toSeq

  private def findValueRange(): Boolean = {
    val prev_minval = minVal

    for (i <- x.indices) {
      minVal = Math.min(minVal, x(i).getMin)
      maxVal = Math.max(maxVal, x(i).getMax)
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
    true
  }

  private def allocateFlow(): Unit = {
    flow = new Array[Int](nbVals)
    valMatch = Array.fill(nbVals)(NONE)
    next = Array.fill(x.length)(NONE)
    prev = Array.fill(x.length)(NONE)
    varMatch = Array.fill(x.length)(NONE)
    varSeen = new Array[Int](x.length)
    valSeen = new Array[Int](nbVals)
    magic = 0
  }

  private def assign(k: Int, v: Int): Unit = {
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

  private def unassign(k: Int): Unit = {
    if (varMatch(k) != NONE) {
      sizeFlow -= 1
      val w = varMatch(k)
      flow(w - minVal) -= 1
      if (valMatch(w - minVal) == k) {
        val nk = next(k)
        valMatch(w - minVal) = nk
        if (nk != NONE) prev(nk) = NONE
      } else {
        val pk = prev(k)
        val nk = next(k)
        next(pk) = nk
        if (nk != NONE) prev(nk) = pk
      }
      varMatch(k) = NONE
    }
  }

  private def findInitialFlow(): Unit = {
    sizeFlow = 0
    for (k <- x.indices) {
      val mx = x(k).getMin
      val Mx = x(k).getMax
      var i = mx
      var found = false
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

  private def findMaximalFlow(): Boolean = {
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

  private def findAugmentingPath(k: Int): Boolean = {
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

  private def findAugmentingPathValue(v: Int): Boolean = {
    val vind = v - minVal
    if (valSeen(vind) != magic) {
      valSeen(vind) = magic
      if (flow(vind) < up(vind)) return true
      else if (flow(vind) > 0) {
        var i = valMatch(vind)
        while (i != NONE) {
          if (findAugmentingPath(i)) return true
          i = next(i)
        }
      }
    }
    false
  }

  private def findResidualPathVariable(k: Int, targeti: Int, lookingForMinCard: Boolean, origVar: Int): Boolean = {
    if (k == targeti) true
    else if (varSeen(k) != magic) {
      varSeen(k) = magic
      val mx = x(k).getMin
      val Mx = x(k).getMax
      for (v <- mx to Mx) {
        if (varMatch(k) != v) {
          if (x(k).hasValue(v)) {
            if (findResidualPathValue(v, targeti, lookingForMinCard, origVar)) {
              assign(k, v)
              return true
            }
          }
        }
      }
      false
    } else false
  }

  private def findResidualPathValue(v: Int, targeti: Int, lookingForMinCard: Boolean, origVar: Int): Boolean = {
    val vind = v - minVal
    if (valSeen(vind) != magic) {
      valSeen(vind) = magic
      var k = valMatch(vind)
      while (k != NONE) {
        if (lookingForMinCard) {
          if ((varMatch(k) != origVar || k >= targeti) && findResidualPathVariable(k, targeti, lookingForMinCard, origVar)) return true
        } else {
          if ((varMatch(k) != origVar || k <= targeti) && findResidualPathVariable(k, targeti, lookingForMinCard, origVar)) return true
        }
        k = next(k)
      }
      for (u <- 0 until nbVals) {
        if (u != vind && flow(u) > low(u)) {
          if (findResidualPathValue(u + minVal, targeti, lookingForMinCard, origVar)) return true
        }
      }
    }
    false
  }

  private def findFeasibleFlow(): Boolean = {
    for (v <- minVal to maxVal) {
      while (flow(v - minVal) < low(v - minVal)) {
        if (!findFeasibleFlowTo(v)) return false
      }
    }
    true
  }

  private def findFeasibleFlowTo(q: Int): Boolean = {
    magic += 1
    for (v <- minVal to maxVal) {
      if (flow(v - minVal) > low(v - minVal)) {
        if (findFeasibleFlowValue(v, q)) return true
      }
    }
    false
  }

  private def findFeasibleFlowValue(v: Int, q: Int): Boolean = {
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

  private def findFeasibleFlowVar(k: Int, q: Int): Boolean = {
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

  private def allocateSCC(): Unit = {
    varComponent = new Array[Int](x.length)
    varDfs = new Array[Int](x.length)
    varHigh = new Array[Int](x.length)

    valComponent = new Array[Int](nbVals)
    valDfs = new Array[Int](nbVals)
    valHigh = new Array[Int](nbVals)

    stack = new Array[Int](x.length + nbVals + 2)
    typeArray = new Array[Int](x.length + nbVals + 2)
  }

  private def prune(): Unit = {
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

  private def initSCC(): Unit = {
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

  private def findSCC(): Unit = {
    initSCC()
    for (k <- x.indices) {
      if (varDfs(k) == 0) {
        findSCCvar(k)
      }
    }
  }

  private def findSCCvar(k: Int): Unit = {
    varDfs(k) = dfs
    dfs -= 1
    varHigh(k) = varDfs(k)
    stack(top) = k
    typeArray(top) = 0
    top += 1

    val mx = x(k).getMin
    val Mx = x(k).getMax
    for (w <- mx to Mx) {
      val wind = w - minVal
      if (varMatch(k) != w) {
        if (x(k).hasValue(w)) {
          if (valDfs(wind) == 0) {
            findSCCval(w)
            if (valHigh(wind) > varHigh(k)) varHigh(k) = valHigh(wind)
          } else if (valDfs(wind) > varDfs(k) && valComponent(wind) == 0) {
            if (valDfs(wind) > varHigh(k)) varHigh(k) = valDfs(wind)
          }
        }
      }
    }
    if (varHigh(k) == varDfs(k)) {
      component += 1
      var breakLoop = false
      while (!breakLoop) {
        top -= 1
        val v = stack(top)
        val t = typeArray(top)
        if (t == 0) varComponent(v) = component
        else if (t == 1) valComponent(v - minVal) = component
        else sinkComponent = component
        if (t == 0 && v == k) breakLoop = true
      }
    }
  }

  private def findSCCval(v: Int): Unit = {
    val vind = v - minVal
    valDfs(vind) = dfs
    dfs -= 1
    valHigh(vind) = valDfs(vind)
    stack(top) = v
    typeArray(top) = 1
    top += 1

    var k = valMatch(vind)
    while (k != NONE) {
      if (varDfs(k) == 0) {
        findSCCvar(k)
        if (varHigh(k) > valHigh(vind)) valHigh(vind) = varHigh(k)
      } else if (varDfs(k) > valDfs(vind) && varComponent(k) == 0) {
        if (varDfs(k) > valHigh(vind)) valHigh(vind) = varDfs(k)
      }
      k = next(k)
    }

    if (flow(vind) < up(vind)) {
      if (sinkDfs == 0) {
        findSCCsink()
        if (sinkHigh > valHigh(vind)) valHigh(vind) = sinkHigh
      } else if (sinkDfs > valDfs(vind) && sinkComponent == 0 && sinkDfs > valHigh(vind)) {
        valHigh(vind) = sinkDfs
      }
    }

    if (valHigh(vind) == valDfs(vind)) {
      component += 1
      var breakLoop = false
      while (!breakLoop) {
        top -= 1
        val i = stack(top)
        val t = typeArray(top)
        if (t == 0) varComponent(i) = component
        else if (t == 1) valComponent(i - minVal) = component
        else sinkComponent = component
        if (t == 1 && i == v) breakLoop = true
      }
    }
  }

  private def findSCCsink(): Unit = {
    sinkDfs = dfs
    dfs -= 1
    sinkHigh = sinkDfs
    stack(top) = NONE
    typeArray(top) = 2
    top += 1

    for (i <- x.indices) {
      val w = varMatch(i)
      val wind = w - minVal
      if (flow(wind) > low(wind)) {
        if (valDfs(wind) == 0) {
          findSCCval(w)
          if (valHigh(wind) > sinkHigh) sinkHigh = valHigh(wind)
        } else if (valDfs(wind) > sinkDfs && valComponent(wind) == 0 && valDfs(wind) > sinkHigh) {
          sinkHigh = valDfs(wind)
        }
      }
    }

    if (sinkHigh == sinkDfs) {
      component += 1
      var breakLoop = false
      while (!breakLoop) {
        top -= 1
        val i = stack(top)
        val t = typeArray(top)
        if (t == 0) varComponent(i) = component
        else if (t == 1) valComponent(i - minVal) = component
        else sinkComponent = component
        if (t == 2) breakLoop = true
      }
    }
  }

  private def decreaseMax(w: Int): Boolean = {
    val wind = w - minVal
    while (flow(wind) > up(wind)) unassign(valMatch(wind))
    if (!findMaximalFlow()) return false
    if (!findFeasibleFlow()) return false
    true
  }

  private def increaseMin(w: Int): Boolean = {
    val wind = w - minVal
    while (flow(wind) < low(wind)) {
      if (!findFeasibleFlowTo(w)) return false
    }
    true
  }

  private def pruneBounds(): Unit = {
    for (v <- o.indices) {
      val m = o(v).getMin
      val M = o(v).getMax
      if (m != M) {
        up(v + minValInit - minVal) = m
        while (!decreaseMax(v + minValInit)) {
          up(v + minValInit - minVal) += 1
        }
        o(v).updateMin(up(v + minValInit - minVal))
        up(v + minValInit - minVal) = M
      }
    }

    for (v <- o.indices) {
      val m = o(v).getMin
      val M = o(v).getMax
      if (m != M) {
        low(v + minValInit - minVal) = M
        while (!increaseMin(v + minValInit)) {
          low(v + minValInit - minVal) -= 1
        }
        o(v).updateMax(low(v + minValInit - minVal))
        low(v + minValInit - minVal) = m
      }

      val alreadyPacked = new Array[Boolean](x.length)
      var nPacked = 0
      var wPacked = 0
      for (k <- x.indices) {
        if (x(k).isBound) {
          alreadyPacked(k) = true
          if (x(k).min == v) {
            nPacked += 1
            wPacked += w(k)
          }
        } else {
          alreadyPacked(k) = false
        }
      }

      var currLoad = wPacked
      var currCard = nPacked
      var k = 0

      while (k < x.length && currLoad < l(v - minVal).min && currCard < up(v - minVal)) {
        magic += 1
        if (!alreadyPacked(k) && x(k).hasValue(v) && (varMatch(k) == v || findResidualPathValue(v, k, true, k))) {
          assign(k, v)
          currLoad += w(k)
          currCard += 1
        }
        k += 1
      }

      if (currLoad < l(v - minVal).min) throw oscar.algo.Inconsistency
      o(v - minVal).updateMin(currCard)

      while (k < x.length && currCard < up(v - minVal)) {
        magic += 1
        if (!alreadyPacked(k) && x(k).hasValue(v) && (varMatch(k) == v || findResidualPathValue(v, k, true, k))) {
          assign(k, v)
          currLoad += w(k)
          currCard += 1
        }
        k += 1
      }
      l(v - minVal).updateMax(currLoad)

      currLoad = wPacked
      currCard = nPacked
      k = x.length - 1

      while (k >= 0 && currLoad + w(k) <= l(v - minVal).max && currCard < up(v - minVal)) {
        magic += 2
        if (!alreadyPacked(k) && x(k).hasValue(v) && (varMatch(k) == v || findResidualPathValue(v, k, false, k))) {
          assign(k, v)
          currLoad += w(k)
          currCard += 1
          if (currCard == o(v - minVal).min) {
            l(v - minVal).updateMin(currLoad)
          }
        }
        k -= 1
      }
      o(v - minVal).updateMax(currCard)
    }
  }

  private def updateBounds(): Unit = {
    for (i <- o.indices) {
      var v = o(i).getMin
      if (v > 0) low(i + minValInit - minVal) = v
      else low(i + minValInit - minVal) = 0
      v = o(i).getMax
      if (v < x.length) up(i + minValInit - minVal) = v
      else up(i + minValInit - minVal) = x.length
    }
  }
}
