package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

private[constraints] sealed trait FlowType
private[constraints] object FlowType {
  case object UF extends FlowType
  case object OF extends FlowType
}

/**
 * Soft Global Cardinality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
class SoftGCCAC(val x: Array[CPIntVar], val minvalInit: Int, lowInit: Array[Int], upInit: Array[Int], val viol: CPIntVar) 
  extends Constraint(x(0).store, "SoftGCCAC") {

  private val NONE: Int = Int.MinValue

  private var posted: Boolean = false

  private var minval: Int = minvalInit
  private var maxval: Int = minvalInit + lowInit.length - 1
  private var nbVals: Int = maxval - minval + 1
  private var low: Array[Int] = lowInit
  private var up: Array[Int] = upInit

  private var sumLow: Int = 0

  // flow ("uf" refers to underflow and "of" to overflow)
  private var flow_uf: Array[Int] = _
  private var varMatch_uf: Array[Int] = _
  private var valMatch_uf: Array[Int] = _
  private var sizeFlow_uf: Int = 0
  private var next_uf: Array[Int] = _
  private var prev_uf: Array[Int] = _

  private var flow_of: Array[Int] = _
  private var varMatch_of: Array[Int] = _
  private var valMatch_of: Array[Int] = _
  private var sizeFlow_of: Int = 0
  private var next_of: Array[Int] = _
  private var prev_of: Array[Int] = _

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

  private var isVarAlwaysMatched_uf: Array[Boolean] = _
  private var isVarAlwaysMatched_of: Array[Boolean] = _

  private var stack: Array[Int] = _
  private var `type`: Array[Int] = _
  private var top: Int = 0

  priorityL2 = CPStore.MaxPriorityL2 - 2
  check()

  override def associatedVars(): Iterable[CPVar] = x.toSeq :+ viol

  private def check(): Unit = {
    if (nbVals != low.length) {
      throw new RuntimeException("vals and low must be of the same size")
    }
    if (nbVals != up.length) {
      throw new RuntimeException("vals and up must be of the same size")
    }
    for (i <- 0 until nbVals) {
      if (low(i) < 0) {
        throw new RuntimeException("low vals must be >= 0")
      }
      if (up(i) < 0) {
        throw new RuntimeException("up vals must be >= 0")
      }
      if (low(i) > up(i)) {
        throw new RuntimeException("low[i] must be <= up[i]")
      }
    }
  }

  override def setup(l: CPPropagStrength): Unit = {
    posted = true

    findValueRange()
    allocateFlow()
    findInitialFlow()

    val valViol = getValueViolation
    viol.updateMin(valViol)

    allocateSCC()

    propagate()

    for (k <- x.indices) {
      if (!x(k).isBound)
        x(k).callPropagateWhenDomainChanges(this)
    }
    if (!viol.isBound) viol.callPropagateWhenBoundsChange(this)
  }

  override def propagate(): Unit = {
    for (k <- x.indices) {
      if (varMatch_uf(k) != NONE) {
        if (!x(k).hasValue(varMatch_uf(k))) {
          unassign(k, FlowType.UF)
        }
      }
      if (varMatch_of(k) != NONE) {
        if (!x(k).hasValue(varMatch_of(k))) {
          unassign(k, FlowType.OF)
        }
      }
    }

    val valViol = getValueViolation

    //prune lower bound of violation
    viol.updateMin(valViol)

    //prune variable domains (the constraint is consistent at this point)
    prune(valViol)

    //prune upper bound of violation if all variables are bound
    var allBound = true
    var i = 0
    while (i < x.length && allBound) {
      if (!x(i).isBound) {
        allBound = false
      }
      i += 1
    }
    if (allBound)
      viol.updateMax(valViol)
  }

  private def findValueRange(): Unit = {
    val prev_minval = minval

    for (i <- x.indices) {
      minval = math.min(minval, x(i).getMin)
      maxval = math.max(maxval, x(i).getMax)
    }
    val d = prev_minval - minval

    nbVals = maxval - minval + 1

    // low
    val low_ = new Array[Int](nbVals)

    // up
    val up_ = new Array[Int](nbVals)
    for (k <- 0 until nbVals) {
      up_(k) = x.length
    }

    sumLow = 0

    for (i <- low.indices) {
      if (low(i) > 0) {
        low_(i + d) = low(i)
        sumLow += low(i)
      }
    }

    for (i <- up.indices) {
      if (up(i) < x.length) {
        up_(i + d) = up(i)
      }
    }
    low = low_
    up = up_
  }

  def hasValInBestAssignment(i: Int): Boolean = {
    if (!posted || i < 0 || i >= x.length) return false
    if (varMatch_of(i) == NONE) false else true
  }

  def getValInBestAssignment(i: Int): Int = {
    if (hasValInBestAssignment(i)) {
      varMatch_of(i)
    } else if (i < x.length && i >= 0 && posted) {
      x(i).getMin
    } else {
      NONE
    }
  }

  def getReducedCost(i: Int, v: Int): Int = {
    if (i >= x.length || i < 0 || !posted || !x(i).hasValue(v)) {
      return Int.MaxValue
    }

    findBestUnderFlow()
    findBestOverFlow()

    if (!hasValInBestAssignment(i) || varMatch_of(i) == v) {
      return 0
    }

    var reducedCost = 0

    findSCC(FlowType.UF)
    computeIsVarAlwaysMatched(FlowType.UF)
    if (varComponent(i) != valComponent(v - minval) && (low(v - minval) > 0 || isVarAlwaysMatched_uf(i))) {
      reducedCost += 1
    }

    findSCC(FlowType.OF)
    computeIsVarAlwaysMatched(FlowType.OF)
    if (varComponent(i) != valComponent(v - minval) && (up(v - minval) > 0 || isVarAlwaysMatched_of(i))) {
      reducedCost += 1
    }

    reducedCost
  }

  private def allocateFlow(): Unit = {
    // flow
    flow_uf = new Array[Int](nbVals)
    flow_of = new Array[Int](nbVals)

    // first variable matched
    valMatch_uf = new Array[Int](nbVals)
    valMatch_of = new Array[Int](nbVals)
    for (k <- 0 until nbVals) {
      valMatch_uf(k) = NONE // unmatched
      valMatch_of(k) = NONE // unmatched
    }

    // next variable matched
    next_uf = new Array[Int](x.length)
    next_of = new Array[Int](x.length)
    for (k <- x.indices) {
      next_uf(k) = NONE // no next
      next_of(k) = NONE // no next
    }

    // previous variable matched
    prev_uf = new Array[Int](x.length)
    prev_of = new Array[Int](x.length)
    for (k <- x.indices) {
      prev_uf(k) = NONE // no prev
      prev_of(k) = NONE // no prev
    }

    // variable assignment
    varMatch_uf = new Array[Int](x.length)
    varMatch_of = new Array[Int](x.length)
    for (k <- x.indices) {
      varMatch_uf(k) = NONE // unmatched
      varMatch_of(k) = NONE // unmatched
    }

    // flag
    varSeen = new Array[Int](x.length)
    valSeen = new Array[Int](nbVals)
    magic = 0
  }

  private def assign(k: Int, v: Int, ft: FlowType): Unit = {
    var flow: Array[Int] = null
    var varMatch: Array[Int] = null
    var next: Array[Int] = null
    var prev: Array[Int] = null
    var valMatch: Array[Int] = null

    if (ft == FlowType.UF) {
      flow = flow_uf
      varMatch = varMatch_uf
      next = next_uf
      prev = prev_uf
      valMatch = valMatch_uf
      sizeFlow_uf += 1
    } else { // OF
      flow = flow_of
      varMatch = varMatch_of
      next = next_of
      prev = prev_of
      valMatch = valMatch_of
      sizeFlow_of += 1
    }

    unassign(k, ft)

    // k is now first on the list of v
    varMatch(k) = v
    flow(v - minval) += 1
    val nk = valMatch(v - minval)
    next(k) = nk
    prev(k) = NONE
    if (nk != NONE)
      prev(nk) = k
    valMatch(v - minval) = k
  }

  private def unassign(k: Int, ft: FlowType): Unit = {
    var flow: Array[Int] = null
    var varMatch: Array[Int] = null
    var next: Array[Int] = null
    var prev: Array[Int] = null
    var valMatch: Array[Int] = null

    if (ft == FlowType.UF) {
      flow = flow_uf
      varMatch = varMatch_uf
      next = next_uf
      prev = prev_uf
      valMatch = valMatch_uf
    } else { // OF
      flow = flow_of
      varMatch = varMatch_of
      next = next_of
      prev = prev_of
      valMatch = valMatch_of
    }

    if (varMatch(k) != NONE) { // this guy is assigned; must be removed
      if (ft == FlowType.UF) sizeFlow_uf -= 1
      else sizeFlow_of -= 1

      val w = varMatch(k)
      flow(w - minval) -= 1
      if (valMatch(w - minval) == k) { // first in the list
        val nk = next(k)
        valMatch(w - minval) = nk
        if (nk != NONE)
          prev(nk) = NONE // nk is now first
      } else { // not first
        val pk = prev(k)
        val nk = next(k)
        next(pk) = nk
        if (nk != NONE)
          prev(nk) = pk
      }
      varMatch(k) = NONE
    }
  }

  private def findInitialFlow(): Unit = {
    sizeFlow_uf = 0
    sizeFlow_of = 0
    for (k <- x.indices) {
      val mx = x(k).getMin
      val Mx = x(k).getMax
      
      var foundUf = false
      var i = mx
      while (i <= Mx && !foundUf) {
        if (flow_uf(i - minval) < low(i - minval)) {
          if (x(k).hasValue(i)) {
            assign(k, i, FlowType.UF)
            foundUf = true
          }
        }
        i += 1
      }
      
      var foundOf = false
      i = mx
      while (i <= Mx && !foundOf) {
        if (flow_of(i - minval) < up(i - minval)) {
          if (x(k).hasValue(i)) {
            assign(k, i, FlowType.OF)
            foundOf = true
          }
        }
        i += 1
      }
    }
  }

  private def getValueViolation: Int = {
    val buf = findBestUnderFlow()
    val bof = findBestOverFlow()
    buf + bof
  }

  private def findBestUnderFlow(): Int = {
    var k = 0
    while (k < x.length && sizeFlow_uf < x.length) {
      if (varMatch_uf(k) == NONE) {
        magic += 1
        findAugmentingPath(k, FlowType.UF)
      }
      k += 1
    }
    sumLow - sizeFlow_uf
  }

  private def findBestOverFlow(): Int = {
    for (i <- minval to maxval) {
      flow_of(i - minval) = flow_uf(i - minval)
      valMatch_of(i - minval) = valMatch_uf(i - minval)
    }
    for (k <- x.indices) {
      varMatch_of(k) = varMatch_uf(k)
      next_of(k) = next_uf(k)
      prev_of(k) = prev_uf(k)
    }
    sizeFlow_of = sizeFlow_uf

    var k = 0
    while (k < x.length && sizeFlow_of < x.length) {
      if (varMatch_of(k) == NONE) {
        magic += 1
        findAugmentingPath(k, FlowType.OF)
      }
      k += 1
    }
    x.length - sizeFlow_of
  }

  private def findAugmentingPath(k: Int, ft: FlowType): Boolean = {
    val varMatch = if (ft == FlowType.UF) varMatch_uf else varMatch_of

    if (varSeen(k) != magic) {
      varSeen(k) = magic
      val mx = x(k).getMin
      val Mx = x(k).getMax
      for (v <- mx to Mx) {
        if (varMatch(k) != v) {
          if (x(k).hasValue(v)) {
            if (findAugmentingPathValue(v, ft)) {
              assign(k, v, ft)
              return true
            }
          }
        }
      }
    }
    false
  }

  private def findAugmentingPathValue(v: Int, ft: FlowType): Boolean = {
    var flow: Array[Int] = null
    var next: Array[Int] = null
    var valMatch: Array[Int] = null
    var capa: Array[Int] = null

    if (ft == FlowType.UF) {
      flow = flow_uf
      next = next_uf
      valMatch = valMatch_uf
      capa = low
    } else { // OF
      flow = flow_of
      next = next_of
      valMatch = valMatch_of
      capa = up
    }

    if (valSeen(v - minval) != magic) {
      valSeen(v - minval) = magic
      if (flow(v - minval) < capa(v - minval))
        return true
      else if (flow(v - minval) > 0) {
        var i = valMatch(v - minval)
        while (i != NONE) {
          if (findAugmentingPath(i, ft))
            return true
          i = next(i)
        }
      }
    }
    false
  }

  private def computeIsVarAlwaysMatched(ft: FlowType): Unit = {
    var isVarAlwaysMatched: Array[Boolean] = null
    var varMatch: Array[Int] = null

    if (ft == FlowType.UF) {
      isVarAlwaysMatched = isVarAlwaysMatched_uf
      varMatch = varMatch_uf
    } else {
      isVarAlwaysMatched = isVarAlwaysMatched_of
      varMatch = varMatch_of
    }

    val nbVarInComponent = new Array[Int](component + 1)
    for (k <- x.indices) {
      if (varMatch(k) == NONE) {
        nbVarInComponent(varComponent(k)) += 1
      }
    }
    for (k <- x.indices) {
      isVarAlwaysMatched(k) = false
      if (varMatch(k) != NONE && nbVarInComponent(varComponent(k)) == 0) {
        isVarAlwaysMatched(k) = true
      }
    }
  }

  private def allocateSCC(): Unit = {
    varComponent = new Array[Int](x.length)
    varDfs = new Array[Int](x.length)
    varHigh = new Array[Int](x.length)

    valComponent = new Array[Int](nbVals)
    valDfs = new Array[Int](nbVals)
    valHigh = new Array[Int](nbVals)

    stack = new Array[Int](x.length + nbVals + 1)
    `type` = new Array[Int](x.length + nbVals + 1)

    isVarAlwaysMatched_uf = new Array[Boolean](x.length)
    isVarAlwaysMatched_of = new Array[Boolean](x.length)
  }

  private def initSCC(): Unit = {
    for (k <- x.indices) {
      varComponent(k) = 0
      varDfs(k) = 0
      varHigh(k) = 0
    }
    for (k <- 0 until nbVals) {
      valComponent(k) = 0
      valDfs(k) = 0
      valHigh(k) = 0
    }

    sinkComponent = 0
    sinkDfs = 0
    sinkHigh = 0

    top = 0
    dfs = x.length + nbVals + 1
    component = 0
  }

  private def findSCC(ft: FlowType): Unit = {
    initSCC()
    for (k <- x.indices) {
      if (varDfs(k) == 0)
        findSCCvar(k, ft)
    }
  }

  private def findSCCvar(k: Int, ft: FlowType): Unit = {
    val varMatch = if (ft == FlowType.UF) varMatch_uf else varMatch_of

    varDfs(k) = dfs
    dfs -= 1
    varHigh(k) = varDfs(k)
    stack(top) = k
    `type`(top) = 0
    top += 1

    val mx = x(k).getMin
    val Mx = x(k).getMax
    for (w <- mx to Mx) {
      if (varMatch(k) != w) {
        if (x(k).hasValue(w)) {
          if (valDfs(w - minval) == 0) {
            findSCCval(w, ft)
            if (valHigh(w - minval) > varHigh(k))
              varHigh(k) = valHigh(w - minval)
          } else if ((valDfs(w - minval) > varDfs(k)) && (valComponent(w - minval) == 0)) {
            if (valDfs(w - minval) > varHigh(k))
              varHigh(k) = valDfs(w - minval)
          }
        }
      }
    }

    if (varMatch(k) != NONE) {
      for (i <- x.indices) {
        if (varMatch(i) == NONE) {
          if (varDfs(i) == 0) {
            findSCCvar(i, ft)
            if (varHigh(i) > varHigh(k))
              varHigh(k) = varHigh(i)
          } else if ((varDfs(i) > varDfs(k)) && (varComponent(i) == 0)) {
            if (varDfs(i) > varHigh(k))
              varHigh(k) = varDfs(i)
          }
        }
      }
    }

    if (varHigh(k) == varDfs(k)) {
      component += 1
      var done = false
      while (!done) {
        top -= 1
        val i = stack(top)
        val t = `type`(top)
        if (t == 0)
          varComponent(i) = component
        else if (t == 1)
          valComponent(i - minval) = component
        else
          sinkComponent = component
        if (t == 0 && i == k)
          done = true
      }
    }
  }

  private def findSCCval(v: Int, ft: FlowType): Unit = {
    val valMatch = if (ft == FlowType.UF) valMatch_uf else valMatch_of
    val capa = if (ft == FlowType.UF) low else up
    val next = if (ft == FlowType.UF) next_uf else next_of
    val flow = if (ft == FlowType.UF) flow_uf else flow_of

    valDfs(v - minval) = dfs
    dfs -= 1
    valHigh(v - minval) = valDfs(v - minval)
    stack(top) = v
    `type`(top) = 1
    top += 1

    var k = valMatch(v - minval)
    while (k != NONE) {
      if (varDfs(k) == 0) {
        findSCCvar(k, ft)
        if (varHigh(k) > valHigh(v - minval))
          valHigh(v - minval) = varHigh(k)
      } else if ((varDfs(k) > valDfs(v - minval)) && (varComponent(k) == 0)) {
        if (varDfs(k) > valHigh(v - minval))
          valHigh(v - minval) = varDfs(k)
      }
      k = next(k)
    }

    if (flow(v - minval) < capa(v - minval)) {
      if (sinkDfs == 0) {
        findSCCsink(ft)
        if (sinkHigh > valHigh(v - minval))
          valHigh(v - minval) = sinkHigh
      } else if ((sinkDfs > valDfs(v - minval)) && (sinkComponent == 0)) {
        if (sinkDfs > valHigh(v - minval))
          valHigh(v - minval) = sinkDfs
      }
    }

    if (valHigh(v - minval) == valDfs(v - minval)) {
      component += 1
      var done = false
      while (!done) {
        top -= 1
        val i = stack(top)
        val t = `type`(top)
        if (t == 0)
          varComponent(i) = component
        else if (t == 1)
          valComponent(i - minval) = component
        else
          sinkComponent = component
        if (t == 1 && i == v)
          done = true
      }
    }
  }

  private def findSCCsink(ft: FlowType): Unit = {
    val varMatch = if (ft == FlowType.UF) varMatch_uf else varMatch_of
    val flow = if (ft == FlowType.UF) flow_uf else flow_of

    sinkDfs = dfs
    dfs -= 1
    sinkHigh = sinkDfs
    stack(top) = NONE
    `type`(top) = 2
    top += 1

    for (i <- x.indices) {
      val w = varMatch(i)
      if (w != NONE) {
        if (flow(w - minval) > 0) {
          if (valDfs(w - minval) == 0) {
            findSCCval(w, ft)
            if (valHigh(w - minval) > sinkHigh)
              sinkHigh = valHigh(w - minval)
          } else if ((valDfs(w - minval) > sinkDfs) && (valComponent(w - minval) == 0)) {
            if (valDfs(w - minval) > sinkHigh)
              sinkHigh = valDfs(w - minval)
          }
        }
      }
    }

    for (i <- x.indices) {
      if (varMatch(i) == NONE) {
        if (varDfs(i) == 0) {
          findSCCvar(i, ft)
          if (varHigh(i) > sinkHigh)
            sinkHigh = varHigh(i)
        } else if ((varDfs(i) > sinkDfs) && (varComponent(i) == 0)) {
          if (varDfs(i) > sinkHigh)
            sinkHigh = varDfs(i)
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
        if (t == 0)
          varComponent(i) = component
        else if (t == 1)
          valComponent(i - minval) = component
        else
          sinkComponent = component
        if (t == 2)
          done = true
      }
    }
  }

  private def prune(valViol: Int): Unit = {
    if (valViol < viol.getMax - 1) {
      return
    }

    val varComponent_uf = new Array[Int](x.length)
    val valComponent_uf = new Array[Int](nbVals)
    
    findSCC(FlowType.UF)
    computeIsVarAlwaysMatched(FlowType.UF)
    for (v <- 0 until nbVals) {
      valComponent_uf(v) = valComponent(v)
    }
    for (k <- x.indices) {
      varComponent_uf(k) = varComponent(k)
    }

    findSCC(FlowType.OF)
    computeIsVarAlwaysMatched(FlowType.OF)

    if (valViol == viol.getMax - 1) {
      for (k <- x.indices) {
        if (varMatch_of(k) != NONE) {
          val mx = x(k).getMin
          val Mx = x(k).getMax
          for (w <- mx to Mx) {
            if (x(k).hasValue(w)) {
              if (varMatch_uf(k) != w && varMatch_of(k) != w) {
                if ((varComponent_uf(k) != valComponent_uf(w - minval) && (low(w - minval) > 0 || isVarAlwaysMatched_uf(k))) && 
                    (varComponent(k) != valComponent(w - minval) && (up(w - minval) > 0 || isVarAlwaysMatched_of(k)))) {
                  x(k).removeValue(w)
                }
              }
            }
          }
        }
      }
    } else if (valViol == viol.getMax) {
      // under-flow filtering
      for (k <- x.indices) {
        if (varMatch_of(k) != NONE) {
          val mx = x(k).getMin
          val Mx = x(k).getMax
          for (w <- mx to Mx) {
            if (x(k).hasValue(w)) {
              if (varMatch_uf(k) != w && varMatch_of(k) != w) {
                if (varComponent_uf(k) != valComponent_uf(w - minval) && (low(w - minval) > 0 || isVarAlwaysMatched_uf(k))) {
                  x(k).removeValue(w)
                }
              }
            }
          }
        }
      }
      // over-flow filtering
      for (k <- x.indices) {
        if (varMatch_of(k) != NONE) {
          val mx = x(k).getMin
          val Mx = x(k).getMax
          for (w <- mx to Mx) {
            if (x(k).hasValue(w)) {
              if (varMatch_of(k) != w) {
                if (varComponent(k) != valComponent(w - minval) && (up(w - minval) > 0 || isVarAlwaysMatched_of(k))) {
                  x(k).removeValue(w)
                }
              }
            }
          }
        }
      }
    }
  }
}
