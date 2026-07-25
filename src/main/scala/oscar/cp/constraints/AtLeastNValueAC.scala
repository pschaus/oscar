package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

object AtLeastNValueAC {
  private val NONE = -Int.MaxValue
}

class AtLeastNValueAC(val x: Array[CPIntVar], val nValueVar: CPIntVar) extends Constraint(x(0).store, "AtLeastNValueAC") {

  def this(x: Array[CPIntVar], nval: CPIntVar, dontPostFWC: Boolean) = this(x, nval)

  private var posted: Boolean = false

  private var matchArray: Array[Int] = _
  private var varSeen: Array[Int] = _

  private var min: Int = 0
  private var max: Int = 0
  private var valSize: Int = 0
  private var valMatch: Array[Int] = _
  private var sizeMatching: Int = 0
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

  private var stack: Array[Int] = _
  private var typeArray: Array[Int] = _
  private var top: Int = 0

  private var domArray: Array[Array[Int]] = _
  private var unBoundIdx: Array[Int] = _
  private var nUnBound: Int = 0

  priorityL2 = CPStore.MaxPriorityL2 - 3
  idempotent = true

  override def associatedVars(): Iterable[CPVar] = x.toSeq :+ nValueVar

  override def setup(l: CPPropagStrength): Unit = {
    posted = true

    if (nValueVar.getMin < x.length) {
      s.post(new AtLeastNValueFWC(x, nValueVar))
    } else {
      s.post(new AllDiffFWC(x))
    }

    findValueRange()

    unBoundIdx = new Array[Int](x.length)
    domArray = Array.ofDim[Int](x.length, valSize)

    initMatching()
    findInitialMatching()
    
    val sizeMatchingVal = findMaximalMatching()
    nValueVar.updateMax(sizeMatchingVal)
    
    if (nValueVar.getMin > sizeMatchingVal) {
      throw oscar.algo.Inconsistency
    }

    allocateSCC()
    propagate()

    for (k <- x.indices) {
      if (!x(k).isBound) {
        x(k).callPropagateWhenDomainChanges(this)
      }
    }
    
    if (!nValueVar.isBound) {
      nValueVar.callPropagateWhenBoundsChange(this)
    }
  }

  def hasValInBestAssignment(i: Int): Boolean = {
    posted && i >= 0 && i < x.length && matchArray(i) != AtLeastNValueAC.NONE
  }

  def getValInBestAssignment(i: Int): Int = {
    if (hasValInBestAssignment(i)) {
      matchArray(i)
    } else if (i >= 0 && i < x.length && posted) {
      x(i).getMin
    } else {
      Int.MinValue
    }
  }

  override def propagate(): Unit = {
    nUnBound = 0
    for (k <- x.indices) {
      if (matchArray(k) != AtLeastNValueAC.NONE) {
        if (!x(k).hasValue(matchArray(k))) {
          valMatch(matchArray(k) - min) = -1
          matchArray(k) = AtLeastNValueAC.NONE
          sizeMatching -= 1
        }
      }
      if (!x(k).isBound) {
        unBoundIdx(nUnBound) = k
        nUnBound += 1
      }
    }

    val maxMatching = findMaximalMatching()
    nValueVar.updateMax(maxMatching)
    if (nValueVar.min > maxMatching) {
      throw oscar.algo.Inconsistency
    } else if (nValueVar.min == maxMatching) {
      prune(maxMatching)
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
    valMatch = Array.fill(valSize)(-1)
  }

  private def initMatching(): Unit = {
    magic = 0
    matchArray = Array.fill(x.length)(AtLeastNValueAC.NONE)
    varSeen = new Array[Int](x.length)
    valSeen = new Array[Int](valSize)
  }

  private def findInitialMatching(): Unit = {
    sizeMatching = 0
    for (k <- x.indices) {
      val mx = x(k).getMin
      val Mx = x(k).getMax
      var i = mx
      var found = false
      while (i <= Mx && !found) {
        if (valMatch(i - min) < 0) {
          if (x(k).hasValue(i)) {
            matchArray(k) = i
            valMatch(i - min) = k
            sizeMatching += 1
            found = true
          }
        }
        i += 1
      }
    }
  }

  private def findMaximalMatching(): Int = {
    if (sizeMatching < x.length) {
      for (k <- x.indices) {
        if (matchArray(k) == AtLeastNValueAC.NONE) {
          magic += 1
          if (findAlternatingPath(k)) {
            sizeMatching += 1
          }
        }
      }
    }
    sizeMatching
  }

  private def findAlternatingPath(i: Int): Boolean = {
    if (varSeen(i) != magic) {
      varSeen(i) = magic
      val mx = x(i).getMin
      val Mx = x(i).getMax
      for (v <- mx to Mx) {
        if (matchArray(i) != v) {
          if (x(i).hasValue(v)) {
            if (findAlternatingPathValue(v)) {
              matchArray(i) = v
              valMatch(v - min) = i
              return true
            }
          }
        }
      }
    }
    false
  }

  private def findAlternatingPathValue(v: Int): Boolean = {
    if (valSeen(v - min) != magic) {
      valSeen(v - min) = magic
      if (valMatch(v - min) == -1) return true
      if (findAlternatingPath(valMatch(v - min))) return true
    }
    false
  }

  private def allocateSCC(): Unit = {
    varComponent = new Array[Int](x.length * 2)
    varDfs = new Array[Int](x.length * 2)
    varHigh = new Array[Int](x.length * 2)
    valComponent = new Array[Int](valSize)
    valDfs = new Array[Int](valSize * 2)
    valHigh = new Array[Int](valSize * 2)
    stack = new Array[Int]((x.length + valSize) * 2)
    typeArray = new Array[Int]((x.length + valSize) * 2)
  }

  private def initSCC(): Unit = {
    for (k <- x.indices) {
      varComponent(k) = 0
      varDfs(k) = 0
      varHigh(k) = 0
    }
    for (v <- min to max) {
      valComponent(v - min) = 0
      valDfs(v - min) = 0
      valHigh(v - min) = 0
    }
    top = 0
    dfs = x.length + valSize
    component = 0
  }

  private def findSCC(): Unit = {
    initSCC()
    for (k <- x.indices) {
      if (varDfs(k) == 0) findSCCvar(k)
    }
  }

  private def findSCCvar(k: Int): Unit = {
    varDfs(k) = dfs
    dfs -= 1
    varHigh(k) = varDfs(k)
    stack(top) = k
    typeArray(top) = 0
    top += 1

    val nVal = x(k).fillArray(domArray(k))
    for (i <- 0 until nVal) {
      val w = domArray(k)(i)
      if (matchArray(k) != w) {
        if (valDfs(w - min) == 0) {
          findSCCval(w)
          if (valHigh(w - min) > varHigh(k)) varHigh(k) = valHigh(w - min)
        } else if (valDfs(w - min) > varDfs(k) && valComponent(w - min) == 0) {
          if (valDfs(w - min) > varHigh(k)) varHigh(k) = valDfs(w - min)
        }
      }
    }

    if (matchArray(k) != AtLeastNValueAC.NONE) {
      for (i <- x.indices) {
        if (matchArray(i) == AtLeastNValueAC.NONE) {
          if (varDfs(i) == 0) {
            findSCCvar(i)
            if (varHigh(i) > varHigh(k)) varHigh(k) = varHigh(i)
          } else if (varDfs(i) > varDfs(k) && varComponent(i) == 0) {
            if (varDfs(i) > varHigh(k)) varHigh(k) = varDfs(i)
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
        else valComponent(v - min) = component
        if (t == 0 && v == k) breakLoop = true
      }
    }
  }

  private def findSCCval(k: Int): Unit = {
    valDfs(k - min) = dfs
    dfs -= 1
    valHigh(k - min) = valDfs(k - min)
    stack(top) = k
    typeArray(top) = 1
    top += 1

    if (valMatch(k - min) != -1) {
      val w = valMatch(k - min)
      if (varDfs(w) == 0) {
        findSCCvar(w)
        if (varHigh(w) > valHigh(k - min)) valHigh(k - min) = varHigh(w)
      } else if (varDfs(w) > valDfs(k - min) && varComponent(w) == 0) {
        if (varDfs(w) > valHigh(k - min)) valHigh(k - min) = varDfs(w)
      }
    } else {
      for (i <- x.indices) {
        if (matchArray(i) != AtLeastNValueAC.NONE) {
          val w = matchArray(i)
          if (valDfs(w - min) == 0) {
            findSCCval(w)
            if (valHigh(w - min) > valHigh(k - min)) valHigh(k - min) = valHigh(w - min)
          } else if (valDfs(w - min) > valDfs(k - min) && valComponent(w - min) == 0) {
            if (valDfs(w - min) > valHigh(k - min)) valHigh(k - min) = valDfs(w - min)
          }
        } else {
          if (varDfs(i) == 0) {
            findSCCvar(i)
            if (varHigh(i) > valHigh(k - min)) valHigh(k - min) = varHigh(i)
          } else if (varDfs(i) > valDfs(k - min) && varComponent(i) == 0) {
            if (varDfs(i) > valHigh(k - min)) valHigh(k - min) = varDfs(i)
          }
        }
      }
    }

    if (valHigh(k - min) == valDfs(k - min)) {
      component += 1
      var breakLoop = false
      while (!breakLoop) {
        top -= 1
        val v = stack(top)
        val t = typeArray(top)
        if (t == 0) varComponent(v) = component
        else valComponent(v - min) = component
        if (t == 1 && v == k) breakLoop = true
      }
    }
  }

  private def prune(sizeMatching: Int): Unit = {
    findSCC()
    for (j <- 0 until nUnBound) {
      val k = unBoundIdx(j)
      val nVal = x(k).fillArray(domArray(k))
      for (i <- 0 until nVal) {
        val w = domArray(k)(i)
        if (matchArray(k) != w && varComponent(k) != valComponent(w - min)) {
          x(k).removeValue(w)
        }
      }
    }
  }
}
