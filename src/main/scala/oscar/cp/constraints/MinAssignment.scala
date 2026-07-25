package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPIntVarAdaptable
import oscar.cp.core.variables.CPVar
import java.util.Arrays

/**
 * Minimum Assignment (or weighted matching)
 * @author Pierre Schaus pschaus@gmail.com
 */
class MinAssignment(xIn: Array[CPIntVar], weightMat: Array[Array[Int]], var cost: CPIntVar) 
  extends Constraint(xIn(0).store, "minassingmentscala") {

  if (weightMat.length != xIn.length) throw new IllegalArgumentException("MinAssignment: dim of x and weights must match")

  private val n: Int = weightMat(0).length
  private val x: Array[CPIntVar] = new Array[CPIntVar](n)
  
  for (i <- 0 until n) {
    if (i < xIn.length) x(i) = xIn(i)
    else x(i) = new CPIntVarAdaptable(s, 0, n - 1, true, "x_minAssignment" + i)
  }

  priorityL2 = CPStore.MaxPriorityL2 - 3

  private var weights: Array[Array[Int]] = _

  private var lc: Array[ReversibleInt] = _
  private var lr: Array[ReversibleInt] = _
  private var valc: Array[ReversibleInt] = _ // row assigned to the column
  private var valr: Array[ReversibleInt] = _ // column assign to the row
  private var w: Array[Array[ReversibleInt]] = _

  private var markc: Array[Boolean] = _
  private var markr: Array[Boolean] = _
  private var pi: Array[Int] = _
  private var pathRowOfColumn: Array[Int] = _

  private var M: Int = 0

  private val NONE: Int = Int.MaxValue

  private val values: Array[Int] = new Array[Int](n) // used to fill in domain
  private val sp: Array[Array[Int]] = Array.ofDim[Int](n, n) // shortest path dist in residual graph
  private val distance: Array[Int] = new Array[Int](2 * n) // used by bellman-ford

  private val isValueBound: Array[Boolean] = new Array[Boolean](n)
  private val unboundVars: Array[Int] = new Array[Int](n)
  private val unboundVals: Array[Int] = new Array[Int](n)
  
  private val valr_ : Array[Int] = new Array[Int](n)
  private val valc_ : Array[Int] = new Array[Int](n)
  private var nUnboundVars: Int = 0
  private var nUnboundVals: Int = 0

  private val delta: Array[DeltaIntVar] = new Array[DeltaIntVar](n)

  private var exactReducedCosts: Boolean = false

  initWeightMatrix(weightMat)

  override def associatedVars(): Iterable[CPVar] = x.toSeq

  private def initWeightMatrix(weightMat: Array[Array[Int]]): Unit = {
    this.weights = Array.ofDim[Int](x.length, x.length)
    for (i <- x.indices) {
      if (i < weightMat.length && weightMat(i).length != x.length) {
        throw new RuntimeException(s"weightMat should be a square ${x.length}x${x.length} matrix")
      }
      if (i < weightMat.length) {
        for (j <- weightMat(i).indices) {
          weights(i)(j) = weightMat(i)(j)
        }
      }
    }
  }

  private def initTrails(): Unit = {
    M = Int.MinValue
    w = Array.ofDim[ReversibleInt](x.length, x.length)
    for (i <- x.indices) {
      for (j <- x.indices) {
        w(i)(j) = new ReversibleInt(s, 0)
        w(i)(j).setValue(weights(i)(j))
        M = math.max(M, weights(i)(j))
      }
    }
    M += 1

    for (i <- x.indices) {
      for (j <- x.indices) {
        if (!x(i).hasValue(j)) {
          w(i)(j).setValue(M)
        }
      }
    }

    lc = new Array[ReversibleInt](x.length)
    lr = new Array[ReversibleInt](x.length)
    valc = new Array[ReversibleInt](x.length)
    valr = new Array[ReversibleInt](x.length)

    for (i <- lc.indices) {
      lc(i) = new ReversibleInt(s, 0)
      lc(i).setValue(0)

      lr(i) = new ReversibleInt(s, 0)
      lr(i).setValue(0)

      valc(i) = new ReversibleInt(s, NONE)
      valc(i).setValue(NONE)
      valr(i) = new ReversibleInt(s, NONE)
      valr(i).setValue(NONE)
    }
  }

  override def setup(l: CPPropagStrength): Unit = {
    initTrails()

    markc = new Array[Boolean](x.length)
    markr = new Array[Boolean](x.length)
    pi = new Array[Int](x.length)
    pathRowOfColumn = new Array[Int](x.length)

    reduceMatrix()
    initAssignment()
    findMinimalAssignment()
    updateUnBounds()
    prune()

    for (i <- x.indices) {
      if (!x(i).isBound) {
        delta(i) = x(i).callPropagateOnChangesWithDelta(this)
      }
    }
    if (!cost.isBound) {
      cost.callPropagateWhenBoundsChange(this)
    }

    if (l == CPPropagStrength.Strong) {
      exactReducedCosts = true
    }
  }

  private def reduceMatrix(): Unit = {
    for (j <- x.indices) {
      var m = Int.MaxValue
      for (i <- x.indices) {
        val value = w(i)(j).getValue() - lc(j).getValue() - lr(i).getValue()
        m = math.min(m, value)
      }
      lc(j).setValue(lc(j).getValue() + m)
    }
    for (i <- x.indices) {
      var m = Int.MaxValue
      for (j <- x.indices) {
        val value = w(i)(j).getValue() - lc(j).getValue() - lr(i).getValue()
        m = math.min(m, value)
      }
      lr(i).setValue(lr(i).getValue() + m)
    }
  }

  private def initAssignment(): Unit = {
    for (i <- x.indices) {
      var done = false
      var j = 0
      while (j < x.length && !done) {
        if (!colAssigned(j) && w(i)(j).getValue() == 0) {
          assignRow(i, j)
          done = true
        }
        j += 1
      }
    }
  }

  private def findMinimalAssignment(): Unit = {
    for (i <- x.indices) {
      if (!rowAssigned(i)) {
        applyAssignment(i, findPath(i))
      }
    }
  }

  private def findPath(i: Int): Int = {
    for (j <- x.indices) {
      markr(j) = false
      markc(j) = false
    }
    markr(i) = true
    for (c <- x.indices) {
      pi(c) = w(i)(c).getValue() - lc(c).getValue() - lr(i).getValue()
      pathRowOfColumn(c) = i
    }
    
    var done = false
    var resultCol = -1
    
    while (!done) {
      var col = -1
      var row = -1
      // find an arc i->j with zero reduced cost
      var cIdx = 0
      while (cIdx < x.length && col == -1) {
        if (!markc(cIdx) && pi(cIdx) == 0) {
          col = cIdx
        }
        cIdx += 1
      }
      
      // dual step
      if (col < 0) {
        // no zero reduced cost arc found so we reduce to introduce new zero-cost edge
        var m = Int.MaxValue
        for (c <- x.indices) {
          if (!markc(c)) {
            if (pi(c) < m) {
              m = pi(c)
              col = c
            }
          }
        }
        for (k <- x.indices) {
          if (markr(k)) lr(k).setValue(lr(k).getValue() + m)
          if (markc(k)) lc(k).setValue(lc(k).getValue() - m)
          else pi(k) -= m
        }
      }
      
      assert(pi(col) == 0)

      // primal step
      if (colAssigned(col)) {
        row = valc(col).getValue()
        markr(row) = true
        markc(col) = true
      } else {
        // augmenting path
        resultCol = col
        done = true
      }

      // update the minimum dual values pi and the (future) path if a column is selected
      if (!done) {
        for (c <- x.indices) {
          if (!markc(c)) {
            val m = w(row)(c).getValue() - lc(c).getValue() - lr(row).getValue()
            if (m < pi(c)) {
              pi(c) = m
              pathRowOfColumn(c) = row
            }
          }
        }
      }
    }
    resultCol
  }

  private def applyAssignment(r: Int, cInit: Int): Unit = {
    var c = cInit
    var row = -1
    while ({ {
      row = pathRowOfColumn(c)
      valc(c).setValue(row)
      val col = valr(row).getValue()
      valr(row).setValue(c)
      c = col
    } ; row != r}) ()
  }

  private def rowAssigned(i: Int): Boolean = {
    valr(i).getValue() != NONE
  }

  private def colAssigned(j: Int): Boolean = {
    valc(j).getValue() != NONE
  }

  private def assignRow(i: Int, j: Int): Unit = {
    valr(i).setValue(j)
    valc(j).setValue(i)
  }

  override def propagate(): Unit = {
    // treat the deltas
    for (r <- x.indices) {
      if (delta(r) != null) { // if variable was not already bound at posting
        val nRemoved = delta(r).fillArray(values)
        for (j <- 0 until nRemoved) {
          val c = values(j)
          w(r)(c).setValue(M)
          if (valr(r).getValue() == c) {
            valr(r).setValue(NONE)
            valc(c).setValue(NONE)
          }
        }
      }
    }

    var valid = true
    var i = 0
    while (i < x.length && valid) {
      if (!rowAssigned(i)) {
        valid = false
      }
      i += 1
    }
    if (!valid) {
      findMinimalAssignment()
    }
    for (i <- x.indices) {
      valc_(i) = valc(i).getValue()
      valr_(i) = valr(i).getValue()
    }

    prune()
  }

  private def updateUnBounds(): Unit = {
    Arrays.fill(isValueBound, false)
    nUnboundVars = 0
    nUnboundVals = 0
    for (i <- x.indices) {
      if (!x(i).isBound) {
        unboundVars(nUnboundVars) = i
        nUnboundVars += 1
      } else {
        isValueBound(x(i).min) = true
      }
    }
    for (i <- x.indices) {
      if (!isValueBound(i)) {
        unboundVals(nUnboundVals) = i
        nUnboundVals += 1
      }
    }
  }

  def updateAllPairsShortestPathFloydWarshall(): Unit = {
    for (i <- x.indices) {
      Arrays.fill(sp(i), M)
    }

    val dist = Array.ofDim[Int](x.length + x.length, x.length + x.length)
    for (i <- 0 until x.length + x.length) {
      for (j <- 0 until x.length + x.length) {
        if (i == j) {
          dist(i)(j) = 0
        } else if (i < x.length && j >= x.length && valr(i).getValue() != j - x.length) {
          dist(i)(j) = w(i)(j - x.length).getValue()
        } else if (i >= x.length && j < x.length && valr(j).getValue() == i - x.length) {
          dist(i)(j) = -w(j)(i - x.length).getValue()
        } else {
          dist(i)(j) = M
        }
      }
    }
    for (k <- 0 until x.length + x.length) {
      for (i <- 0 until x.length + x.length) {
        for (j <- 0 until x.length + x.length) {
          if (dist(i)(j) > dist(i)(k) + dist(k)(j)) {
            dist(i)(j) = dist(i)(k) + dist(k)(j)
          }
        }
      }
    }

    for (s_idx <- 0 until nUnboundVars) {
      val i = unboundVars(s_idx)
      for (ind <- 0 until nUnboundVals) {
        val j = unboundVals(ind)
        sp(i)(j) = dist(i)(j + x.length)
      }
    }
  }

  def updateAllPairsShortestPathBellmanFord(): Unit = {
    for (i <- x.indices) {
      Arrays.fill(sp(i), M)
    }

    for (s_idx <- 0 until nUnboundVars) {
      val source = unboundVars(s_idx)
      Arrays.fill(distance, M)
      distance(source) = 0
      
      for (k <- 0 until (nUnboundVars + nUnboundVals) - 1) {
        // edges from left to right (variable to values)
        for (l <- 0 until nUnboundVars) {
          val i = unboundVars(l)
          val nVals = x(i).fillArray(values)
          for (ind <- 0 until nVals) {
            val j = values(ind)
            if (valr_(i) != j) {
              // there is an edge i->j in residual graph
              if (distance(i) + weights(i)(j) < distance(x.length + j)) {
                distance(x.length + j) = distance(i) + weights(i)(j)
              }
            }
          }
        }
        // edges from right to left (matched values to variables)
        for (l <- 0 until nUnboundVals) {
          val i = unboundVals(l)
          val j = valc_(i)
          if (distance(i + x.length) - weights(j)(i) < distance(j)) {
            distance(j) = distance(i + x.length) - weights(j)(i)
          }
        }
      }
      for (l <- 0 until nUnboundVals) {
        val c = unboundVals(l)
        sp(source)(c) = distance(c + x.length)
      }
    }
  }

  private def prune(): Unit = {
    var sum = 0
    for (i <- x.indices) {
      sum += lc(i).getValue()
      sum += lr(i).getValue()
    }
    cost.updateMin(sum)
    val slack = cost.getMax - sum
    pruneLPReducedCosts(slack)
    if (exactReducedCosts) {
      pruneExactReducedCosts(slack)
    }
  }

  def pruneLPReducedCosts(slack: Int): Unit = {
    for (s_idx <- 0 until nUnboundVars) {
      val i = unboundVars(s_idx)
      val nVals = x(i).fillArray(values)
      for (ind <- 0 until nVals) {
        val j = values(ind)
        if (valr_(i) != j) {
          val value = w(i)(j).getValue()
          val m = value - lc(j).getValue() - lr(i).getValue()
          if (m > slack) {
            w(i)(j).setValue(M)
            x(i).removeValue(j)
          }
        }
      }
    }
  }

  def pruneExactReducedCosts(slack: Int): Unit = {
    updateAllPairsShortestPathFloydWarshall()

    for (s_idx <- 0 until nUnboundVars) {
      val i = unboundVars(s_idx)
      val nVals = x(i).fillArray(values)
      for (ind <- 0 until nVals) {
        val j = values(ind)
        if (valr_(i) != j) {
          val i_p = valc_(j)
          val j_p = valr_(i)

          val m = (weights(i)(j) + sp(i_p)(j_p)) - (weights(i)(j_p) + weights(i_p)(j))

          if (m > slack) {
            w(i)(j).setValue(M)
            x(i).removeValue(j)
          }
        }
      }
    }
  }
}
