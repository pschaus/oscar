package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import oscar.cp.modeling.constraint.{absolute, minus, mul}

class Deviation(val x: Array[CPIntVar], val target: Int, val nd: CPIntVar) extends Constraint(x(0).store, "Deviation") {
  require(x.length >= 2)

  private val n: Int = x.length
  private val nx: Array[Int] = new Array[Int](n)
  private val nxmin: Array[Int] = new Array[Int](n)
  private val nxmax: Array[Int] = new Array[Int](n)
  private var nmaxsum: Int = 0
  private var nminsum: Int = 0

  private val overlaps: Array[Int] = new Array[Int](n)
  private val maximum: Array[Int] = new Array[Int](n)
  private val overlaps_sup: Array[Int] = new Array[Int](n)

  override def associatedVars(): Iterable[CPVar] = x.toSeq :+ nd

  override def setup(l: CPPropagStrength): Unit = {
    val devVar = new Array[CPIntVar](n)
    for (i <- x.indices) {
      devVar(i) = absolute(minus(mul(x(i), n), target))
    }
    x(0).store.post(new Sum(devVar, nd))

    for (i <- x.indices) {
      if (!x(i).isBound) {
        x(i).callPropagateWhenBoundsChange(this)
      }
    }
    nd.callPropagateWhenBoundsChange(this)
    propagate()
  }

  override def propagate(): Unit = {
    propagateSum()
    initData(false)
    computeMinDevAssignment()
    val delta_min = computeMinDev()
    nd.updateMin(delta_min)
    propagateBounds(delta_min)
  }

  private def propagateSum(): Unit = {
    var maxsum = 0
    var minsum = 0
    for (i <- x.indices) {
      maxsum += x(i).getMax
      minsum += x(i).getMin
    }
    for (i <- x.indices) {
      x(i).updateMax(target - (minsum - x(i).getMin))
      x(i).updateMin(target - (maxsum - x(i).getMax))
    }
  }

  private def initData(mirror: Boolean): Unit = {
    nmaxsum = 0
    nminsum = 0
    for (i <- x.indices) {
      nxmax(i) = n * (if (mirror) -x(i).getMin else x(i).getMax)
      nxmin(i) = n * (if (mirror) -x(i).getMax else x(i).getMin)
      nmaxsum += nxmax(i)
      nminsum += nxmin(i)
    }
  }

  private def computeMinDev(): Int = {
    var res = 0
    for (i <- 0 until n) {
      res += Math.abs(nx(i) - target)
    }
    res
  }

  private def computeMinDevAssignment(): Unit = {
    var sum = 0
    val sinf = if (target >= 0) target - target % n else target - (n - (-target % n))
    val ssup = sinf + n
    val s_nearest = if (ssup - target <= target - sinf) ssup else sinf
    var nbOverlaps = 0

    for (i <- 0 until n) {
      if (nxmin(i) >= target) {
        nx(i) = nxmin(i)
      } else if (nxmax(i) <= target) {
        nx(i) = nxmax(i)
      } else {
        nx(i) = s_nearest
        if (target % n != 0) {
          overlaps(nbOverlaps) = i
          nbOverlaps += 1
        }
      }
      sum += nx(i)
    }

    val delta = if (sum > n * target) -n else n

    var j = 0
    while (j < nbOverlaps && sum != n * target) {
      nx(overlaps(j)) += delta
      sum += delta
      j += 1
    }

    var i = 0
    while (i < n && sum != n * target) {
      val nxi = nx(i)
      if (sum < n * target) {
        nx(i) += n * target - sum
        nx(i) = Math.min(nx(i), nxmax(i))
      } else {
        nx(i) -= (sum - n * target)
        nx(i) = Math.max(nx(i), nxmin(i))
      }
      sum += nx(i) - nxi
      i += 1
    }
  }

  private def boundConsistentValue(j: Int, upper: Boolean): Int = {
    initData(false)
    computeMinDevAssignment()
    var mindev = computeMinDev()
    var currval = nx(j)
    while (mindev <= nd.getMax) {
      currval += (if (upper) n else -n)
      nmaxsum = 0
      nminsum = 0
      for (i <- x.indices) {
        nxmax(i) = if (j == i) currval else n * x(i).getMax
        nxmin(i) = if (j == i) currval else n * x(i).getMin
        nmaxsum += nxmax(i)
        nminsum += nxmin(i)
      }
      if (n * target >= nminsum && n * target <= nmaxsum) {
        computeMinDevAssignment()
        mindev = computeMinDev()
      } else {
        mindev = nd.getMax + 1 // break equivalent
      }
    }
    if (upper) (currval - n) / n
    else (currval + n) / n
  }

  private def propagateBoundsShaving(): Unit = {
    for (i <- 0 until n) {
      if (!x(i).isBound) {
        val max = boundConsistentValue(i, true)
        x(i).updateMax(max)
        val min = boundConsistentValue(i, false)
        x(i).updateMin(min)
      }
    }
  }

  private def propagateBounds(min_delta: Int): Unit = {
    propagateBounds(min_delta, true)
    propagateBounds(min_delta, false)
  }

  private def divFloor(val_ : Int, div: Int): Int = {
    assert(div > 0)
    var res = val_ / div
    if (val_ < 0 && val_ % div != 0) res -= 1
    res
  }

  private def propagateBounds(min_delta: Int, upperBounds: Boolean): Unit = {
    initData(!upperBounds)
    val sValue = if (!upperBounds) -this.target else this.target

    assert(n * sValue >= nminsum && n * sValue <= nmaxsum)

    var sum = 0
    val sinf = if (sValue >= 0) sValue - sValue % n else sValue - (n - (-sValue % n))
    val ssup = sinf + n
    val s_nearest = if (ssup - sValue <= sValue - sinf) ssup else sinf
    var nbOverlaps = 0

    assert(sinf % n == 0 && sinf <= sValue && (sValue - sinf < n))

    for (i <- 0 until n) {
      if (nxmin(i) >= sValue) {
        nx(i) = nxmin(i)
      } else if (nxmax(i) <= sValue) {
        nx(i) = nxmax(i)
      } else {
        nx(i) = s_nearest
        if (s_nearest != sValue) {
          overlaps(nbOverlaps) = i
          nbOverlaps += 1
        }
      }
      sum += nx(i)
    }

    if (sum == n * sValue) {
      var nboverlapssup = 0
      if (s_nearest == ssup) nboverlapssup = nbOverlaps
      for (i <- 0 until n) {
        maximum(i) = nx(i)
        if (nxmin(i) < sValue && nxmax(i) > sValue && s_nearest == ssup && sValue % n != 0) {
          overlaps_sup(i) = nboverlapssup - 1
        } else {
          overlaps_sup(i) = nboverlapssup
        }
      }
    } else {
      if ((sum > n * sValue && s_nearest == ssup) || (sum < n * sValue && s_nearest == sinf)) {
        val delta = if (sum > n * sValue) -n else n
        var j = 0
        while (j < nbOverlaps && sum != n * sValue) {
          nx(overlaps(j)) += delta
          sum += delta
          j += 1
        }
      }
      var nboverlapssup = 0
      for (i <- 0 until n) {
        if (nx(i) > nxmin(i) && nx(i) == ssup && sValue % n != 0) nboverlapssup += 1
      }
      if (sum == n * sValue) {
        for (i <- 0 until n) {
          val overlapi = nxmin(i) < sValue && nxmax(i) > sValue
          if (overlapi && nboverlapssup > 0) {
            maximum(i) = ssup
            overlaps_sup(i) = nboverlapssup - 1
          } else {
            maximum(i) = nx(i)
            overlaps_sup(i) = nboverlapssup
          }
        }
      } else if (sum > n * sValue) {
        for (i <- 0 until n) {
          maximum(i) = nx(i)
          overlaps_sup(i) = 0
        }
      } else {
        for (i <- 0 until n) {
          val overlapi = nxmin(i) < sValue && nxmax(i) > sValue && nboverlapssup > 0
          if (overlapi) overlaps_sup(i) = nboverlapssup - 1
          else overlaps_sup(i) = nboverlapssup

          if (nx(i) < nxmax(i)) maximum(i) = nx(i) + (n * sValue - sum)
          else maximum(i) = nx(i)
        }
      }
    }

    val increase_down_up = (ssup - sValue) - (sValue - sinf)
    val increase_up_down = -increase_down_up

    for (i <- 0 until n) {
      if (!x(i).isBound && maximum(i) < nxmax(i)) {
        var maxval = maximum(i)
        var deltamin = min_delta

        if (overlaps_sup(i) > 0 && (deltamin + overlaps_sup(i) * (n + increase_up_down) >= nd.getMax)) {
          val delta = nd.getMax - deltamin
          maxval += (n * delta) / (n + increase_up_down)
          val bound = divFloor(maxval, n)
          pruneBound(x(i), bound, !upperBounds)
        } else {
          if (maxval == sinf && sinf < sValue) {
            assert(overlaps_sup(i) == 0)
            deltamin += n + increase_down_up
            if (deltamin > nd.getMax) {
              assert(maxval % n == 0)
              val bound = maxval / n
              pruneBound(x(i), bound, !upperBounds)
            } else {
              maxval += n
              deltamin += overlaps_sup(i) * (n + increase_up_down)
              maxval += n * overlaps_sup(i)
              val delta = nd.getMax - deltamin
              maxval += delta / 2
              val bound = divFloor(maxval, n)
              pruneBound(x(i), bound, !upperBounds)
            }
          } else {
            deltamin += overlaps_sup(i) * (n + increase_up_down)
            maxval += n * overlaps_sup(i)
            val delta = nd.getMax - deltamin
            maxval += delta / 2
            val bound = divFloor(maxval, n)
            pruneBound(x(i), bound, !upperBounds)
          }
        }
      }
    }
  }

  private def pruneBound(xVar: CPIntVar, bound: Int, mirror: Boolean): Unit = {
    if (!mirror) {
      xVar.updateMax(bound)
    } else {
      xVar.updateMin(-bound)
    }
  }
}
