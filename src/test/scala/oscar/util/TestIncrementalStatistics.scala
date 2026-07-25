package oscar.util

import oscar.cp.testUtils._

import oscar.cp.util.ArrayUtils
import oscar.util.IncrementalStatistics
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestIncrementalStatistics extends AnyFunSuite {

  def getAverage(vals: Array[Double]): Double = {
    val sum = ArrayUtils.sum(vals)
    sum / vals.length
  }

  def getVariance(vals: Array[Double]): Double = {
    var res = 0.0
    val avg = getAverage(vals)
    for (v <- vals) {
      res += (v - avg) * (v - avg)
    }
    res / vals.length
  }

  def round(a: Double): Int = {
    (1000 * a).toInt
  }

  test("test0") {
    val vals = Array[Double](-3.5)
    val stat = new IncrementalStatistics()
    for (v <- vals) {
      stat.addPoint(v)
    }
    assert(round(stat.average) == round(getAverage(vals)))
    assert(round(stat.variance) == round(getVariance(vals)))
  }

  test("test2") {
    val vals = Array[Double](-3, -2, 2, 3, 9, 10)
    val stat = new IncrementalStatistics()
    for (v <- vals) {
      stat.addPoint(v)
    }
    assert(round(stat.average) == round(getAverage(vals)))
    assert(round(stat.variance) == round(getVariance(vals)))
  }

}
