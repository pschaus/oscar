package oscar.cp.util

object NumberUtils {

  def isPerfectSquare(n: Int): Boolean = {
    if (n < 0) return false
    (n & 0x3F) match {
      case 0x00 | 0x01 | 0x04 | 0x09 | 0x10 | 0x11 |
           0x19 | 0x21 | 0x24 | 0x29 | 0x31 | 0x39 =>
        val sqrt = if (n < 410881L) {
          var i = 0
          var x2 = 0f
          var y = 0f
          x2 = n * 0.5f
          y = n.toFloat
          i = java.lang.Float.floatToRawIntBits(y)
          i = 0x5f3759df - (i >> 1)
          y = java.lang.Float.intBitsToFloat(i)
          y = y * (1.5f - (x2 * y * y))
          (1.0f / y).toInt
        } else {
          Math.sqrt(n.toDouble).toInt
        }
        sqrt * sqrt == n
      case _ => false
    }
  }

  def negativeProduct(v1: Int, v2: Int): Boolean = {
    ((v2 < 0) ^ (v1 < 0)) && (v1 != 0 && v2 != 0)
  }

  def positiveProduct(v1: Int, v2: Int): Boolean = {
    (v2 > 0 && v1 > 0) || (v1 < 0 && v2 < 0)
  }

  def ceilDiv(v1: Int, v2: Int): Int = {
    v1 / v2 + (if (v1 % v2 != 0 && positiveProduct(v1, v2)) 1 else 0)
  }

  def floorDiv(v1: Int, v2: Int): Int = {
    v1 / v2 - (if (v1 % v2 != 0 && negativeProduct(v1, v2)) 1 else 0)
  }

  @scala.annotation.varargs
  def minCeilDiv(c: Int, vals: Int*): Int = {
    var res = Int.MaxValue
    for (v <- vals) {
      val tmp = ceilDiv(c, v)
      if (tmp < res) res = tmp
    }
    res
  }

  @scala.annotation.varargs
  def maxFloorDiv(c: Int, vals: Int*): Int = {
    var res = Int.MinValue
    for (v <- vals) {
      val tmp = floorDiv(c, v)
      if (tmp > res) res = tmp
    }
    res
  }

  def overFlowMul(a: Int, b: Int): Boolean = {
    if (a == 0 || b == 0) return false
    (a * b) / b != a
  }

  def safeMul(a: Int, b: Int): Int = {
    if (overFlowMul(a, b)) {
      println(s"warning: overflow multiplying ${a}*${b}")
      if ((a > 0 && b > 0) || (a < 0 && b < 0)) {
        Int.MaxValue
      } else {
        Int.MinValue
      }
    } else {
      a * b
    }
  }
}
