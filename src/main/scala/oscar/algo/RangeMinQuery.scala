/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.algo



/**
 * @author Pierre Schaus pschaus@gmail.com
 * Range minimum query <O(n.log(n)),O(1)>
 */
class RangeMinQuery(val values: Array[Int]) {

    val n = values.length;
    val logTable = Array.fill(n+1)(0)
    for (i <- 2 to n)
      logTable(i) = logTable(i >> 1) + 1
    val rmq = Array.fill(logTable(n) + 1)(Array.fill(n)(0))

    for (i <- 0 until n)
      rmq(0)(i) = i;
    var k = 1
    while ((1 << k) < n) {
      var i = 0
      while ((i + (1 << k)) <= n) {
        val x = rmq(k - 1)(i)
        val y = rmq(k - 1)(i + (1 << k - 1))
        rmq(k)(i) = if (values(x) <= values(y))  x else y
        i += 1
      }
      k += 1
    }
  
  /**
   * @param 0 <= a < b < values.length
   * @return values(rmq(i,j)) = min(values(a),values(a+1),...,values(b))
   */
  def apply(a: Int, b: Int): Int = {
    val i = a min b
    val j = a max b
    val k = logTable(j - i)
    val x = rmq(k)(i)
    val y = rmq(k)(j - (1 << k) + 1)
    if (values(x) <= values(y)) x else y
  }

}