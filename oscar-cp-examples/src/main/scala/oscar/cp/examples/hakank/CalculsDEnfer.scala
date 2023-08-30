/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.examples.hakank

import oscar.cp._
import scala.io.Source._
import scala.math._
/*
  Calcul d'enfer puzzle in Oscar.
  Problem from Jianyang Zhou "The Manual of NCL version 1.2", page 33
  http://citeseer.ist.psu.edu/161721.html
  The solution in the manual is:
  """
  a = -16, b = -14, c = -13, d = -12, e = -10,
  f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
  l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
  r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
  x = 7, y = -2, z = -5.
  max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
  """
  This model happens to yield this solution as well:
  X:  -16 -14 -13 -12 -10 4 13 -1 -3 -11 -9 16 -8 11 0 -6 -4 15 2 9 -15 14 -7 7 -2 -5
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object CalculsDEnfer extends CPModel with App {
  // returns the maximal element in t
  def my_max(t: Array[CPIntVar]): CPIntVar = {
    val cp = t(0).store
    val mmax = CPIntVar(t(0).min to t(0).max)
    for (i <- 0 to t.length - 1) {
      post(mmax >= t(i))
    }
    mmax
  }
  // data
  val N = 26
  val RANGE = -100 to 100
  //
  // variables
  // 
  val X = Array.fill(N)(CPIntVar(RANGE))
  val Array(a, b, c, d, e, f, g, h, i, j, k, l, m) = X slice (0, 13)
  val Array(n, o, p, q, r, s, t, u, v, w, x, y, z) = X slice (13, 26)
  val x_max = my_max(for (I <- 0 until N) yield X(I).abs) // this is faster
  // val x_max = maximum(for(I <- 0 until N) yield X(I).abs())
  //
  // constraints
  //
  var numSols = 0
  minimize(x_max)
  // solveAll 
  add(allDifferent(X), Strong)
  add(z + e + r + o === 0);
  add(o + n + e === 1);
  add(t + w + o === 2);
  add(t + h + r + e + e === 3);
  add(f + o + u + r === 4);
  add(f + i + v + e === 5);
  add(s + i + x === 6);
  add(s + e + v + e + n === 7);
  add(e + i + g + h + t === 8);
  add(n + i + n + e === 9);
  add(t + e + n === 10);
  add(e + l + e + v + e + n === 11);
  add(t + w + e + l + f === 12); // Sic!
  // for solveAll
  // add(x_max == 16);
  search {
    binaryMaxDegree(X ++ Array(x_max))
  }
  onSolution {
    println("\nSolution:");
    println("x_max: " + x_max)
    println("X: " + X.mkString(""))
  }
  println(start())
}
