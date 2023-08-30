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
package oscar.cp.examples.hakank

import oscar.cp._
import scala.math.pow
/**
 *
 * de Bruijn sequences in Oscar.
 *
 * Implementation of de Bruijn sequences , both 'classical' and 'arbitrary'.
 * The 'arbitrary' version is when the length of the sequence
 * (variable m here) is < base**n.
 *
 *
 * Compare with the the web based programs:
 *    http://www.hakank.org/comb/debruijn.cgi
 *    http://www.hakank.org/comb/debruijn_arb.cgi
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object DeBruijn extends CPModel with App {
  // channeling between IntVar array t <=> IntVar s
  def toNum(t: Array[CPIntVar], base: Int = 10) = sum(
    Array.tabulate(t.length)(i => t(i) * pow(base, t.length - i - 1).toInt))
  // data
  val base = if (args.length > 0) args(0).toInt else 3;
  val n = if (args.length > 1) args(1).toInt else 3;
  val m = if (args.length > 2) args(2).toInt else pow(base, n).toInt;
  println("base: " + base + " n: " + n + " m: " + m)
  //
  // variables
  //
  // (Improvements from the original version suggested by Pierre Schaus.)
  val x = Array.fill(m)(CPIntVar(0 to pow(base, n).toInt - 1))
  val binary = Array.fill(m, n)(CPIntVar(0 to base - 1))
  val bin_code = Array.fill(m)(CPIntVar(0 to base - 1))
  val gccv = Array.tabulate(base)(i => (i, CPIntVar(0 to m)))
  //
  // constraints
  //
  var numSols = 0

  add(allDifferent(x), Strong)
  // channeling x <-> binary
  for (i <- 0 until m) {
    val t = Array.tabulate(n)(j => CPIntVar(0 to base - 1))
    add(x(i) === toNum(t, base))
    for (j <- 0 until n) {
      add(binary(i)(j) === t(j))
    }
  }
  // the de Bruijn condition
  // the first elements in binary[i] is the same as the last
  // elements in binary[i-i]
  for (i <- 1 until m; j <- 1 until n) {
    add(binary(i - 1)(j) === binary(i)(j - 1))
  }
  // and around the corner
  for (j <- 1 until n) {
    add(binary(m - 1)(j) === binary(0)(j - 1))
  }
  // convert binary -> bin_code (de Bruijn sequence)
  for (i <- 0 until m) {
    add(bin_code(i) === binary(i)(0))
  }
  // gcc on the de Bruijn sequence
  add(gcc(bin_code, gccv))
  // symmetry breaking: the smallest number in x should be first
  add(minimum(x, x(0)))
  search {
    binaryFirstFail(x)
  }
  /*
  onSolution {
    println("x: " + x.mkString(""))
    println("de Bruijn sequence:" + bin_code.mkString(""))
    println("gcc:" + gccv.mkString(""))
    println()
  }*/
  println(start())
}
