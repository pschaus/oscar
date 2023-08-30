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
import scala.io.Source._
import scala.math._
/*
  Costas Array in Oscar.
  From http://mathworld.wolfram.com/CostasArray.html:
  """
  An order-n Costas array is a permutation on {1,...,n} such
  that the distances in each row of the triangular difference
  table are distinct. For example, the permutation {1,3,4,2,5}
  has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
  and {4}. Since each row contains no duplications, the permutation
  is therefore a Costas array.
  """
  Also see
  - http://en.wikipedia.org/wiki/Costas_array
  - My MiniZinc model: http://www.hakank.org/minizinc/costas_array.mzn
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object CostasArray extends CPModel with App {
  //
  // data
  //
  val n = if (args.length > 0) args(0).toInt else 6;
  //
  // variables
  //
  val costas = Array.fill(n)(CPIntVar(1 to n))
  // Matrix of differences
  val differences = Array.fill(n, n)(CPIntVar(-n + 1 to n - 1))
  //
  // constraints
  //

  // Fix the values in the lower triangle in the
  // difference matrix to -n+1. This removes variants
  // of the difference matrix for the the same Costas array.
  for (i <- 0 until n; j <- 0 to i) {
    add(differences(i)(j) === -n + 1)
  }
  add(allDifferent(costas), Strong)
  // hakank: All the following constraints (and comments)
  // are from Barry O'Sullivans's original MiniZinc model
  // which my own are based on:
  // http://www.hakank.org/minizinc/costas_array.mzn
  // "How do the positions in the Costas array relate
  //  to the elements of the distance triangle."
  for (i <- 0 until n; j <- 0 until n if i < j) {
    add(differences(i)(j) === (costas(j) - costas(j - i - 1)))
  }
  // "All entries in a particular row of the difference
  //  triangle must be distint."
  for (i <- 0 until n - 2) {
    add(allDifferent(
      for (j <- 0 until n if j > i) yield differences(i)(j)), Strong)
  }
  //
  // "All the following are redundant - only here to speed up search."
  //
  // "We can never place a 'token' in the same row as any other."
  for (i <- 0 until n; j <- 0 until n if i < j) {
    add(differences(i)(j) !== 0, Strong)
    add(differences(i)(j) !== 0, Strong)
  }
  for (k <- 2 until n; l <- 2 until n if k < l) {
    add(
      (differences(k - 2)(l - 1) + differences(k)(l)) -
        (differences(k - 1)(l - 1) + differences(k - 1)(l)) === 0)
  }
  search {
    binaryFirstFail(costas)
  }
  onSolution {
    println("\nSolution:")
    println("costas:" + costas.mkString(""))
    println("differences:");
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        val v = differences(i)(j).value
        print(if (v == -n + 1) "   " else "%3d".format(v))
      }
      println()
    }
    println()
  }
  println(start())
}
