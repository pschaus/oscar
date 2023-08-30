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
  Langford's number problem in Oscar.
  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html
  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object Langford extends CPModel with App {
  //
  // data
  //
  val k = if (args.length > 0) args(0).toInt else 4;
  val num_to_show = if (args.length > 1) args(1).toInt else 0;
  //
  // variables
  //
  val position = Array.fill(2 * k)(CPIntVar(0 to 2 * k - 1))
  // channel positions to a solution array
  val solution = Array.fill(2 * k)(CPIntVar(1 to k))
  //
  // constraints
  //
  var numSols = 0

  add(allDifferent(position), Medium)
  for (i <- 1 to k) {
    add(position(i + k - 1) === (position(i - 1) + i + 1))
    add(elementVar(solution,position(i-1),i),Strong)
    add(elementVar(solution,position(k + i - 1),i),Strong)
  }
  // symmetry breaking
  add(solution(0) < solution(2 * k - 1))
  search {
    binary(position, _.size, _.min)
  }
  onSolution {
    print("solution:" + solution.mkString("") + " ")
    println("position:" + position.mkString(""))
    numSols += 1
  }
  println(start(num_to_show))
}
