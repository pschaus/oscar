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
  Magic squares and cards problem in Oscar.
  Martin Gardner (July 1971)
  """
  Allowing duplicates values, what is the largest constant sum for an order-3
  magic square that can be formed with nine cards from the deck.
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object MagicSquareAndCards extends CPModel with App  {
    //
    // data
    //
    val n = if (args.length > 0) args(0).toInt else 3;
    val RANGE = 0 until n
    val colors = 4
    val values = 13
    //
    // variables
    // 
    val x = Array.fill(n,n)(CPIntVar(1 to values))
    val s = CPIntVar(1 to values*colors)
    val counts = Array.tabulate(values+1)(i => (i,CPIntVar(0 to colors)))
    //
    // constraints
    //
    var numSols = 0
   maximize(s) 
     add(gcc(x.flatten, counts), Strong)
      // the standard magic square constraints 
      // (sans all_different of all elements)
      for(i <- RANGE) {
        // rows
        val row = for{j <- RANGE} yield x(i)(j)
       add( sum(row) === s)
       add( allDifferent(row), Strong)
        // columns
        val col = for{j <- RANGE} yield x(j)(i)
       add( sum(col) === s)
       add( allDifferent(col), Strong)
      }
      // diagonals
     add( sum(for{i <- RANGE} yield x(i)(i)) === s)
     add( sum(for{i <- RANGE} yield x(i)(n-i-1)) === s)
      // redundant constraint
     add(sum(counts.map(_._2)) === n*n)
      // symmetry breaking
     add(x(n-1)(n-1) === values)
    search{
      binaryMaxDegree(x.flatten.toSeq)
    }
onSolution {
      println("\nSolution:")
      println("counts: " + counts.mkString(""))
      for(i <- RANGE) {
        println(x(i).mkString(""))
      }
      numSols += 1
    }
    println(start())
  }
