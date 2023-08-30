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
import scala.math._
/**
  Magic square in Oscar.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 */
object MagicSquare extends CPModel with App  {
    val n = if (args.length > 0) args(0).toInt else 4;
    val num_to_show = if (args.length > 1) args(1).toInt else 0;
    val n2 = n*n
    println("n:" + n + " num_to_show: " + num_to_show)
    //
    // variables
    //
    val x = Array.fill(n,n)(CPIntVar(1 to n2))
    val x_t = x.transpose
    // val total = CPIntVar(cp, 1 to n*n*n)
    val total = (n * (n*n + 1) / 2)
    //
    // constraints
    //
  
      add(allDifferent(x.flatten), Strong)
       // rows and columns
       for(i <- 0 until n) {
        add(sum(x(i)) === total)
        add(sum(x_t(i)) === total)
       }
       // diagonals
      add(sum(for(i <- 0 until n) yield x(i)(i)) === total)
      add(sum(for(i <- 0 until n) yield x(i)(n-i-1)) === total)
       // symmetry breaking
      add(x(0)(0)   < x(0)(n-1))
      add(x(0)(n-1) < x(n-1)(0))
      add(x(0)(0)   < x(n-1)(n-1))
     search{
       binary(x.flatten.toSeq, _.size, _.min)
     }
onSolution {
       println("\nSolution:\ntotal " + total)
       for(i <- 0 until n) {
         println(x(i).map(j=>"%3d".format(j.value)).mkString(""))
       }
       println()
     } 
     println(start(num_to_show))
   }
