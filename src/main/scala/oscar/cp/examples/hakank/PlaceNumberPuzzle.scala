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
  Place number puzzle in Oscar.
  From http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  """
  Place numbers 1 through 8 on nodes
  - each number appears exactly once
  - no connected nodes have consecutive numbers
        2 - 5
      / | X |                                \
    1 - 3 - 6 - 8
      \ | X | /
        4 - 7
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object PlaceNumberPuzzle extends CPModel with App  {
    //
    // data
    //
    val n = 8 // number of nodes
    // Note: this is 1-based for compatibility (and lazyness)
    val graph =  Array(Array(1,2),
                       Array(1,3),
                       Array(1,4),
                       Array(2,1),
                       Array(2,3),
                       Array(2,5),
                       Array(2,6),
                       Array(3,2),
                       Array(3,4),
                       Array(3,6),
                       Array(3,7),
                       Array(4,1),
                       Array(4,3),
                       Array(4,6),
                       Array(4,7),
                       Array(5,2),
                       Array(5,3),
                       Array(5,6),
                       Array(5,8),
                       Array(6,2),
                       Array(6,3),
                       Array(6,4),
                       Array(6,5),
                       Array(6,7),
                       Array(6,8),
                       Array(7,3),
                       Array(7,4),
                       Array(7,6),
                       Array(7,8),
                       Array(8,5),
                       Array(8,6),
                       Array(8,7))
    val m = graph.length
    //
    // variables
    //
    val x = Array.fill(n)(CPIntVar(1 to n))
    //
    // constraints
    //
    var numSols = 0
  
     add(allDifferent(x))
      for(i <- 0 until m) {
        // (also make 0-base)
       add( (x(graph(i)(0)-1)-x(graph(i)(1)-1)).abs > 1)
      }
      // symmetry breaking
     add(x(0) < x(n-1))
    search{
      binaryMaxDegree(x)
    }
onSolution {
      println("x: " + x.mkString(""))
      numSols += 1
   } 
   println(start())
  }
