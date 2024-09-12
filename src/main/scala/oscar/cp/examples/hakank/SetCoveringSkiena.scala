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
  Set covering problem in Oscar.
  Example from Steven Skiena, The Stony Brook Algorithm Repository
  http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
  """
  Input Description: A set of subsets S_1, ..., S_m of the
  universal set U = {1,...,n}.
  Problem: What is the smallest subset of subsets T subset S such
  that \cup_{t_i in T} t_i = U?
  """
  Data is from the pictures INPUT/OUTPUT.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object SetCoveringSkiena extends CPModel with App {
    //
    // data
    //
    val num_sets = 7
    val num_elements = 12
    // Which element belongs to which set
    val belongs = Array(
                           // 1 2 3 4 5 6 7 8 9 0 1 2  elements
                        Array(1,1,0,0,0,0,0,0,0,0,0,0), // Set 1
                        Array(0,1,0,0,0,0,0,1,0,0,0,0), //     2
                        Array(0,0,0,0,1,1,0,0,0,0,0,0), //     3
                        Array(0,0,0,0,0,1,1,0,0,1,1,0), //     4
                        Array(0,0,0,0,0,0,0,0,1,1,0,0), //     5
                        Array(1,1,1,0,1,0,0,0,1,1,1,0), //     6
                        Array(0,0,1,1,0,0,1,1,0,0,1,1)) //     7
    //
    // variables
    //
    val x = Array.fill(num_sets)(CPIntVar(0 to 1))
    val z = sum(x)
    // total number of elements in the choosen sets
    val tot_elements = CPIntVar(0 to num_sets*num_elements)
    //
    // constraints
    //
    var numSols = 0
   minimize(z) 
      // all sets must be used
      for(j <- 0 until num_elements) {
       add(
               sum(
                   for{i <- 0 until num_sets} yield x(i)*belongs(i)(j)
                   ) >= 1
               )
      }
      // number of used elements
     add(
             tot_elements ===
             sum( for{i <- 0 until num_sets
                      j <- 0 until num_elements
                } yield x(i) * belongs(i)(j))
             )
    search{
      binaryStatic(x)
    }
onSolution {
      println("\nSolution:")
      println("z: " + z)
      println("tot_elements: " + tot_elements)
      println("x: " + x.mkString(""))
      println("Selected alternatives: " + 
              (for(i <- 0 until x.length if x(i).value==1 ) yield i).mkString(" "))
      numSols += 1
    }
    println(start())
  }
