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
  Bales of hay problem in Oscar.
  From The Math Less Traveled, 
  "The haybaler", http://www.mathlesstraveled.com/?p=582 
  """
  You have five bales of hay.
  For some reason, instead of being weighed individually, they were weighed 
  in all possible combinations of two. The weights of each of these 
  combinations were written down and arranged in numerical order, without 
  keeping track of which weight matched which pair of bales. The weights, 
  in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.
  How much does each bale weigh? Is there a solution? Are there multiple 
  possible solutions? 
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object BalesOfHay extends CPModel with App {
  // Decomposition of increasing
  def increasing(y: Array[CPIntVar]) = {
    for (i <- 1 until y.length) {
      add(y(i - 1) <= y(i))
    }
  }
  //
  // data
  //
  val n = 5
  val weights = Array(80, 82, 83, 84, 85, 86, 87, 88, 90, 91)
  //
  // variables
  //
  val bales = Array.fill(n)(CPIntVar(0 to 50))
  //
  // constraints
  //
  var numSols = 0
  for (w <- 0 until weights.length) {
    // indices in bales
    val i = CPIntVar(0 until n)
    val j = CPIntVar(0 until n)
    add(bales(i) + bales(j) === weights(w))
    add(i < j) // symmetry breaking
  }
  // symmetry breaking
  increasing(bales)
  search { binary(bales, -_.constraintDegree, _.max) }
  onSolution {
    println("bales:" + bales.mkString(""))
    numSols += 1
  }
  println(start())
}
