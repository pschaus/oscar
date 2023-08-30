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
/**
 *
 * Marathon puzzle in Oscar
 *
 * """
 * From Xpress example
 * http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
 * """
 * Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
 * have arrived as the first six at the Paris marathon.
 * Reconstruct their arrival order from the following
 * information:
 * a) Olivier has not arrived last
 * b) Dominique, Pascal and Ignace have arrived before Naren
 *    and Olivier
 * c) Dominique who was third last year has improved this year.
 * d) Philippe is among the first four.
 * e) Ignace has arrived neither in second nor third position.
 * f) Pascal has beaten Naren by three positions.
 * g) Neither Ignace nor Dominique are on the fourth position.
 *
 * (c) 2002 Dash Associates
 * author: S. Heipcke, Mar. 2002
 * """
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Marathon extends CPModel with App  {
   // 
   // Decomposition of inverse constraint
   // 
   // Channel of positions of x and y:
   //    j == x(i) <=> y(j) == i
   // 
   // Here: 
   //   x is the position array
   //   y are the placements
   // 
   def inverse(x: Array[CPIntVar], y: Array[CPIntVar]): Unit = {
      val len = x.length
      for(i <- 0 until len;
          j <- 0 until len) {
       add((y(j) ?=== i) === (x(i) ?=== j))
      }
   }
   // Same as inverse() but returns the y Array
   def inverse2(x: Array[CPIntVar]) : Array[CPIntVar] = {
      val len = x.length
      val y = Array.fill(len)(CPIntVar(0 to len-1))
      for(i <- 0 until len;
          j <- 0 until len) {
       add((y(j) ?=== i) === (x(i) ?=== j))
      }
      y
   }
      //
      // data
      // 
      val n = 6
      val runners_str = Array("Dominique", "Ignace", "Naren",
                              "Olivier", "Philippe", "Pascal")
      //
      // decision variables
      // 
      // Note: in order to use inverse(), the runners and places are in the domain 0..n-1
      val runners = Array.fill(n)(CPIntVar(0 to n-1))
      val Array(dominique, ignace, naren, olivier, philippe, pascal) = runners
      val places = inverse2(runners)
      var numSols = 0
    
    	add(allDifferent(runners), Strong)
        // a: Olivier not last
       add(olivier !== n)
        // b: Dominique, Pascal and Ignace before Naren and Olivier
        for(a <- Array(dominique, pascal, ignace);
            b <- Array(naren, olivier)) {
          add(a < b)
        }
        // c: Dominique better than third
       add(dominique  < 2)
        // d: Philippe is among the first four
       add(philippe   <= 3)
        // e: Ignace neither second nor third
       add(ignace     !== 1)
       add(ignace     !== 2)
        // f: Pascal three places earlier than Naren
       add(pascal + 3 === naren)
        // g: Neither Ignace nor Dominique on fourth position
       add(ignace     !== 3)
       add(dominique  !== 3)
      search{
        binaryStatic(runners ++ places)
      }
onSolution {
        println("Runners: " ++ runners.mkString(""))
        println("Places:")
        for(p <- 0 until n) {
          println("Place " + (p+1) + ": " + runners_str(places(p).value))
        }
        println()
        numSols += 1
      }
      println(start())
  }
