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
  P-median problem in Oscar.
  Model and data from the OPL Manual, which describes the problem:
  """
  The P-Median problem is a well known problem in Operations Research.
  The problem can be stated very simply, like this: given a set of customers
  with known amounts of demand, a set of candidate locations for warehouses,
  and the distance between each pair of customer-warehouse, choose P
  warehouses to open that minimize the demand-weighted distance of serving
  all customers from those P warehouses.
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object PMedian extends CPModel with App  {
    //
    // data
    //
    val p = 2
    val num_customers = 4
    val CUSTOMERS = 0 until num_customers
    val num_warehouses = 3;
    val WAREHOUSES = 0 until num_warehouses
    val demand = Array(100,80,80,70)
    val distance = Array(
                         Array( 2, 10, 50),
                         Array( 2, 10, 52),
                         Array(50, 60,  3),
                         Array(40, 60,  1))
    //
    // variables
    //
    val open = Array.fill(num_warehouses)(CPIntVar(0 to num_warehouses))
    val ship = Array.fill(num_customers,num_warehouses)(CPIntVar(0 to 1))
      // val z = CPIntVar(cp, 0 to 1000)
    val z = sum(for{c <- CUSTOMERS
                    w <- WAREHOUSES} yield ship(c)(w)*demand(c)*distance(c)(w))      
    //
    // constraints
    //
    var numSols = 0
   minimize(z) 
     add(sum(open) === p)
      for(c <- CUSTOMERS) {
        for(w <- WAREHOUSES) {
         add(ship(c)(w) <= open(w))
        }
       add(sum(for(w <- WAREHOUSES) yield ship(c)(w)) === 1)
      }
    search{
      binary(ship.flatten ++ open, _.constraintDegree, _.max)
    }
onSolution {
      println("z:" + z)
      println("open:" + open.mkString(""))
      println("ship:")
      for(c <- CUSTOMERS) {
        println(ship(c).mkString(""))
      }
      println()
      numSols += 1
   }
   println(start())
  }
