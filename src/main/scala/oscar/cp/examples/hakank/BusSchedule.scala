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
  Bus scheduling in Oscar.
  Minimize number of buses in timeslots.
  Problem from Taha "Introduction to Operations Research", page 58.
  Note: This is a slightly more general model than Taha's.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object BusSchedule extends CPModel with App {
  //
  // data
  //
  val time_slots = 6
  // min number of buses for each time slot
  val demands = Array(8, 10, 7, 12, 4, 4)
  val max_num = demands.sum
  //
  // variables
  //
  // How many buses start the schedule at time slot t
  val x = Array.fill(time_slots)(CPIntVar(0 to max_num))
  // Total number of buses
  val num_buses = sum(x)
  //
  // constraints
  //
  minimize(num_buses)
  // Meet the demands for this and the next time slot.
  for (i <- 0 until time_slots - 1) {
    add(x(i) + x(i + 1) >= demands(i))
  }
  // The demand "around the clock"
  add(x(time_slots - 1) + x(0) - demands(time_slots - 1) === 0)
  search {
    binaryStatic(x)
  }
  onSolution {
    println("\nSolution:")
    println("x: " + x.mkString(""))
    println("num_buses : " + num_buses)
    println()
  }
  println(start())
}
