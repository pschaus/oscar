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
/*
  Furniture Moving (scheduling) problem in Oscar.
  Problem from Marriott & Stuckey: 'Programming with constraints', page  112f
  This model includes a decomposition of the cumulative constraint.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object FurnitureMoving extends CPModel with App  {
  
  // Thanks Pierre Schaus for help with this decomposition of cumulative.
  def myCumulative(
    s: Array[CPIntVar],
    d: Array[Int],
    r: Array[Int],
    b: CPIntVar): Unit = {
    val tasks = for {
      i <- 0 to s.length - 1
      if (r(i) > 0 && d(i) > 0)
    } yield i
    val times_min = tasks.map(i => s(i).min).min
    val d_max = d.max
    val times_max = tasks.map(i => s(i).max + d_max).min
    for (t <- times_min to times_max) {
      add(sum(tasks)(i => ((s(i) ?< t) && ((s(i) + d(i)) ?> t)) * r(i)) <= b)
    }
    add(b <= r.sum);
  }
  
  //
  // data
  //
  val n = 4
  val upper_limit = 160
  val durations = Array(30, 10, 15, 15)
  val resources = Array(3, 1, 3, 2)
  
  //
  // variables
  //
  val starts = Array.fill(n)(CPIntVar(0 to upper_limit))
  val end_times = Array.fill(n)(CPIntVar(0 to upper_limit * 2))
  val num_persons = CPIntVar(1 to 100)
  val max_end_time = maximum(end_times)
  
  //
  // constraints
  //
  var numSols = 0
  //minimize(max_end_time) 
  minimize(num_persons)
  // ensure that end_times is starts + duration
  for (i <- 0 until n) {
    add(end_times(i) === starts(i) + durations(i))
  }
  // when searching for max_end_time
  //add(num_persons == 3)
  myCumulative(starts, durations, resources, num_persons)
  search {
    binaryFirstFail(starts ++ Array(num_persons))
  }
  onSolution {
    println("\nSolution:")
    println("num_persons : " + num_persons)
    println("max_end_time: " + max_end_time)
    println("start       : " + starts.mkString(" "))
    println("durations   : " + durations.mkString(" "))
    println("resources   : " + resources.mkString(" "))
    println("end_times   : " + end_times.mkString(" "))
    println()
    numSols += 1
  }
  println(start())
}
