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


/*
  Breaking news puzzle (Dell Logic Puzzles) in Oscar.
  Problem from  
  http://brownbuffalo.sourceforge.net/BreakingNewsClues.html
  """
  Title: Breaking News
  Author: Faith Johnson
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1
  The Daily Galaxy sent its four best reporters (Corey, Jimmy, Lois, and 
  Perry) to different locations (Bayonne, New Hope, Port Charles, and 
  South Amboy) to cover four breaking news events (30-pound baby, blimp 
  launching, skyscraper dedication, and beached whale). Their editor is 
  trying to remember where each of the reporters is. Can you match the name 
  of each reporter with the place he or she was sent, and the event that 
  each covered?
  1. The 30-pound baby wasn't born in South Amboy or New Hope.
  2. Jimmy didn't go to Port Charles.
  3. The blimp launching and the skyscraper dedication were covered, in some 
     order, by Lois and the reporter who was sent to Port Charles.
  4. South Amboy was not the site of either the beached whale or the 
     skyscraper dedication.
  5. Bayonne is either the place that Corey went or the place where the 
     whale was beached, or both.
  Determine: Reporter -- Location -- Story
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object BreakingNews extends CPModel with App {
  // 
  // Decomposition of inverse constraint
  // 
  // Channel of positions of x and y:
  //    j == x(i) <=> y(j) == i
  // 
  // Note: This requires the domain 0..n-1
  //
  def inverse(x: Array[CPIntVar], y: Array[CPIntVar]): Unit = {
    val len = x.length
    for (i <- 0 until len; j <- 0 until len) {
      add((y(j) ?=== i) === (x(i) ?=== j))
    }
  }
  // Convenient function which returns y (for presentation)
  def inverse2(x: Array[CPIntVar]): Array[CPIntVar] = {
    val y = Array.fill(x.length)(CPIntVar(x(0).min to x(0).max))
    inverse(x, y)
    y
  }
  //
  // data
  //
  val n = 4
  val Array(corey, jimmy, lois, perry) = (0 to n - 1).toArray
  val names = Array("Corey", "Jimmy", "Lois", "Perry")
  //
  // variables
  //
  val locations = Array.fill(n)(CPIntVar(0 to n - 1))
  val Array(bayonne, new_hope, port_charles, south_amboy) = locations
  // for output
  val locationsStr = Array("Bayonne", "New Hope", "Port Charles", "South Amboy")
  val locationsInv = inverse2(locations)
  val events = Array.fill(n)(CPIntVar(0 to n - 1))
  val Array(baby, blimp, skyscraper, whale) = events
  // for output
  val eventsStr = Array("Baby", "Blimp", "Skyscraper", "Whale")
  val eventsInv = inverse2(events)
  //
  // constraints
  //
  add(allDifferent(locations), Strong)
  add(allDifferent(events), Strong)
  // 1. The 30-pound baby wasn't born in South Amboy or New Hope.
  add(baby !== south_amboy)
  add(baby !== new_hope)
  //  2. Jimmy didn't go to Port Charles.
  add(port_charles !== jimmy)
  //  3. The blimp launching and the skyscraper dedication were covered, 
  //     in some order, by Lois and the reporter who was sent to 
  //     Port Charles.
  add(
    ((blimp ?=== lois) && (skyscraper ?=== port_charles))
      ||
      ((skyscraper ?=== lois) && (blimp ?=== port_charles))
  )
  //  4. South Amboy was not the site of either the beached whale or the 
  //     skyscraper  dedication.
  add(south_amboy !== whale)
  add(south_amboy !== skyscraper)
  //  5. Bayonne is either the place that Corey went or the place where 
  //     the whale was beached, or both.
  add((bayonne ?=== corey) || (bayonne ?=== whale))
  search { binaryStatic(locations ++ events) }
  onSolution {
    println("Names    : " + names.mkString(" "))
    println("Locations:" + locations.mkString(""))
    println("Events   :" + events.mkString(""))
    println()
    println("Names    : " + names.mkString(", "))
    println("Locations: " + locationsInv.map(s => locationsStr(s.value)).mkString(", "))
    println("Events   : " + eventsInv.map(s => eventsStr(s.value)).mkString(", "))
    println()
    println((0 until n).
      map(s => Array(names(s), locationsStr(locationsInv(s).value), eventsStr(eventsInv(s).value)).mkString(", ")).mkString("\n"))
    println()
  }
  println(start())
}
