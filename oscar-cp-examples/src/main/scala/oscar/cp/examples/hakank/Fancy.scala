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
  Mr Greenguest puzzle (fancy dress) in Oscar.
  Problem (and LPL) code in
  http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2
  """
  (** Mr. Greenfan wants to give a dress party where the male guests
   * must wear green dresses. The following rules are given:
   * 1 If someone wears a green tie he has to wear a green shirt.
   * 2 A guest may only wear green socks and a green shirt 
   *   if he wears a green tie or a green hat.
   * 3 A guest wearing a green shirt or a green hat or who does
   *   not wear green socks must wear a green tie.
   * 4 A guest who is not dressed according to rules 1-3 must
   *   pay a $11 entrance fee.
   * Mr Greenguest wants to participate but owns only a green shirt 
   * (otherwise he would have to pay one for $9). He could buy 
   * a green tie for $10, a green hat (used) for $2 and green socks
   * for $12.
   * What is the cheapest solution for Mr Greenguest to participate?
   *)
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object Fancy extends CPModel with App  {
    //
    // data
    //
    val k = 5
    val z = Array("t","h","r","s","n")
    //
    // variables
    //
    val x = Array.fill(k)(CPBoolVar())
    val x2 = Array.fill(k)(CPIntVar(0 to 1))
    val Array(t,h,r,s,n) = x
    val Array(t2,h2,r2,s2,n2) = x2
    // This don't work with CPBoolVar
    val cost = weightedSum(Array(10,2,12,11), Array(t2,h2,s2,n2))
    //
    // constraints
    //
   minimize(cost) 
    //solveAll 
      // channeling between CPBoolVar and CPIntVar
      for(i <- 0 until k) {
       add(x(i) === x2(i))
      }
      // This is a straight translation from the LPL code
     add( (t==>r) || n )
     add( ((s || r) ==> (t || h)) || n )  
     add( ((r || h || !s) ==> t) || n )
      // Using CPIntVar instead (not as nice...)
      //add( ((t===1) ==> (r===1)) || n===1 )
      //add( ((s===1 || r===1) ==> (t===1 || h===1)) || n===1 )  
      //add( ((r===1 || h===1 || s===0) ==> (t===1)) || n===1 )
    search{
      binaryStatic(x)
    }
onSolution {
      println("Cost: " + cost)
      println(" " + z.mkString(" "))
      println(x2.mkString(""))
      println()
    }
    println(start())
  }
