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


package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite

import oscar.cp.constraints._

import oscar.cp._

class TestSortedness extends TestSuite {

  
  test("Sortedness1") {
    val cp = CPSolver()
    val x_ = Array(5,2,4,1,3)
    val s_ = Array(1,2,3,4,5)
    val p_ = Array(3,1,4,2,0)
    val x = Array.tabulate(x_.size)(i => CPIntVar(x_(i))(cp))
    val s = Array.tabulate(x_.size)(i => CPIntVar(s_(i))(cp))
    val p = Array.tabulate(x_.size)(i => CPIntVar(0 until x.size)(cp))
    cp.add(sortedness(x,s,p),Strong)
    //println(p.mkString(","))
    for (i <- 0 until x.size) {
      p(i).value should be(p_(i))
    }
    cp.isFailed should be(false)
  }
  
  test("Sortedness2") {
    val cp = CPSolver()
    val x_ = Array(5,2,4,1,3)
    val s_ = Array(1,2,3,4,5)
    val p_ = Array(3,1,4,2,0)
    val x = Array.tabulate(x_.size)(i => CPIntVar(x_(i))(cp))
    val s = Array.tabulate(x_.size)(i => CPIntVar(0 to 100)(cp))
    val p = Array.tabulate(x_.size)(i => CPIntVar(p_(i))(cp))
    cp.add(sortedness(x,s,p),Strong)
    //println(p.mkString(","))
    for (i <- 0 until x.size) {
      s(i).value should be(s_(i))
    }
    cp.isFailed should be(false)
  }
  
  test("Sortedness3") {
    val cp = CPSolver()
    val x_ = Array(5,2,4,1,3)
    val s_ = Array(1,2,3,4,5)
    val p_ = Array(3,1,4,2,0)
    val x = Array.tabulate(x_.size)(i => CPIntVar(0 to 10)(cp))
    val s = Array.tabulate(x_.size)(i => CPIntVar(0 to 100)(cp))
    val p = Array.tabulate(x_.size)(i => CPIntVar(p_(i))(cp))
    cp.add(sortedness(x,s,p),Strong)
    for (i <- 0 until x_.size) {
      cp.add(x(i) === x_(i))
    }
    
    //println(p.mkString(","))
    for (i <- 0 until x.size) {
      s(i).value should be(s_(i))
    }
    cp.isFailed should be(false)
  }

  
  test("Sortedness4") {
    val cp = CPSolver()
    
    val doms =   Array((0,4),(0,19),(0,23),(0,27),(0,10),(0,15),(0,17),(0,18),(0,21),(0,22),(0,26),(0,27),(0,9),(0,10),(0,13),(0,15),(0,26),(0,1),(0,6),(0,16),(0,17),(0,9),(0,11),(0,15),(0,20),(0,29))
    val sol = Array( 1, 16, 20, 23, 5, 11, 13, 15, 18, 19, 22, 24, 3, 6, 8, 9, 21, 0, 2, 12, 14, 4, 7, 10, 17, 25)
    
    val x = doms.map(d => CPIntVar(d._1 to d._2)(cp))
    val s = doms.map(d => CPIntVar(0 until 30)(cp))
    val p = Array.fill(x.size)(CPIntVar(0 until x.size)(cp))
   
    cp.add(sortedness(x,s,p),Strong)
    for (i <- 0 until x.size) {
      cp.add(x(i) === sol(i))
    }
    cp.allBounds(p) should be(true)
    cp.allBounds(s) should be(true)
    //println(s.mkString(","))
    //println(p.mkString(","))
  }
  
  

  test("Sortedness5") {
    val cp = CPSolver()
    val x_ = Array(7, 2, 5, 1, 3)
    val s_ = Array(1, 2, 3, 5, 7)
    val p_ = Array(3, 1, 4, 2, 0)
    val x = Array.tabulate(x_.size)(i => CPIntVar(0 to 10)(cp))
    val s = Array.tabulate(x_.size)(i => CPIntVar(0 to 100)(cp))
    val p = Array.tabulate(x_.size)(i => CPIntVar(0 to 4)(cp))
    cp.add(sortedness(x,s,p),Strong)
    for (i <- 0 until x_.size) {
      cp.add(x(i) === x_(i))
    }
    for (i <- 0 until x.size) {
      s(i).value should be(s_(i))
    }
    cp.isFailed should be(false)
  }

}
  
  
