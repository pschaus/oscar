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
import oscar.cp.core._
import oscar.cp.modeling._
import oscar.cp.multiobjective.ListPareto

class TestListPareto extends TestSuite {

  test("equals") {
    val p1 = ListPareto[Any](2, maximization = true)
    val p2 = ListPareto[Any](2, maximization = true)
    p1 == p2 should be(true)
    p1.insert(null, 1, 2)
    p1 == p2 should be(false)
    p2.insert(null, 1, 2)
    p1 == p2 should be(true)
    p1.insert(null, 0, 3)
    p1 == p2 should be(false)
    p2.insert(null, 0, 3)
    p1 == p2 should be(true)
  }

  test("dominate - max") {
    val paretoSet = ListPareto[Any](2, maximization = true)    
    val sol1 = Array(2, 2)
    val sol2 = Array(2, 3)
    val sol3 = Array(4, 4)
    val sol4 = Array(1, 5)
    // A solutions dominates itself
    paretoSet.dominate(sol1, sol1) should be(true)
    // Pareto dominance
    paretoSet.dominate(sol1, sol2) should be(false)
    paretoSet.dominate(sol2, sol1) should be(true)
    paretoSet.dominate(sol3, sol1) should be(true)
    paretoSet.dominate(sol1, sol3) should be(false)
    // Non dominated
    paretoSet.dominate(sol3, sol4) should be(false)
    paretoSet.dominate(sol4, sol3) should be(false)
  }

  test("dominate - min") {
    val paretoSet = ListPareto[Any](2, maximization = false)    
    val sol1 = Array(2, 2)
    val sol2 = Array(2, 3)
    val sol3 = Array(4, 4)
    val sol4 = Array(1, 5)
    // A solutions dominates itself
    paretoSet.dominate(sol1, sol1) should be(true)
    // Pareto dominance
    paretoSet.dominate(sol1, sol2) should be(true)
    paretoSet.dominate(sol2, sol1) should be(false)
    paretoSet.dominate(sol3, sol1) should be(false)
    paretoSet.dominate(sol1, sol3) should be(true)
    // Non dominated
    paretoSet.dominate(sol3, sol4) should be(false)
    paretoSet.dominate(sol4, sol3) should be(false)
  }

  test("dominate - min max") {
    val paretoSet = ListPareto[Any](false, true)  
    val sol1 = Array(2, 2)
    val sol2 = Array(2, 3)
    val sol3 = Array(4, 4)
    val sol4 = Array(1, 5)
    // A solutions dominates itself
    paretoSet.dominate(sol1, sol1) should be(true)
    // Pareto dominance
    paretoSet.dominate(sol1, sol2) should be(false)
    paretoSet.dominate(sol2, sol1) should be(true) 
    paretoSet.dominate(sol3, sol4) should be(false)
    paretoSet.dominate(sol4, sol3) should be(true)
    // Non dominated
    paretoSet.dominate(sol3, sol1) should be(false)
    paretoSet.dominate(sol1, sol3) should be(false)   
  }
  
  test("nObjs and Objs") {    
    val paretoSet1 = ListPareto[Any](false, true, true, true)
    val paretoSet2 = ListPareto[Any](4)   
    paretoSet1.nObjs should be(4)
    paretoSet2.nObjs should be(4)   
    paretoSet1.Objs == (0 until 4) should be(true)
    paretoSet2.Objs == (0 until 4) should be(true)
  }
  
  test("size and isEmpty") {    
    val paretoSet = ListPareto[Any](true, true)    
    // Initially empty
    paretoSet.isEmpty should be(true)
    paretoSet.size should be(0)
    // Add one solution
    paretoSet.insert(null, 3, 4)
    paretoSet.isEmpty should be (false)
    paretoSet.size should be(1)
    // Add a second non dominated solution
    paretoSet.insert(null, 4, 3)
    paretoSet.isEmpty should be (false)
    paretoSet.size should be(2)
  }
  
  test("removeAll") {   
    val paretoSet = ListPareto[Any](true, true)    
    // No effect on an empty set
    paretoSet.removeAll()
    paretoSet.isEmpty should be(true)
    // Remove all the solutions
    paretoSet.insert(null, 3, 4)
    paretoSet.insert(null, 4, 3)   
    paretoSet.removeAll()
    paretoSet.isEmpty should be(true)
  } 
  
  test("insert") {    
    val paretoSet = ListPareto[Any](true, true)    
    paretoSet.size should be(0)
    // Singleton
    paretoSet.insert(null, 3, 4) should be(true)
    paretoSet.size should be(1)
    // Non dominated
    paretoSet.insert(null, 4, 3) should be(true)
    paretoSet.size should be(2)
    // Same solution
    paretoSet.insert(null, 4, 3) should be(false)
    paretoSet.size should be(2)
    // Dominated solution
    paretoSet.insert(null, 1, 1) should be(false)
    paretoSet.size should be(2)
    // Best of all
    paretoSet.insert(null, 5, 5) should be(true)
    paretoSet.size should be(1)
  }
  
  test("getDominant and isDominated") {    
    val paretoSet = ListPareto[String](true, true)     
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))    
    // Dominated by many solutions
    paretoSet.getDominant(Array(2, 2)).isDefined should be(true)
    paretoSet.isDominated(Array(2, 2)) should be(true)
    // Dominated by one solution
    paretoSet.getDominant(Array(3, 4)).isDefined should be(true)
    paretoSet.getDominant(Array(3, 4)).get should be("A")
    paretoSet.isDominated(Array(3, 4)) should be(true)
    // Not dominated
    paretoSet.getDominant(Array(5, 5)).isDefined should be(false)
    paretoSet.isDominated(Array(5, 5)) should be(false)  
  }
  
  test("foreach") {    
    val paretoSet = ListPareto[String](true, true)     
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))  
    paretoSet.insert("C", Array(5, 2))   
    paretoSet.insert("D", Array(2, 5))  
    // Insert all the solutions in a list
    val sols = for (p <- paretoSet) yield p
    sols.size should be(4)
    sols contains "A" should be(true)
    sols contains "B" should be(true)
    sols contains "C" should be(true)
    sols contains "D" should be(true)
  }
  
  test("map") {   
    val paretoSet = ListPareto[String](true, true)       
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))  
    paretoSet.insert("C", Array(5, 2))   
    paretoSet.insert("D", Array(2, 5))  
    paretoSet.map(_.toLowerCase()) == Array("a", "b", "c", "d") 
  }
  
  test("mkString") {
    val paretoSet = ListPareto[String](true, true)    
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))  
    paretoSet.insert("C", Array(5, 2))   
    paretoSet.insert("D", Array(2, 5))     
    paretoSet.mkString("+") == "A+B+C+D"  
  }
  
  test("filter") {
    val paretoSet = ListPareto[String](true, true)    
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))  
    paretoSet.insert("C", Array(5, 2))   
    paretoSet.insert("D", Array(2, 5))     
    val filtered1 = paretoSet.filter(_ != "A")
    filtered1.size should be(3)
    filtered1 contains "A" should be(false)
    filtered1 contains "B" should be(true)
    filtered1 contains "C" should be(true)
    filtered1 contains "D" should be(true)
    val filtered2 = paretoSet.filter(_ != "C")
    filtered2.size should be(3)
    filtered2 contains "A" should be(true)
    filtered2 contains "B" should be(true)
    filtered2 contains "C" should be(false)
    filtered2 contains "D" should be(true)
  }
  
  test("toList") {
    val paretoSet = ListPareto[String](true, true)    
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))  
    paretoSet.insert("C", Array(5, 2))   
    paretoSet.insert("D", Array(2, 5))     
    val sols = paretoSet.toList  
    sols.size should be(4)
    sols contains "A" should be(true)
    sols contains "B" should be(true)
    sols contains "C" should be(true)
    sols contains "D" should be(true)
  }
  
  test("objectiveSols") {
    val paretoSet = ListPareto[String](true, true)   
    val a1 = IndexedSeq(3, 4)
    val a2 = IndexedSeq(4, 3)
    val a3 = IndexedSeq(5, 2)
    val a4 = IndexedSeq(2, 5)
    paretoSet.insert("A", a1)
    paretoSet.insert("B", a2)  
    paretoSet.insert("C", a3)   
    paretoSet.insert("D", a4)     
    val sols = paretoSet.objectiveSols  
    sols.size should be(4)
    sols contains a1 should be(true)
    sols contains a2 should be(true)
    sols contains a3 should be(true)
    sols contains a4 should be(true)
  }
  
  test("min") {  
    val paretoSet = ListPareto[String](true, true)   
    // Empty set
    val min1 = paretoSet.min(_.charAt(0).toInt)
    min1.isDefined should be(false) 
    // Non empty set
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))  
    paretoSet.insert("C", Array(5, 2))   
    paretoSet.insert("D", Array(2, 5)) 
    val min2 = paretoSet.min(_.charAt(0).toInt).get
    min2 should be("A")
  }
  
  test("max") {  
    val paretoSet = ListPareto[String](true, true)   
    // Empty set
    val max1 = paretoSet.max(_.charAt(0).toInt)
    max1.isDefined should be(false) 
    // Non empty set
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))  
    paretoSet.insert("C", Array(5, 2))   
    paretoSet.insert("D", Array(2, 5)) 
    val max2 = paretoSet.max(_.charAt(0).toInt).get
    max2 should be("D")
  }
  
  test("sortBy") {  
    val paretoSet = ListPareto[String](true, true)   
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))  
    paretoSet.insert("C", Array(5, 2))   
    paretoSet.insert("D", Array(2, 5)) 
    val sorted1 = paretoSet.sortBy(_.charAt(0).toInt)
    val sorted2 = paretoSet.sortBy(-_.charAt(0).toInt)
    sorted1 should be(List("A", "B", "C", "D"))
    sorted2 should be(List("D", "C", "B", "A"))
  }
    
  test("sortByObj") {  
    val paretoSet = ListPareto[String](true, true)   
    paretoSet.insert("A", Array(3, 4))
    paretoSet.insert("B", Array(4, 3))  
    paretoSet.insert("C", Array(5, 2))   
    paretoSet.insert("D", Array(2, 5)) 
    val sorted1 = paretoSet.sortByObj(0)
    val sorted2 = paretoSet.sortByObj(1)
    sorted1 should be(List("D", "A", "B", "C"))
    sorted2 should be(List("C", "B", "A", "D"))
  }
}
