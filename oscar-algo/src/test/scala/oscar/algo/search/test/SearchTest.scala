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
package oscar.algo.search.test


import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.algo.search._
import oscar.algo.reversible._
import oscar.algo.testUtils.TestSuite


class SearchTest extends TestSuite {

  test("test search0") {
    val node = new DFSearchNode()

    val i = new ReversibleInt(node, 0)
    node.search {
      if (i > 2) noAlternative
      else branch { i += 1} { if (i.value == 1) node.fail else i += 1}
    } 
    
    val stat = node.start()
    for (d <- 0 to 3) {
      node.start()
    }
    assert(stat.nSols == 4)
    
  }
  
  test("test search1") {
    val node = new DFSearchNode()

    val i = new ReversibleInt(node, 0)
    node.search {
      if (i.value > 2) noAlternative
      else {
        branch {i.incr() } { i.incr() }
      }
    }

    
    var nbSol = 0

    node.onSolution {
      nbSol +=1 
    }
    
    nbSol = 0
    assert(node.start().nSols == 8)
    assert(nbSol == 8)
    
    nbSol = 0
    assert(node.start(nSols = 2).nSols == 2)
    assert(nbSol == 2)
    
    nbSol = 0
    assert(node.start(failureLimit = 5).nSols == 5)
    assert(nbSol == 5)
    
    nbSol = 0
    assert(node.start(maxDiscrepancy = 0).nSols == 1)
    assert(nbSol == 1)
    
    nbSol = 0
    assert(node.start(maxDiscrepancy = 1).nSols == 4)
    assert(nbSol == 4)
    
    nbSol = 0
    assert(node.start(maxDiscrepancy = 2).nSols == 7)
    assert(nbSol == 7)

    nbSol = 0
    assert(node.start(maxDiscrepancy = 3).nSols == 8)
    assert(nbSol == 8)
  }
  
  test("test search2") {
    val node = new DFSearchNode()

    val d = Array(false, false, false)

    def branch(left: => Unit)(right: => Unit) = Seq(() => left, () => right)

    //def search
    val b = new Branching() {
      val i = new ReversibleInt(node, 0)

      override def alternatives = {
        if (i.value >= d.size) noAlternative
        else {
          branch {
            d(i.value) = true
            i.incr()
            if (d.count(v => v) == 2) node.fail() // if two values = true, we fail
          } {
            d(i.value) = false
            i.incr()
            if (d.count(v => v) == 2) node.fail() // if two values = true, we fail
          }
        }
      }
    }
    // solutions

    node.search(b)
    
    var nbSol = 0

    node.onSolution {
      nbSol +=1 
    }

    nbSol = 0
    assert(node.start().nSols == 4)
    assert(nbSol == 4)
    
    nbSol = 0
    assert(node.start(failureLimit = 1).nSols == 0)
  }
  
  
  test("test search3") {
    val node = new DFSearchNode()

    def branch(left: => Unit)(right: => Unit) = Seq(() => left, () => right)

    //def search
    val b = new Branching() {
      val i = new ReversibleInt(node, 0)

      override def alternatives = {
        if (i.value > 30) noAlternative
        else {
          branch {i.incr() } { i.incr() }
        }
      }
    }
    
    node.search(b)
     
    assert(node.start(timeLimit = 1).time <= 15000L)
    
  }
  
  test("test search4") {
    val node = new DFSearchNode()


    //def search
    val b1 = new Branching() {
      val i = new ReversibleInt(node, 0)
      override def alternatives = {
        if (i.value > 2) noAlternative
        else {
          branch {i.incr() } { i.incr() }
        }
      }
    }
    
    //def search
    val b2 = new Branching() {
      val i = new ReversibleInt(node, 0)
      override def alternatives = {
        if (i.value > 1) noAlternative
        else {
          branch {i.incr() } { i.incr() }
        }
      }
    }  
    
    var c = 0
    node.onSolution { c += 1 }
    node.search(b1++b2)
    assert(node.start().nSols  == 32)
    c should be(32)    
  }
  
  test("test search5") {
    val node = new DFSearchNode()
    
    var c = 0
    node.onSolution { c += 1 }

    //def search
    val i1 = new ReversibleInt(node, 0)
    val i2 = new ReversibleInt(node, 0)
    node.search {
      Branching {
        if (i1.value > 2) noAlternative
        else {
          branch { i1 += 1 } { i1 += 1 }
        } 
      } ++ Branching {
        if (i2.value > 1) noAlternative
        else {
          branch {i2 += 1 } { i2 += 1 }
        }           
      }
    }

    node.start().nSols should be(32)
    c should be(32)    
  }
  
  test("test search6") {
    val node = new DFSearchNode()

    val i = new ReversibleInt(node, 0)
    node.search {
      if (i > 2) noAlternative
      else branchAll(1 to 2) {v =>  i += 1}
    } 
    
    val stat = node.start()
    i.value should be(0)
    stat.nSols should be(8)
    
    node.search {
      if (i > 2) noAlternative
      else branchAll(1 to 3) {v =>  i += 1}
    } 
    
    val stat2 = node.start()
    stat2.nSols should be(27)
    
  }
 
  
  test("test search7") {
    val node = new DFSearchNode()
    val i = new ReversibleInt(node, 0)
    node.search {
      if (i > 1) noAlternative
      else branchAll(1 to 3) {v =>  i += 1}
    } 
    val stat2 = node.start()
    stat2.nSols should be(9)
  }   
  
  test("test search8") {
    val node = new DFSearchNode()
    val i = new ReversibleInt(node, 0)
    node.search {
      if (i > 0) noAlternative
      else branchAll(1 to 3) {v =>  i += 1}
    } 
    val stat2 = node.start()
    stat2.nSols should be(3)
  }
  
  
  test("test search9") {
    val node = new DFSearchNode()
    val i = new ReversibleInt(node, 0)
    node.search {
      if (i > 0) noAlternative
      else branchAll(1 to 3) {v =>  i += 1}
    } 
    val stat2 = node.start(maxDiscrepancy = 1)
    stat2.nSols should be(2)
  } 
  
  test("test search10") {
    val node = new DFSearchNode()
    val i = new ReversibleInt(node, 0)
    node.search {
      if (i > 2) noAlternative
      else branchAll(1 to 3) {v =>  i += 1}
    } 
    val stat2 = node.start(maxDiscrepancy = 5)
    stat2.nSols should be(26)
  }
  
  
  test("test search11") {
    val node = new DFSearchNode()
    val i = new ReversibleInt(node, 0)
    node.search {
      if (i > 2) noAlternative
      else branchAll(1 to 3) {v => if (v == 1) node.fail else  i += 1}
    } 
    val stat2 = node.start()
    stat2.nSols should be(8)
  }
  
  test("should restore the state of the root node") {
    val node = new DFSearchNode()
    
    val a = new ReversibleInt(node, 0)
    val b = new ReversibleInt(node, 0)
    
    a.value = 3
    b.value = 5
    node.pushState
    
    a.value = 4
    b.value = 4
    node.pushState
    
    a.value = 5
    b.value = 3
    node.pushState
    
    node.search {
      if (a > 8 || b > 8) noAlternative
      else branch(a.incr())(b.incr())
    } 
    
    node.start()
    
    assert(a.value == 5)
    assert(b.value == 3)
  }
}

