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
import oscar.cp.core.CPPropagStrength
import oscar.cp._
import oscar.cp.core.delta.DeltaSetVar
import oscar.cp.core.variables.CPVar

import scala.collection.mutable.ArrayBuffer


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestDeltaSetPropagate extends TestSuite {




  test("test delta set 1") {
    var propag = false
    
    class MyCons(val X: CPSetVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2-5
      override def setup(l: CPPropagStrength): Unit = {
        //println("setup")
        X.filterWhenDomainChangesWithDelta(){ delta =>
          propag = true
          
          delta.changed should be(true)
          delta.requiredChanged should be(true)
          delta.possibleChanged should be(true)
          delta.deltaRequiredSize should be(2)
          delta.deltaRequired.toSet should be(Set(1,3))
          delta.deltaPossibleSize should be(2)
          delta.deltaPossible.toSet should be(Set(2,4))
        }
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = new CPSetVar(cp, 1 , 5)
    //println(x.requiredSize+" "+x.possibleSize)
    cp.add(new MyCons(x))
    
    val cons = ArrayBuffer[Constraint]()
    cons.append(x ++ 1)
    cons.append(x ++ 3)
    cons.append(x -- 2)
    cons.append(x -- 4)
    
    cp.add(cons)
    
    //println(x.requiredSet+" <= x <="+x.possibleSet)
    propag should be(true)
  }
  
  
  test("test delta set 2") {
    var propag = false
    
    class MyCons(val X: CPSetVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2-5
      var delta: DeltaSetVar = null
      override def setup(l: CPPropagStrength): Unit = {
        delta = X.callPropagateOnChangesWithDelta(this)
      }
      override def propagate(): Unit = {
          propag = true
          delta.changed() should be(true)
          delta.requiredChanged should be(true)
          delta.possibleChanged should be(true)
          delta.deltaRequiredSize should be(2)
          delta.deltaRequired().toSet should be(Set(1,3))
          delta.deltaPossibleSize() should be(2)
          delta.deltaPossible().toSet should be(Set(2,4))
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = new CPSetVar(cp, 1 , 5)
    //println(x.requiredSize+" "+x.possibleSize)
    cp.add(new MyCons(x))
    
    val cons = ArrayBuffer[Constraint]()
    cons.append(x ++ 1)
    cons.append(x ++ 3)
    cons.append(x -- 2)
    cons.append(x -- 4)
    
    cp.add(cons)
    
    //println(x.requiredSet+" <= x <="+x.possibleSet)
    propag should be(true)
  }
  
  
}
