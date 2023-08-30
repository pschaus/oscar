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
package oscar.algo.paretofront.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.algo.paretofront.QTMin
import oscar.algo.paretofront.ParetoElement
import oscar.algo.paretofront.LinearList
import oscar.algo.paretofront.LinearListDouble


/**
 * @author : Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 * @author : Renaud Hartert ren.hartert@gmail.com
 */
class DummyElement(evals: Array[Double]) extends ParetoElement[Double] {
  override def objectives = evals
  override def coordinates = evals
}
object DummyElement {
  def apply(evals: Array[Double]) = new DummyElement(evals)
}

class LinearListTest extends FunSuite with Matchers {
  
  val randGen = new util.Random
  
  def genRandomPoint(intervals: Array[(Double, Double)]): Array[Double] = {
    Array.tabulate(intervals.length)(i => intervals(i)._1 + (intervals(i)._2 - intervals(i)._1) * randGen.nextDouble)
  }

  test("Inserted element in an empty Linear List should result in real insertion") {
    val linLs = LinearListDouble[ParetoElement[Double]]()
    val elem = DummyElement(Array(0, 0, 0))
    linLs.insert(elem)
    linLs.toSet should be(Set(elem))
  }
  
  test("Insertion of a non-dominating element should not remove other elements") {
    val linLs = LinearListDouble[ParetoElement[Double]]()
    val elem1 = DummyElement(Array(0, 0, 0))
    val elem2 = DummyElement(Array(1, 0, -1))
    val elem3 = DummyElement(Array(-1, 0, 1))
    val elem4 = DummyElement(Array(0, 1, -1))
    linLs.insert(elem1)
    linLs.insert(elem2)
    linLs.insert(elem3)
    linLs.insert(elem4)
    linLs.toSet should be(Set(elem1, elem2, elem3, elem4))
  }
  
  test("Insertion of a dominated element should not change the Pareto front") {
    val linLs = LinearListDouble[ParetoElement[Double]]()
    val elem1 = DummyElement(Array(0, 0, 0))
    val elem2 = DummyElement(Array(1, 0, -1))
    val elem3 = DummyElement(Array(-1, 0, 1))
    val elem4 = DummyElement(Array(0, 1, -1))
    val dominated = DummyElement(Array(1, 1, 1))
    linLs.insert(elem1)
    linLs.insert(elem2)
    linLs.insert(elem3)
    linLs.insert(elem4)
    linLs.toSet should be(Set(elem1, elem2, elem3, elem4))
    linLs.insert(dominated)
    linLs.toSet should be(Set(elem1, elem2, elem3, elem4))    
  }
}

