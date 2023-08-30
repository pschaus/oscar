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
import oscar.algo.paretofront.QTNode
import oscar.algo.paretofront.QTMinDouble


/**
 * @author : Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 * @author : Renaud Hartert ren.hartert@gmail.com
 */
class DummyQTNode(evals: Array[Double]) extends QTNode[Double] {
  override def objectives = evals
  override def coordinates = evals
}

object DummyQTNode {
  def apply(evals: Array[Double]) = new DummyQTNode(evals)
}

class QuadTreeTest extends FunSuite with Matchers {
  
  val randGen = new util.Random
  
  def genRandomPoint(intervals: Array[(Double, Double)]): Array[Double] = {
    Array.tabulate(intervals.length)(i => intervals(i)._1 + (intervals(i)._2 - intervals(i)._1) * randGen.nextDouble)
  }

  test("Inserted element in an empty QT should be root") {
    val qt = QTMinDouble(3)
    val elem = DummyQTNode(Array(0, 0, 0))
    qt.insert(elem)
    qt.root.get should be(elem)
  }
  
  test("Insertion of a non-dominating element should not change the root") {
    val qt = QTMinDouble(3)
    val root = DummyQTNode(Array(0, 0, 0))
    val newElem = DummyQTNode(Array(-1, 0, 1))
    qt.insert(root)
    qt.root.get should be(root)
    qt.insert(newElem)
    qt.root.get should be(root)
  }
  
  test("Insertion of a non-dominat[ing/ed] element should add it in the QT") {
    val qt = QTMinDouble(3)
    val root = DummyQTNode(Array(0, 0, 0))
    val newElem = DummyQTNode(Array(-1, 0, 1))
    qt.insert(root)
    qt.toSet should be(Set(root))
    qt.insert(newElem)
    qt.toSet should be(Set(root, newElem))
  }
  
  test("Insertion of a dominated element should not alter the QT") {
    val qt = QTMinDouble(3)
    val root = DummyQTNode(Array(0, 0, 0))
    val newElem = DummyQTNode(Array(1, 0, 0))
    qt.insert(root)
    qt.toSet should be(Set(root))
    qt.insert(newElem)
    qt.toSet should be(Set(root))
  }
  
  
  test("Insertion of a non-dominat[ing/ed] element should be inserted into the adequate quadrant") {
    val qt = QTMinDouble(3)
    val root = DummyQTNode(Array(0, 0, 0))
    val son001 = DummyQTNode(Array(1, 1, -2))
    val son011 = DummyQTNode(Array(1, -1, -1))
    val son101 = DummyQTNode(Array(-1, 1, -1))
    val son010 = DummyQTNode(Array(1, -2, 1))
    val son110 = DummyQTNode(Array(-1, -1, 1))
    val son100 = DummyQTNode(Array(-2, 1, 1))
    qt.insert(root)
    qt.insert(son001)
    qt.insert(son011)
    qt.insert(son101)
    qt.insert(son010)
    qt.insert(son110)
    qt.insert(son100)
    root.successors(0) should be (None)
    root.successors(1).get should be (son001)
    root.successors(2).get should be (son010)
    root.successors(3).get should be (son011)
    root.successors(4).get should be (son100)
    root.successors(5).get should be (son101)
    root.successors(6).get should be (son110)
    root.successors(7) should be (None)
  }
  
  test("Insertion of a dominating element should remove dominated elements") {
    val qt = QTMinDouble(3)
    val root = DummyQTNode(Array(0, 0, 0))
    val elem = DummyQTNode(Array(1, 1, -1))
    val dominator = DummyQTNode(Array(1, 1, -2))
    qt.insert(root)
    qt.toSet should be(Set(root))
    qt.insert(elem)
    qt.toSet should be(Set(root, elem))
    qt.insert(dominator)
    qt.toSet should be(Set(root, dominator))
  }
  
  test("Insertion of a dominating element should replace dominated element and contains non-dominated sons of the discarded element") {
    val qt = QTMinDouble(3)
    val root = DummyQTNode(Array(0, 0, 0))
    val elemL11 = DummyQTNode(Array(1, 1, -1))
    val elemL21 = DummyQTNode(Array(2, 0.5, -1))
    val elemL22 = DummyQTNode(Array(0.5, 2, -1))
    val elemL23 = DummyQTNode(Array(1, 1.5, -2))
    val elemL24 = DummyQTNode(Array(1.5, 1, -2))
    val dominator = DummyQTNode(Array(1, 1, -3))
    qt.insert(root)
    qt.toSet should be(Set(root))
    qt.insert(elemL11)
    qt.toSet should be(Set(root, elemL11))
    qt.insert(elemL21)
    qt.insert(elemL22)
    qt.insert(elemL23)
    qt.insert(elemL24)
    qt.toSet should be(Set(root, elemL11, elemL21, elemL22, elemL23, elemL24))
    elemL11.successorsToSet should be (Set(elemL21, elemL22, elemL23))
    qt.insert(dominator)
    qt.toSet should be(Set(root, dominator, elemL21, elemL22))
    root.successorsToSet should be (Set(dominator))
  }

}

