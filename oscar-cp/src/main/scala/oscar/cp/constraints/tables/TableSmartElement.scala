/** *****************************************************************************
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
 * **************************************************************************** */

package oscar.cp.constraints.tables

import oscar.cp.core.variables.CPIntVar

import scala.collection.mutable.ArrayBuffer

/**
 * Representations of the smart element and basic smart elements
 *
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
object SmartElement {
  val nbClassSmartElement = 11
  val unsortedID = -1
  val equalID = 0
  val notEqualID = 1
  val inSetID = 2
  val notInSetID = 3
  val starID = 4
  val lessEqID = 5
  val greatEqID = 6
  val equalXID = 7
  val notEqualXID = 8
  val lessEqXID = 9
  val greatEqXID = 10

  val listBasicSmartElement = List(equalID, notEqualID, inSetID, notInSetID, starID, lessEqID, greatEqID)
  val listNotBasicSmartElement = List(equalXID, notEqualXID, lessEqXID, greatEqXID)

  /**
   * Compress the given values into basic smart element =, !=, *, <=, >=, \in S
   * in accordance to a given domain. Try to find the best decomposition.
   * First try *, then !=, then if the values match the tails of the domains <= and >=, and finally put the rest in a set.
   *
   * @param domain  the given domain
   * @param values  the values to be compressed
   * @param withSet if true, use \in S, if false doesn't produce \in S
   * @return a list of basic smart element
   */
  def compressToBasicSmartElement(domain: Array[Int], values: Array[Int], withSet: Boolean): Array[BasicSmartElement] = {
    if (withSet)
      compressToBasicSmartElementWithSet(domain, values)
    else
      compressToBasicSmartElementWithoutSet(domain, values)
  }

  private def compressToBasicSmartElementWithoutSet(domain: Array[Int], values: Array[Int]): Array[BasicSmartElement] = {
    val _domain = domain.sorted
    val domSize = _domain.length
    val _values = values.sorted
    val nbValues = _values.length
    val buff = new ArrayBuffer[BasicSmartElement]()
    if (nbValues == 1)
      buff += Equal(_values(0))
    else if (nbValues == domSize)
      buff += Star()
    else if (_values.length == domSize - 1) {
      var lb = 0
      var ub = _values.length
      while (lb != ub) {
        val mid = (lb + ub) / 2
        if (_domain(mid) == _values(mid))
          lb = mid + 1
        else
          ub = mid
      }
      buff += NotEqual(_domain(lb))

    } else {
      var lb = 0
      var ub = nbValues
      while (lb != ub) {
        val mid = (lb + ub) / 2
        if (_values(mid) > _domain(mid))
          ub = mid
        else
          lb = mid + 1
      }
      var LB = lb - 1

      val diff = domSize - nbValues
      lb = -1
      ub = nbValues - 1
      while (lb != ub) {
        val mid = (lb + ub + 1) / 2
        if (_values(mid) < _domain(mid + diff))
          lb = mid
        else
          ub = mid - 1
      }
      var UB = diff + ub + 1

      if (LB > 0)
        buff += LessEq(_domain(LB))
      else
        LB = -1
      if (UB < domSize - 1)
        buff += GreatEq(_domain(UB))
      else
        UB = domSize

      for (i <- LB + 1 until UB - diff)
        buff += Equal(values(i))

    }
    buff.toArray
  }

  private def compressToBasicSmartElementWithSet(domain: Array[Int], values: Array[Int]): Array[BasicSmartElement] = {
    val _domain = domain.sorted
    val domSize = _domain.length
    val _values = values.sorted
    val nbValues = _values.length
    val buff = new ArrayBuffer[BasicSmartElement]()
    if (nbValues == 1)
      buff += Equal(_values(0))
    else if (nbValues == domSize)
      buff += Star()
    else if (_values.length == domSize - 1) {
      var lb = 0
      var ub = _values.length
      while (lb != ub) {
        val mid = (lb + ub) / 2
        if (_domain(mid) == _values(mid))
          lb = mid + 1
        else
          ub = mid
      }
      buff += NotEqual(_domain(lb))
    } else {
      var lb = 0
      var ub = nbValues
      while (lb != ub) {
        val mid = (lb + ub) / 2
        if (_values(mid) > _domain(mid))
          ub = mid
        else
          lb = mid + 1
      }
      var LB = lb - 1

      val diff = domSize - nbValues
      lb = -1
      ub = nbValues - 1
      while (lb != ub) {
        val mid = (lb + ub + 1) / 2
        if (_values(mid) < _domain(mid + diff))
          lb = mid
        else
          ub = mid - 1
      }
      var UB = diff + ub + 1

      if (LB > 0)
        buff += LessEq(_domain(LB))
      else
        LB = -1
      if (UB < domSize - 1)
        buff += GreatEq(_domain(UB))
      else
        UB = domSize

      val set = scala.collection.mutable.Set[Int]()
      for (i <- LB + 1 until UB - diff) {
        set.add(values(i))
      }
      if (set.size == 1)
        buff += Equal(values(LB + 1))
      else if (set.size > 1)
        buff += InSet(set.toSet)
    }
    buff.toArray
  }

  def getSortingTuple(element: BasicSmartElement): (Int, String) = {
    element match {
      case Equal(v) => (element.classId, v.toString)
      case NotEqual(v) => (element.classId, v.toString)
      case LessEq(v) => (element.classId, v.toString)
      case GreatEq(v) => (element.classId, v.toString)
      case Star() => (element.classId, "0")
      case InSet(values) => (element.classId,values.toArray.sorted.mkString(","))
      case NotInSet(values) => (element.classId,values.toArray.sorted.mkString(","))
    }

  }
}

trait SmartElement {
  // Id of the kind of smart element
  val classId: Int

  /**
   * Check if there exist at least one support
   *
   * @return true if there is, false if not
   */
  def isValid(vr: CPIntVar): Boolean

  /**
   * Check if this accept v as value for the variable
   *
   * @param v value
   * @return true if v is possible, false otherwise
   */
  def accept(v: Int): Boolean

  /**
   * Create a new smart element associated to a variable y = associatedVar - offset
   *
   * @param offset value linking the variables
   * @return the smart element with the offset
   */
  def applyOffset(offset: Int): SmartElement

  /**
   * Return a single value, used for sorting tuples
   *
   * @return a value
   */
  def getValue: Int = 0

  /**
   * Compute the number of ground values represented by this
   *
   * @return the number of ground values
   */
  def groundCount(vr: CPIntVar) = 1

  /**
   * Refactor this in a * is there is equivalence
   *
   * @return a smart element either this, either Star()
   */
  def refactorStar(vr: CPIntVar) = this

  /**
   * Apply the function fun to all the values of the smart element valid regarding to the variable
   */
  def foreach(vr: CPIntVar, fun: Int => Unit): Unit
}

/**
 * Trait allowing to make a class distinction between smart element and basic smart element
 */
trait BasicSmartElement extends SmartElement

/**
 * Smart Element representing '= v'
 *
 * @param value associated value
 */
case class Equal(value: Int) extends BasicSmartElement {
  val classId: Int = SmartElement.equalID

  def isValid(vr: CPIntVar): Boolean = {
    vr.hasValue(value)
  }

  def accept(v: Int): Boolean = {
    v == value
  }

  def applyOffset(offset: Int): SmartElement = {
    Equal(value - offset)
  }

  override def getValue: Int = {
    value
  }

  def foreach(vr: CPIntVar, fun: Int => Unit): Unit = {
    if (isValid(vr))
      fun(value)
  }

  override def toString: String = {
    "=" + value
  }
}

/**
 * Smart Element representing '!= v'
 *
 * @param value associated value
 */
case class NotEqual(value: Int) extends BasicSmartElement {
  val classId: Int = SmartElement.notEqualID

  def isValid(vr: CPIntVar): Boolean = {
    !vr.isBoundTo(value)
  }

  def accept(v: Int): Boolean = {
    v != value
  }

  def applyOffset(offset: Int): SmartElement = {
    NotEqual(value - offset)
  }

  override def getValue: Int = {
    value
  }

  override def groundCount(vr: CPIntVar): Int = {
    vr.size - (if (this.isValid(vr)) 1 else 0)
  }

  def foreach(vr: CPIntVar, fun: Int => Unit): Unit = {
    for (v <- vr.iterator; if value != v)
      fun(v)
  }

  override def toString: String = {
    "!=" + value
  }
}

/**
 * Smart Element representing '\in S'
 *
 * @param values associated values
 */
case class InSet(values: Set[Int]) extends BasicSmartElement {
  val classId: Int = SmartElement.inSetID

  val hash: Long = values.foldLeft(0L)((a, b) => a + 2 ^ b)

  def isValid(vr: CPIntVar): Boolean = {
    values.exists(value => vr.hasValue(value))
  }

  def accept(v: Int): Boolean = {
    values.contains(v)
  }

  def applyOffset(offset: Int): SmartElement = {
    InSet(values.map(_ - offset))
  }

  override def groundCount(vr: CPIntVar): Int = {
    values.count(v => vr.hasValue(v))
  }

  def foreach(vr: CPIntVar, fun: Int => Unit): Unit = {
    for (v <- vr.iterator; if values.contains(v))
      fun(v)
  }

  override def toString: String = {
    "in" + values.mkString("{", ",", "}")
  }
}

/**
 * Smart Element representing '\not\in S'
 *
 * @param values associated values
 */
case class NotInSet(values: Set[Int]) extends BasicSmartElement {
  val classId: Int = SmartElement.notInSetID

  val hash: Long = values.foldLeft(0L)((a, b) => a + 2 ^ b)

  def isValid(vr: CPIntVar): Boolean = {
    vr.exists(value => !values.contains(value))
  }

  def accept(v: Int): Boolean = {
    !values.contains(v)
  }

  def applyOffset(offset: Int): SmartElement = {
    NotInSet(values.map(_ - offset))
  }

  override def groundCount(vr: CPIntVar) = {
    vr.size - values.count(v => vr.hasValue(v))
  }

  def foreach(vr: CPIntVar, fun: Int => Unit): Unit = {
    for (v <- vr.iterator; if !values.contains(v))
      fun(v)
  }

  override def toString: String =
    "!in" + values.mkString("{", ",", "}")
}

/**
 * Smart Element representing '*'
 */
case class Star() extends BasicSmartElement {
  val classId: Int = SmartElement.starID

  def isValid(vr: CPIntVar): Boolean = {
    vr.size > 0
  }

  def accept(v: Int): Boolean = {
    true
  }

  def applyOffset(offset: Int): SmartElement = {
    Star()
  }

  override def groundCount(vr: CPIntVar): Int = {
    vr.size
  }

  def foreach(vr: CPIntVar, fun: Int => Unit): Unit = {
    for (v <- vr.iterator)
      fun(v)
  }

  override def toString: String = {
    "*"
  }
}

/**
 * Smart Element representing '<= v'
 *
 * @param value associated value
 */
case class LessEq(value: Int) extends BasicSmartElement {
  val classId: Int = SmartElement.lessEqID

  def isValid(vr: CPIntVar): Boolean = {
    vr.min <= value
  }

  def accept(v: Int): Boolean = {
    v <= value
  }

  def applyOffset(offset: Int): SmartElement = {
    LessEq(value - offset)
  }

  override def getValue = {
    value
  }

  override def groundCount(vr: CPIntVar) = {
    vr.iterator.count(_ <= value)
  }

  override def refactorStar(vr: CPIntVar) = {
    if (vr.max <= value)
      Star()
    else
      this
  }

  def foreach(vr: CPIntVar, fun: Int => Unit): Unit = {
    for (v <- vr.iterator; if v <= value)
      fun(v)
  }

  override def toString: String = {
    "<=" + value
  }
}


object Less {
  /**
   * Smart Element representing '< v'
   *
   * @param value associated value
   */
  def apply(value: Int): BasicSmartElement = {
    LessEq(value - 1)
  }
}

/**
 * Smart Element representing '>= v'
 *
 * @param value associated value
 */
case class GreatEq(value: Int) extends BasicSmartElement {
  val classId: Int = SmartElement.greatEqID

  def isValid(vr: CPIntVar): Boolean = {
    vr.max >= value
  }

  def accept(v: Int): Boolean = {
    v >= value
  }

  def applyOffset(offset: Int): SmartElement = {
    GreatEq(value - offset)
  }

  override def getValue: Int = {
    value
  }

  override def groundCount(vr: CPIntVar): Int = {
    vr.iterator.count(_ >= value)
  }

  override def refactorStar(vr: CPIntVar) = {
    if (vr.min >= value)
      Star()
    else
      this
  }

  def foreach(vr: CPIntVar, fun: Int => Unit): Unit = {
    for (v <- vr.iterator; if v >= value)
      fun(v)
  }

  override def toString: String = {
    ">=" + value
  }
}

object Great {
  /**
   * Smart Element representing '> v'
   *
   * @param value associated value
   */
  def apply(value: Int): BasicSmartElement = {
    GreatEq(value + 1)
  }
}


// TODO Do the equivalent for the Smart Element

///**
// * Smart Element representing '= x + v'
// * @param variable associated var
// * @param x linked variable
// * @param value associated value
// */
//case class EqualX(variable: CPIntVar, x: CPIntVar, value: Int) extends SmartElement {
//  val classId: Int = SmartElement.equalXID
//  val associatedVar: CPIntVar = variable
//
//  def isValid: Boolean = {
//    associatedVar.exists(value1 => x.exists(value2 => value1 == value2 + value))
//  }
//
//  def accept(v: Int): Boolean = {
//    x.hasValue(v - value)
//  }
//
//  override def toString: String = {
//    "=" + variable.name + "+" + value
//  }
//}
//
///**
// * Smart Element representing '!= x + v'
// * @param variable associated var
// * @param x linked variable
// * @param value associated value
// */
//case class NotEqualX(variable: CPIntVar, x: CPIntVar, value: Int) extends SmartElement {
//  val classId: Int = SmartElement.notEqualXID
//  val associatedVar: CPIntVar = variable
//
//  def isValid: Boolean = {
//    associatedVar.exists(value1 => x.exists(value2 => value1 != value2 + value))
//  }
//
//  def accept(v: Int): Boolean = {
//    !x.hasValue(v - value)
//  }
//
//  override def toString: String = {
//    "!=" + variable.name + "+" + value
//  }
//}
//
///**
// * Smart Element representing '<= x + v'
// * @param variable associated var
// * @param x linked variable
// * @param value associated value
// */
//case class LessEqX(variable: CPIntVar, x: CPIntVar, value: Int) extends SmartElement {
//  val classId: Int = SmartElement.lessEqXID
//  val associatedVar: CPIntVar = variable
//
//  def isValid: Boolean = {
//    associatedVar.min <= x.max + value
//  }
//
//  def accept(v: Int): Boolean = {
//    v <= x.max + value
//  }
//
//  override def toString: String = {
//    "<=" + variable.name + "+" + value
//  }
//}
//
//
//object LessX {
//  /**
//   * Smart Element representing '< x + v'
//   * @param variable associated var
//   * @param x linked variable
//   * @param value associated value
//   */
//  def apply(variable: CPIntVar, x: CPIntVar, value: Int): SmartElement = {
//    LessEqX(variable, x, value - 1)
//  }
//}
//
///**
// * Smart Element representing '>= x + v'
// * @param variable associated var
// * @param x linked variable
// * @param value associated value
// */
//case class GreatEqX(variable: CPIntVar, x: CPIntVar, value: Int) extends SmartElement {
//  val classId: Int = SmartElement.greatEqXID
//  val associatedVar: CPIntVar = variable
//
//  def isValid: Boolean = {
//    associatedVar.max >= x.min + value
//  }
//
//  def accept(v: Int): Boolean = {
//    v >= x.max + value
//  }
//
//  override def toString: String = {
//    ">=" + variable.name + "+" + value
//  }
//}
//
//
//object GreatX {
//  /**
//   * Smart Element representing '> x + v'
//   * @param variable associated var
//   * @param x linked variable
//   * @param value associated value
//   */
//  def apply(variable: CPIntVar, x: CPIntVar, value: Int): SmartElement = {
//    GreatEqX(variable, x, value + 1)
//  }
//}