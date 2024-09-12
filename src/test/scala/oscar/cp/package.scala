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

package oscar

import oscar.cp.constraints.InSet
import oscar.cp.constraints.ModuloLHS
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPBoolVarImpl
import oscar.cp.core.variables.CPIntVarViewMinus
import oscar.cp.modeling.Branchings
import oscar.cp.modeling.CPSolverUtils
import oscar.cp.modeling.Constraints
import oscar.cp.modeling.ElementBuilder
import oscar.cp.modeling.LNSRelaxations

import oscar.algo._
import oscar.cp.core.variables.CPVar
/**
 * The `cp` package provides useful functionalities to model problem using
 * the OscaR Constraint Programming Library.
 *
 * === Commonly Used Types ===
 * This package provides type aliases for types which are commonly used,
 * such as `CPSolver`, `CPIntVar`, or `CPIntervalVar`.
 *
 * === Implicit Conversions ===
 * A number of commonly applied implicit conversions are also defined here.
 * Implicit conversions provide additional higher-order functions to core classes
 * such as `CPIntVar`, `CPIntervalVar`, or `CPSolver`. Implicit conversion also provide
 * simple and natural modeling functionalities for sum and element constraints.
 *
 * === CPModel ===
 * The `CPModel` trait is also defined in this package and provides users with an
 * implicit `CPSolver` named solver. The use of `CPModel` allows users to model
 * problems without considering the underlying solver.
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
package object cp extends Constraints with Branchings with ElementBuilder with CPSolverUtils with LNSRelaxations {
  // Alias to useful classes and companion objects
  type CPIntVar = oscar.cp.core.variables.CPIntVar
  final val CPIntVar = oscar.cp.core.variables.CPIntVar

  type CPBoolVar = oscar.cp.core.variables.CPBoolVar
  final val CPBoolVar = oscar.cp.core.variables.CPBoolVar

  type CPSetVar = oscar.cp.core.variables.CPSetVar
  final val CPSetVar = oscar.cp.core.variables.CPSetVar

  type CPGraphVar = oscar.cp.core.variables.CPGraphVar
  final val CPGraphVar = oscar.cp.core.variables.CPGraphVar

  type CPStore = oscar.cp.core.CPStore
  final val CPStore = oscar.cp.core.CPStore

  type CPSolver = oscar.cp.core.CPSolver
  final val CPSolver = oscar.cp.core.CPSolver

  type Constraint = oscar.cp.core.Constraint

  type NoSolutionException = oscar.cp.core.NoSolutionException

  trait CPModel { implicit val solver: CPSolver = CPSolver() }

  /**
   * Filtering power can be specified for some of the constraints.
   * The default filtering is Weak.
   */
  val Strong = CPPropagStrength.Strong
  val Medium = CPPropagStrength.Medium
  val Weak = CPPropagStrength.Weak

  object TightenType extends Enumeration {
    val WeakTighten = Value("weak tighten")
    val StrongTighten = Value("strong tighten")
    val NoTighten = Value("no tighten")
    val MaintainTighten = Value("maintain tighten")
  }
  import TightenType._

  // TODO Dangerous implicits
  implicit def convert2(vals: IndexedSeq[Int]) = vals.toArray[Int]
  implicit def indexed2Array(x: IndexedSeq[CPIntVar]) = x.toArray[CPIntVar]
  implicit def args2Array(x: CPIntVar*) = x.toArray[CPIntVar]
  implicit def indexed2ArrayBool(x: IndexedSeq[CPBoolVar]) = x.toArray[CPBoolVar]
  implicit def args2ArrayBool(x: CPBoolVar*) = x.toArray[CPBoolVar]

  // TODO Should move to oscar.cp.util
  implicit def arrayVar2IterableVarOps(s: Array[CPIntVar]) = new IterableVarOps(s)
  implicit class IterableVarOps(val seq: Iterable[CPIntVar]) extends AnyVal {

    /** @return true is all the variables are bound */
    def areBound: Boolean = seq.forall(_.isBound)

    /** @return one unbound variable with minimum domain (randomly chosen is several of them) */
    def minDomNotBound: CPIntVar = {
      val res: Option[(CPIntVar, Int)] = selectMin(seq.zipWithIndex)(x => !x._1.isBound)(y => (y._1.size, y._2))
      res match {
        case Some((x, i)) => x
        case None         => throw new java.util.NoSuchElementException("no unbound var")
      }
    }

    /** @return the maximum value taken a bound variable or v if no variable is bound */
    def maxBoundOrElse(v: Int): Int = {
      val res: Option[CPIntVar] = selectMin(seq)(_.isBound)(-_.value)
      res match {
        case Some(x) => x.value
        case None    => v
      }
    }
  }

  //class LeSymbolic(val x: CPIntVarOps,val y : CPIntVarOps)

  //implicit def LeSymbolicToConstraint(cons: LeSymbolic): Constraint =  new oscar.cp.constraints.Le(cons.x.x, cons.y.x)

  //implicit def LeSymbolicToBoolean(cons: LeSymbolic): CPBoolVar =  cons.x.x.isLeEq(cons.y.x - 1)


  
  implicit class CPIntVarOps(val x: CPIntVar) {

    /**
     * @return difference between second smallest and smallest value in the domain, Int.MaxInt if variable is bound
     */
    def regret: Int = {
      if (x.isBound) Int.MaxValue
      else {
        val min = x.min
        x.valueAfter(min) - min
      }
    }
    
    def maxRegret(costs: Array[Int]): Int = {
      val values = x.toArray
      var min1 = costs(values(0))
      var min2 = Int.MaxValue
      var i = values.length
      while (i > 1) {
        i -= 1
        val value = values(i)
        val cost = costs(value)
        if (cost <= min1) {
          min2 = min1
          min1 = cost 
        } else if (cost < min2) min2 = cost
      }
      min2 - min1
    }
    
    def maxRegret(costFunction: Int => Int): Int = {
      val values = x.toArray
      var min1 = costFunction(values(0))
      var min2 = Int.MaxValue
      var i = values.length
      while (i > 1) {
        i -= 1
        val value = values(i)
        val cost = costFunction(value)
        if (cost <= min1) {
          min2 = min1
          min1 = cost 
        } else if (cost < min2) min2 = cost
      }
      min2 - min1
    }
    
    def minBy(costs: Array[Int]): Int = {
      val values = x.toArray
      var i = values.length
      var minValue = values(0)
      var minCost = costs(minValue)
      while (i > 1) {
        i -= 1
        val value = values(i)
        val cost = costs(value)
        if (cost < minCost) {
          minValue = value
          minCost = cost
        }
      }
      minValue
    }

    /**
     * @return The median value of the domain of the variable
     */
    def median: Int = {
      val vals = x.toArray.sortBy(i => i)
      vals(vals.length / 2)
    }

    /**
     *  Returns the value assigned to the variable.
     *  Throws an Exception if the variable is not assigned.
     */
    def value: Int = {
      if (x.isBound) x.min
      else throw new NoSuchElementException("the variable is not bound")
    }

    /**
     * -x
     */
    def unary_-() = new CPIntVarViewMinus(x)
    /**
     * x+y
     */
    def +(y: CPIntVar) = plus(x, y)
    /**
     * x-y
     */
    def -(y: CPIntVar) = minus(x, y)
    /**
     * x+y
     */
    def +(y: Int) = plus(x, y)
    /**
     * x-y
     */
    def -(y: Int) = minus(x, y)

    def +(s: String) = s"$x$s"

    /**
     * x*y
     */
    def *(y: CPIntVar): CPIntVar = {
      if (y.isBound) x * (y.value)
      else mul(x, y)
    }
    /**
     * x*y
     */
    def *(y: Int): CPIntVar = mul(x, y)

    def abs = absolute(x)

    /**
     * Reified constraint
      *
      * @param y a variable
     * @return a boolean variable b in the same store linked to x by the relation x == y <=> b == true
     */
    def isEq(y: CPIntVar): CPBoolVar = {
      val b = CPBoolVar()(x.store)
      x.store.post(new oscar.cp.constraints.EqReifVar(x, y, b))
      b
    }



    /**
     * x must take a value from set
     */
    def in(set: Set[Int]): Constraint = new InSet(x, set)

    /**
     * x<y
     */
    def <(y: CPIntVar) = new oscar.cp.constraints.Le(x, y)
    /**
     * x<y
     */
    def <(y: Int) = new oscar.cp.constraints.Le(x, y)
    /**
     * x>y
     */
    def >(y: CPIntVar) = new oscar.cp.constraints.Gr(x, y)
    /**
     * x>y
     */
    def >(y: Int) = new oscar.cp.constraints.Gr(x, y)
    /**
     * x<=y
     */
    def <=(y: CPIntVar) = new oscar.cp.constraints.LeEq(x, y)
    /**
     * x<=y
     */
    def <=(y: Int) = new oscar.cp.constraints.LeEq(x, y)
    /**
     * x>=y
     */
    def >=(y: CPIntVar) = new oscar.cp.constraints.GrEq(x, y)
    /**
     * x>=y
     */
    def >=(y: Int) = new oscar.cp.constraints.GrEq(x, y)

    /**
     * x == v
     */
    def ===(v: Int) = x.eq(v)

    /**
      * x == y
      */
    def ===(y: CPIntVar) = x.eq(y)

    /**
      * x != v
      */
    def !==(v: Int) = x.diff(v)

    /**
      * x == y
      */
    def !==(y: CPIntVar) = x.diff(y)



    def %(y: Int) = ModuloLHS(x, y)

    def mod(y: Int) = modulo(x, y)
    
    // New experimental function names for reification 
    
    /**
     * b <=> x == v
     */
    def ?=== (v: Int) = x.isEq(v)

    /**
     * b <=> x == y
     */
    def ?=== (y: CPIntVar) = x.isEq(y)

    /**
     * b <=> x!= y
     */
    def ?!== (y: CPIntVar) = x.isDiff(y)

    /**
     * b <=> x!= y
     */
    def ?!== (y: Int) = x.isDiff(y)

    /**
     * b <=> x >= y
     */
    def ?>= (y: Int) = x.isGrEq(y)

    /**
     * b <=> x >= y
     */
    def ?>= (y: CPIntVar) = x.isGrEq(y)

    /**
     * b <=> x > y
     */
    def ?> (y: Int) = x.isGr(y)

    /**
     * b <=> x > y
     */
    def ?> (y: CPIntVar) = x.isGr(y)

    /**
     * b <=> x >= y
     */
    def ?<= (y: Int) = x.isLeEq(y)

    /**
     * b <=> x >= y
     */
    def ?<= (y: CPIntVar) = x.isLeEq(y)

    /**
     * b <=> x > y
     */
    def ?< (y: Int) = x.isLe(y)

    /**
     * b <=> x > y
     */
    def ?< (y: CPIntVar) = x.isLe(y)
  }

  implicit class CPBoolVarOps(val variable: CPBoolVar) extends AnyVal {

    /** Logical or */
    def or(y: CPBoolVar): CPBoolVar = {
      val b = CPBoolVarImpl(variable.store, "")
      variable.store.post(new oscar.cp.constraints.OrReif2(Array(variable, y), b))
      b
    }

    /** Logical and */
    def and(y: CPBoolVar): CPBoolVar = {
      val res = plus(variable, y)
      res.isEq(2)
    }

    def implies(y: CPBoolVar) = {
      val b = CPBoolVarImpl(variable.store, "")
      variable.store.post(new oscar.cp.constraints.Implication(variable, y, b))
      b
    }

    /** !b */
    def unary_!(): CPBoolVar = variable.not

    /** x | y */
    def |(y: CPBoolVar) = or(y)

    /** x || y */
    def ||(y: CPBoolVar) = or(y)

    /** x & y */
    def &(y: CPBoolVar) = and(y)

    /** x && y */
    def &&(y: CPBoolVar) = and(y)

    /** x ==> y */
    def ==>(y: CPBoolVar) = implies(y)
  }

  def allBounds(vars: Iterable[_ <: CPVar]) = vars.asInstanceOf[Iterable[CPVar]].forall(_.isBound)
}
