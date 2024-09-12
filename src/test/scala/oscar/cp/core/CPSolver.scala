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

package oscar.cp.core

import oscar.algo.search.{DFSLinearizer, DFSReplayer, _}
import oscar.cp._
import oscar.cp.isInconsistent
import oscar.cp.core._
import oscar.cp.constraints._

import scala.collection.mutable.Stack
import oscar.algo.reversible._
import oscar.cp.multiobjective.ListPareto
import oscar.cp.multiobjective.Pareto
import oscar.cp.constraints.ParetoConstraint
import java.util.LinkedList
import java.util.Collection

import scala.reflect.ClassTag

class CPSolver(propagStrength: CPPropagStrength) extends CPOptimizer(propagStrength) {


  def this() = this(CPPropagStrength.Automatic)

  override def startSubjectTo(stopCondition: DFSearch => Boolean, maxDiscrepancy: Int, listener: DFSearchListener)(block: => Unit): SearchStatistics = {
    deactivateNoSolExceptions()
    val stat = super.startSubjectTo(stopCondition,maxDiscrepancy,listener)(block)
    cleanQueues()
    stat
  }

  //the solution variables are the variables that must be assigned to have a solution
  final def replay(dfsLinearizer: DFSLinearizer, solutionVariables: Seq[CPIntVar]): SearchStatistics = {
    replaySubjectTo(dfsLinearizer,solutionVariables){}
  }

  final def replaySubjectTo(dfsLinearizer: DFSLinearizer, solutionVariables: Seq[CPIntVar], timeLimit: Int = Int.MaxValue)(block: => Unit): SearchStatistics = {
    pushState() // Store the current state
    block
    val stats = new DFSReplayer(this, solutionVariables).replay(dfsLinearizer.decisions, timeLimit)
    pop()
    stats
  }



  @inline private def buildStopCondition(nSols: Int, failureLimit: Int, timeLimit: Int): Function1[DFSearch, Boolean] = {
    // Build the stop condition
    val checkSol = nSols < Int.MaxValue
    val checkFailures = failureLimit < Int.MaxValue
    val checkTime = timeLimit < Int.MaxValue
    val maxTime = (timeLimit * 1000) + System.currentTimeMillis()
    (s: DFSearch) => {
      var stop = false
      stop |= (checkSol && s.nSolutions >= nSols)
      stop |= (checkFailures && s.nBacktracks >= failureLimit)
      stop |= (checkTime && System.currentTimeMillis() >= maxTime)
      stop
    }
  }


  private val decVariables = scala.collection.mutable.Set[CPIntVar]()

  var lastSol = new CPSol(Set[CPIntVar]())

  def addDecisionVariables(x: Iterable[_ <: CPIntVar]): Unit = x.foreach(decVariables += _)

  def addDecisionVariables(x: CPIntVar*): Unit = x.foreach(decVariables += _)

  private def recordSol(): Unit = {
    lastSol = new CPSol(decVariables.toSet)
  }

  private var throwNoSolExceptions = true

  /** Deactivate the no solution exception when an add is used and an inconsistent model is detected */
  def deactivateNoSolExceptions(): Unit = throwNoSolExceptions = false
  def activateNoSolExceptions(): Unit = throwNoSolExceptions = true

  /**
   * return true if every variable is bound
   */
  def allBounds(vars: IndexedSeq[_ <: CPIntVar]): Boolean = {
    var i = 0
    val s = vars.size
    while (i < s) {
      if (!vars(i).isBound) return false
      i += 1
    }
    true
  }

  override def minimize(objective: CPIntVar): CPSolver = {
    super.minimize(Seq(objective): _*)
    this
  }

  override def minimize(objective: CPIntVar, ratio: Double): CPSolver = {
    super.minimize(objective, ratio)
    this
  }

  override def maximize(objective: CPIntVar): CPSolver = {
    super.maximize(Seq(objective): _*)
    this
  }

  override def update(): Unit = propagate()

  override def solFound(): Unit = {
    super.solFound()
    lastSol = new CPSol(decVariables.toSet)
    if (recordNonDominatedSolutions) {
      if (!silent) println("new solution:" + objective.objs.map(_.objVar.min).toArray.mkString(","))
      paretoSet.insert(lastSol, objective.objs.map(_.objVar.min): _*)
    }
    objective.tighten()
  }

  override def add(c: Constraint, st: CPPropagStrength): Unit = {
    //TODO GUILLAUME est-ce qu'on doit catch l'inconsistency ici??
    val inconsistent = isInconsistent(post(c, st))
    if ((inconsistent || isFailed) && throwNoSolExceptions) {
      throw new NoSolutionException(s"the stored failed when adding constraint $c")
    }
  }

  override def add(c: Constraint): Unit = add(c, propagStrength)

  /**
   * Add a constraint to the store (b == true) in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty
   */
  override def add(b: CPBoolVar): Unit = {
    //TODO GUILLAUME est-ce qu'on doit catch l'inconsistency ici??
    val inconsistent = isInconsistent(post(b.constraintTrue))
    if ((inconsistent || isFailed) && throwNoSolExceptions) {
      throw new NoSolutionException(s"the stored failed when setting " + b.name + " to true")
    }
  }
    
  override def addCut(c: Constraint): Unit = {
    //TODO GUILLAUME est-ce qu'on doit catch l'inconsistency ici??
    val inconsistent = isInconsistent(postCut(c))
    if ((inconsistent || isFailed) && throwNoSolExceptions) {
      throw new NoSolutionException(s"the stored failed when adding constraint $c")
    }
  }

  /**
   * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
    *
    * @param constraints
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
   * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  override def add(constraints: Array[Constraint], st: CPPropagStrength): Unit = {
    //TODO GUILLAUME est-ce qu'on doit catch l'inconsistency ici??
    val inconsistent = isInconsistent(post(constraints, st))
    if ((inconsistent|| isFailed) && throwNoSolExceptions) {
      throw new NoSolutionException(s"the stored failed when adding constraint $constraints");
    }
  }
  
  override def add(constraints: Array[Constraint]): Unit = add(constraints, propagStrength)

  override def add(constraints: Iterable[Constraint], st: CPPropagStrength): Unit = add(constraints.toArray, st)

  override def add[T: ClassTag](boolVars: Iterable[CPBoolVar]): Unit = {
    //TODO GUILLAUME est-ce qu'on doit catch l'inconsistency ici??
    val inconsistent = isInconsistent(post(boolVars))
    if ((inconsistent || isFailed) && throwNoSolExceptions) {
      throw new NoSolutionException(s"the stored failed when setting those boolVars to true and propagate $boolVars");
    }
  }

  override def add(constraints: Iterable[Constraint]): Unit = add(constraints.toArray, propagStrength)

  override def +=(c: Constraint, st: CPPropagStrength): Unit = add(c, st)
  override def +=(c: Constraint): Unit = add(c, propagStrength)
}

object CPSolver {

  /** Creates a new CP Solver */
  def apply(): CPSolver = new CPSolver()

  /** Creates a new CP Solver with `propagStrength` as default level of propagation */
  def apply(propagStrength: CPPropagStrength) = new CPSolver(propagStrength)

  /** Creates a new CP Solver with `Weak` as default level of propagation */
  def weak: CPSolver = new CPSolver(CPPropagStrength.Weak)

  /** Creates a new CP Solver with `Medium` as default level of propagation */
  def medium: CPSolver = new CPSolver(CPPropagStrength.Medium)

  /** Creates a new CP Solver with `Strong` as default level of propagation */
  def strong: CPSolver = new CPSolver(CPPropagStrength.Strong)
}