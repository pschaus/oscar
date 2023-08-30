package oscar.cp.modeling


import oscar.cp.core.{CPPropagStrength, CPSolver, Constraint}
import oscar.cp.core.CPPropagStrength._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar
import oscar.algo.search._

import scala.reflect.ClassTag


trait CPSolverUtils {
  
    // helper functions to model with an implicit CPSolver
  def add(constraints: Iterable[_ <: Constraint], propagStrengh: CPPropagStrength)(implicit cp: CPSolver): Unit = cp.add(constraints,propagStrengh)
  def add(constraints: Iterable[_ <: Constraint])(implicit cp: CPSolver): Unit = cp.add(constraints)

  def add[T: ClassTag](constraints: Iterable[_ <: CPBoolVar])(implicit cp: CPSolver): Unit = cp.add(constraints)

  def add(c: Constraint, propagStrengh: CPPropagStrength)(implicit cp: CPSolver): Unit = cp.add(c, propagStrengh)
  def add(c: Constraint)(implicit cp: CPSolver): Unit = cp.add(c)

  def add(c: CPBoolVar)(implicit cp: CPSolver): Unit = cp.add(c)

  def post(c: CPBoolVar)(implicit cp: CPSolver): Unit = cp.post(c)

  def post(c: Constraint, propagStrengh: CPPropagStrength = Weak)(implicit cp: CPSolver): Unit = cp.post(c, propagStrengh)
  def post(c: Constraint)(implicit cp: CPSolver): Unit = cp.post(c)

  def search(branching: Branching)(implicit cp: CPSolver) = cp.search(branching)

  def search(block: => Seq[Alternative])(implicit cp: CPSolver) = cp.search(block)

  def minimize(obj: CPIntVar)(implicit cp: CPSolver): CPSolver = cp.minimize(obj)
  def maximize(obj: CPIntVar)(implicit cp: CPSolver): CPSolver = cp.maximize(obj)

  def onSolution(block: => Unit)(implicit cp: CPSolver) = cp.onSolution(block)

  def start(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue, searchListener: DFSearchListener = null)(implicit cp: CPSolver): SearchStatistics = {
    cp.start(nSols,failureLimit,timeLimit,maxDiscrepancy,searchListener)
  }

  def start(stopCondition: => Boolean)(implicit cp: CPSolver): SearchStatistics = {
    cp.start(stopCondition)
  }

  def startSubjectTo(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue, searchListener: DFSearchListener = null)(reversibleBlock: => Unit = {})(implicit cp: CPSolver): SearchStatistics = {
    cp.startSubjectTo(nSols,failureLimit,timeLimit,maxDiscrepancy, searchListener)(reversibleBlock)
  }

}