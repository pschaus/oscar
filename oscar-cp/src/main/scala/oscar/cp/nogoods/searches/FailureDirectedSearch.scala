package oscar.cp.nogoods.searches

import oscar.cp.isInconsistent
import oscar.cp.core.variables.CPIntVar
import oscar.cp.nogoods.decisions.Decision
import oscar.algo.array.ArrayStack
import oscar.algo.reversible.ReversibleInt

import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import oscar.cp.core.variables.CPBoolVar

/** @author Renaud Hartert ren.hartert@gmail.com */
class FailureDirectedSearch(variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends NogoodBranching {

  require(variables.length > 0, "no variable.")

  private[this] var alpha: Double = 0.99
  
  private[this] val store = variables(0).store
  private[this] val nVariables = variables.length

  private[this] var nAssigned = 0
  private[this] val depth = new ReversibleInt(store, -1)

  private[this] val allDecisions: ArrayStack[FDSChoice] = new ArrayStack(100)
  private[this] val stack: ArrayStack[FDSChoice] = new ArrayStack(100)
  private[this] val stackSizeRev = new ReversibleInt(store, 0)
  
  private[this] val sumRatings = new ArrayStack[Double](100)
  private[this] val nRatings = new ArrayStack[Int](100)

  private[this] val heap: PriorityQueue[FDSChoice] = PriorityQueue()(FDSChoiceOrdering)

  final override def nextDecision(): Decision = {
    updateAssigned()
    if (nAssigned == nVariables) null
    else {
      cleanStack()
      depth.incr()
      val nextFromHeap = selectFromHeap()
      val choice = if (nextFromHeap == null) newDecision() else nextFromHeap  
      stack.push(choice)
      stackSizeRev.value = stack.size
      choice.firstDecision
    }
  }

  @inline @tailrec final private def selectFromHeap(): FDSChoice = {
    if (heap.isEmpty) null
    else {
      val choice = heap.dequeue()
      if (!choice.isAssigned) choice
      else {
        stack.push(choice)
        selectFromHeap()
      }
    }
  }

  @inline private def newDecision(): FDSChoice = {
    val varId = nextVariable
    val value = valHeuristic(varId)
    val choice = new FDSChoice(variables(varId), value)
    allDecisions.push(choice)
    choice
  }
  
  @inline private def nextVariable: Int = {
    var min = Int.MaxValue
    var minId = -1
    var i = nVariables
    while (i > 0) {
      i -= 1
      val variable = variables(i)
      if (!variable.isBound) {
        val h = varHeuristic(i)
        if (h < min) {
          min = h
          minId = i
        }
      }
    }
    minId
  }

  @inline private def cleanStack(): Unit = {
    val stackSize = stackSizeRev.value
    while (stack.size > stackSize) {
      val decision = stack.pop()
      heap.enqueue(decision)
    }
  }

  @inline private def updateAssigned(): Unit = {
    var i = 0
    nAssigned = 0
    while (i < nVariables) {
      if (variables(i).isBound) nAssigned += 1
      i += 1
    }
  }
  
  @inline private def spaceSize: Double = {
    var size = 1
    var i = nVariables
    while (i > 0) {
      i -= 1
      size *= variables(i).size
    }
    size
  }
  
  @inline private def averageRating: Double = {
    var rating = 0.0
    allDecisions.foreach(d => rating += d.rating) 
    rating / allDecisions.size
  }

  abstract class FDSDecision extends Decision {
    
    def rating: Double
    def rating_=(rate: Double): Unit
    def isAssigned: Boolean
    def applyDecision(): Unit
    
    override def apply(): Unit = {
      val space = spaceSize
      val inconsistent = isInconsistent(applyDecision())
      val localRating = if (inconsistent) 0.0 else 1 + spaceSize / space
      
      // Handle average ratings
      if (nRatings.length <= depth.value) {
        nRatings.push(2)
        sumRatings.push(1 + localRating)
      } else {
        nRatings(depth.value) += 1
        sumRatings(depth.value) += localRating
      }
      
      val average = sumRatings(depth.value) / nRatings(depth.value)
      
      
      rating = alpha * rating + (1 - alpha) * (localRating / averageRating)
    }
  }
  
  class FDSChoice(val variable: CPIntVar, val value: Int) {
    val decision1 = new FDSLowerEq(variable, value)
    val decision2 = decision1._opposite
    def rating: Double = decision1.rating + decision2.rating
    def isAssigned = decision1.isAssigned && decision2.isAssigned
    def firstDecision = {
      if (decision1.isAssigned) decision2
      else if (decision1.rating < decision2.rating) decision1 
      else decision2
    }
  }

  class FDSLowerEq(val variable: CPIntVar, val value: Int) extends FDSDecision {
    private[this] var rate: Double = 1.0
    val _opposite: FDSDecision = new FDSGreater(variable, value, this)
    override def rating: Double = rate
    override def rating_=(r: Double): Unit = rate = r
    override def isAssigned: Boolean = variable.max <= value || variable.min > value
    override def isTrue: Boolean = variable.max <= value
    override def opposite: Decision = _opposite
    override def toLiteral: CPBoolVar = variable.isLeEq(value)
    override def applyDecision(): Unit = variable.store.post(variable <= value)
  }

  class FDSGreater(val variable: CPIntVar, val value: Int, _opposite: FDSDecision) extends FDSDecision {
    private[this] var rate: Double = 1.0
    override def rating: Double = rate
    override def rating_=(r: Double): Unit = rate = r
    override def isAssigned: Boolean = variable.max <= value || variable.min > value
    override def isTrue: Boolean = variable.min > value
    override def opposite: Decision = _opposite
    override def toLiteral: CPBoolVar = variable.isGrEq(value + 1)
    override def applyDecision(): Unit = variable.store.post(variable > value)
  }

  object FDSChoiceOrdering extends Ordering[FDSChoice] {
    override def compare(a: FDSChoice, b: FDSChoice) = {
      val rateA = a.rating
      val rateB = b.rating
      if (rateA < rateB) -1
      else if (rateA == rateB) 0
      else 1
    }
  }
}

