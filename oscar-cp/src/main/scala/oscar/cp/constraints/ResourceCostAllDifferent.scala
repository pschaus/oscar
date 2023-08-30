package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleBoolean, ReversibleContext, ReversibleInt, ReversibleSparseSet}
import oscar.cp._
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.{CPPropagStrength, Constraint}

/**
  * Created by saschavancauwelaert on 26/10/16.
  */
class ResourceCostAllDifferent(vars: Array[CPIntVar], val consumptions: Array[Int], val assignmentPrices: Array[Int], maxProduct: CPIntVar, onlyChecker: Boolean) extends Constraint(vars.head.store, "ResourceCostAllDifferent") {

  override def associatedVars() = vars ++ Array(maxProduct)

  //for now, the code does not work with negative consumptions/prices
  if(consumptions.exists(_ < 0) || assignmentPrices.exists(_ < 0) ) {
    throw new IllegalArgumentException("Consumptions and prices must be positive")
  }

  /*** Size variables ***/
  private[this] val nVars = vars.length
  private[this] val nConsumptions = consumptions.length
  private[this] val nAssignments = assignmentPrices.length

  /*** Precomputation ***/
  //consumptions
  private[this] val orderedVarIndices = vars.zipWithIndex.zip(consumptions).sortBy(-_._2).map(_._1._2) //sort by decreasing consumption

  //costs
  val costsWithAssignment = assignmentPrices zipWithIndex
  val orderedCostsWithAssignment = costsWithAssignment.sortBy(_._1) //sort by increasing costs
  val (orderedCosts, orderedAssignments) : (Array[Int], Array[Int]) = orderedCostsWithAssignment.unzip

  /*** Reversibles ***/
  private[this] implicit val context = s
  private[this] val assignedCost = ReversibleInt(0) // used to maintain the exact total cost due to assigned variables
  private[this] val unboundVarIndices = new ReversibleDoublyLinkedList(orderedVarIndices)
  private[this] val unmappedAssignmentsIndices = new ReversibleDoublyLinkedList(orderedAssignments)
  private[this] val variablesIndicesWithValue = Array.fill(nAssignments)(new ReversibleSparseSet(context,0, nVars - 1))

  init()

  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](nVars)

  /*** temporary variables ***/
  private[this] val maxDomain = vars.map(_.size).max
  private[this] val domainArray = new Array[Int](maxDomain)
  private[this] var domainArraySize = -1

  private[this] val cumulativeOptimumMapping = Array.fill(nVars)(-1)
  private[this] val leftShiftedCumulativeOptimumMapping = Array.fill(nVars - 1)(-1)
  private[this] val rightShiftedCumulativeOptimumMapping = Array.fill(nVars - 1)(-1)
  private[this] val positionOfAssignment = Array.fill(nAssignments)(-1)

  private[this] val nValuesToRemoveForVariable = Array.fill(nVars)(0)
  private[this] val valuesToRemoveForVariable = Array.fill(nVars, nAssignments)(-1)

  private[this] var nUnboundVariables = -1


  override def setup(l: CPPropagStrength): Unit /*CPOutcome*/ = {
    for (v <- 0 until nVars) {
      deltas(v) = vars(v).callPropagateOnChangesWithDelta(this)
    }

    propagate()
    maxProduct.callPropagateWhenBoundsChange(this)
    /*if (propagate() == Failure)
      Failure
    else {
      maxProduct.callPropagateWhenBoundsChange(this)
      Suspend
    }*/
  }

  override def propagate(): Unit /*CPOutcome*/ = {

//    println("we propagate")

    if (s.isFailed)
      throw Inconsistency
//      return Failure

    updateAssignedCost()
    /*if(updateAssignedCost() == Failure)
      return Failure*/

    updateUnmappedAssignments()

    if(unmappedAssignmentsIndices.size < unboundVarIndices.size) {
      throw Inconsistency
//      return Failure
    }

    val unboundCost = computeUnboundCost()
    val lowerBoundCost = unboundCost + assignedCost.getValue()

    if (lowerBoundCost > maxProduct.max) {
        throw Inconsistency
      //      return Failure
    }
    /*else if(lowerBoundCost > maxProduct.min && maxProduct.updateMin(lowerBoundCost) == Failure)
      return Failure*/
    else if(lowerBoundCost > maxProduct.min)
      maxProduct.updateMin(lowerBoundCost)

    //if there is at least still one variable that is unbound, we try to filter the domains
    if(!onlyChecker && unboundVarIndices.first.getValue() != unboundVarIndices.UNDEF) {
      filterDomains()
      /*if(filterDomains() == Failure)
        return Failure*/
    }
//    Suspend

  }

  private def init() = {
    /*** Checks input is correct ***/
    assert(nVars == nConsumptions)
    assert(nVars <= nAssignments)
    for (v <- 0 until nVars) {
      val variable = vars(v)
      assert(variable.min >= 0 && variable.max < nAssignments)
    }

    //remove from sparse sets containing variables the variables that do not contain a value.
    for (a <- 0 until nAssignments; v <- 0 until nVars; if(!vars(v).hasValue(a))) {
      variablesIndicesWithValue(a).removeValue(v)
    }

    //If the a sparse set gets empty, remove the corresponding value from the ordered assignments
    for (a <- 0 until nAssignments; if(variablesIndicesWithValue(a).isEmpty)) {
      unmappedAssignmentsIndices.remove(a)
    }

  }

  private def updateAssignedCost() : Unit /*CPOutcome*/ = {
    var currentIndex = unboundVarIndices.first.getValue()
    var additionalAmountOfNewBoundVars = 0
    while (currentIndex != unboundVarIndices.UNDEF) {

      val varIndex = orderedVarIndices(currentIndex)

      if(vars(varIndex).isBound) {
        val assignment = vars(varIndex).value
        additionalAmountOfNewBoundVars += consumptions(varIndex) * assignmentPrices(assignment)

        //remove variable from the set of unbound variables
        unboundVarIndices.remove(varIndex)

        //remove assignment from the set of remaining possible assignment
        unmappedAssignmentsIndices.remove(assignment)

        //remove the current variable index from the sparse sets of the other values than the current assignment and update the reversible structures
        val delta = deltas(varIndex)
        if(delta.size > 0) {
          domainArraySize = delta.fillArray(domainArray)
          var i = 0
          while (i < domainArraySize) {
            val removedAssignment = domainArray(i)
            variablesIndicesWithValue(removedAssignment).removeValue(varIndex)
            if(variablesIndicesWithValue(removedAssignment).isEmpty) {
              unmappedAssignmentsIndices.remove(removedAssignment)
            }
            i += 1
          }
        }
      }

      currentIndex = unboundVarIndices.succs(currentIndex)
    }

    //update assignedCost with cost due to new bound variables
    assignedCost.setValue(assignedCost.getValue() + additionalAmountOfNewBoundVars)

    val minimumProduct = assignedCost.getValue()

    //TODO: we cannot do that if we still have unassigned negative prices/consumption !!!
    if (minimumProduct> maxProduct.max) {
      throw Inconsistency
      //      Failure
    }
    /*else if(minimumProduct > maxProduct.min && maxProduct.updateMin(minimumProduct) == Failure)
      Failure*/
    if(minimumProduct > maxProduct.min)
      maxProduct.updateMin(minimumProduct)
    /*else
      Suspend*/
  }

  private def updateUnmappedAssignments() = {
    //update the sparse sets of variables containing a given value v. To do so, loop on all deltas. If an assignment is
    //no longer in any sparse set (i.e. domain), then we remove
    var currentIndex = unboundVarIndices.first.getValue()
    while (currentIndex != unboundVarIndices.UNDEF) {
      val varIndex = orderedVarIndices(currentIndex)
      val delta = deltas(varIndex)
      if(delta.size > 0) {
        domainArraySize = delta.fillArray(domainArray)
        var i = 0
        while (i < domainArraySize) {
          val removedAssignment = domainArray(i)
          variablesIndicesWithValue(removedAssignment).removeValue(varIndex)
          if(variablesIndicesWithValue(removedAssignment).isEmpty) {
            unmappedAssignmentsIndices.remove(removedAssignment)
          }
          i += 1
        }
      }
      currentIndex = unboundVarIndices.succs(currentIndex)
    }
  }

  private def computeUnboundCost() : Int = {
    nUnboundVariables = fillCumulativeArray(cumulativeOptimumMapping, unboundVarIndices.first.getValue(), unmappedAssignmentsIndices.first.getValue())
    if(nUnboundVariables > 0) {
      cumulativeOptimumMapping(nUnboundVariables - 1)
    }
    else
      0

  }

  private def filterDomains() : Unit/*CPOutcome*/ = {
    //TODO: we could fill the cumulative arrays in one pass
    fillCumulativeArray(leftShiftedCumulativeOptimumMapping, unboundVarIndices.succs(unboundVarIndices.first.getValue()), unmappedAssignmentsIndices.first.getValue())
    fillCumulativeArray(rightShiftedCumulativeOptimumMapping, unboundVarIndices.first.getValue(), unmappedAssignmentsIndices.succs(unmappedAssignmentsIndices.first.getValue()))

    computePositionsOfUnmappedAssignments()

    var currentVarPosition = 0
    var currentIndexInOrderedVarIndices = unboundVarIndices.first.getValue()

    //collect the values to be removed from the variable
    while(currentVarPosition < nUnboundVariables) {
      val currentVarIndex = orderedVarIndices(currentIndexInOrderedVarIndices)
      val currentVariable = vars(currentVarIndex)

      domainArraySize = currentVariable.fillArray(domainArray)
      var currentDomainValueIndex = 0
      var nValuesToRemove = 0
      while(currentDomainValueIndex < domainArraySize) {
        val currentDomainValue = domainArray(currentDomainValueIndex)

        val assignmentPosition = positionOfAssignment(currentDomainValue)
        if(assignmentPosition == -1 || assignmentExceedsMaximumCost(currentVarPosition, assignmentPosition, currentVarIndex, currentDomainValue, nUnboundVariables)) {
          valuesToRemoveForVariable(currentVarIndex)(nValuesToRemove) = currentDomainValue
          nValuesToRemove += 1
        }
        currentDomainValueIndex += 1
      }

      if(domainArraySize == nValuesToRemove) {
        throw Inconsistency
//        return Failure
      }


      nValuesToRemoveForVariable(currentVarIndex) = nValuesToRemove
      currentIndexInOrderedVarIndices = unboundVarIndices.succs(currentIndexInOrderedVarIndices)
      currentVarPosition += 1
    }

    //remove the values from the variables domains
    currentIndexInOrderedVarIndices = unboundVarIndices.first.getValue()
    while(currentIndexInOrderedVarIndices != unboundVarIndices.UNDEF) {
      val currentVarIndex = orderedVarIndices(currentIndexInOrderedVarIndices)
      val currentVariable = vars(currentVarIndex)

      var v = 0
      while (v < nValuesToRemoveForVariable(currentVarIndex)) {

        val valueToRemove = valuesToRemoveForVariable(currentVarIndex)(v)

        /*if(currentVariable.removeValue(valueToRemove) == Failure) {
          return Failure
        }*/
        currentVariable.removeValue(valueToRemove)
//        else {
          //if we don't fail, we update the reversible structures because we removed a value (the removed values will not be available in the deltas)
          variablesIndicesWithValue(valueToRemove).removeValue(currentVarIndex)
          if(variablesIndicesWithValue(valueToRemove).isEmpty) {
            unmappedAssignmentsIndices.remove(valueToRemove)
          }
//        }

        v += 1
      }
      currentIndexInOrderedVarIndices = unboundVarIndices.succs(currentIndexInOrderedVarIndices)
    }


//    Suspend
  }

  private def fillCumulativeArray(array: Array[Int], firstVarIndex : Int, firstValueIndex : Int) : Int = {
    var nValues = 0
    var currentIndexInOrderedVarIndices = firstVarIndex
    var currentIndexInOrderedAssignmentIndices = firstValueIndex

    if(nValues < array.length && currentIndexInOrderedVarIndices != unboundVarIndices.UNDEF && currentIndexInOrderedAssignmentIndices != unmappedAssignmentsIndices.UNDEF) {
      //first element of cumulative array
      val currentVarIndex = orderedVarIndices(currentIndexInOrderedVarIndices)
      val currentAssignment = orderedAssignments(currentIndexInOrderedAssignmentIndices)
      array(nValues) = consumptions(currentVarIndex) * assignmentPrices(currentAssignment)
      nValues += 1
      currentIndexInOrderedVarIndices = unboundVarIndices.succs(currentIndexInOrderedVarIndices)
      currentIndexInOrderedAssignmentIndices = unmappedAssignmentsIndices.succs(currentIndexInOrderedAssignmentIndices)

      //others
      while (nValues < array.length && currentIndexInOrderedVarIndices != unboundVarIndices.UNDEF && currentIndexInOrderedAssignmentIndices != unmappedAssignmentsIndices.UNDEF) {
        val currentVarIndex = orderedVarIndices(currentIndexInOrderedVarIndices)
        val currentAssignment = orderedAssignments(currentIndexInOrderedAssignmentIndices)

        array(nValues) = array(nValues - 1) + consumptions(currentVarIndex) * assignmentPrices(currentAssignment)

        nValues += 1
        currentIndexInOrderedVarIndices = unboundVarIndices.succs(currentIndexInOrderedVarIndices)
        currentIndexInOrderedAssignmentIndices = unmappedAssignmentsIndices.succs(currentIndexInOrderedAssignmentIndices)
      }
    }

    nValues
  }

  private def computePositionsOfUnmappedAssignments() /* :scala.collection.mutable.HashMap[Int,Int]*/ = {

    //TODO: check if we have to reset the positions or not
    //reset all positions
    var i = 0
    while(i < nAssignments) {
      positionOfAssignment(i) = -1
      i += 1
    }

    var currentValuePosition = 0
    var currentIndexInOrderedAssignmentIndices = unmappedAssignmentsIndices.first.getValue()
    while (currentIndexInOrderedAssignmentIndices != unmappedAssignmentsIndices.UNDEF) {
      val currentAssignment = orderedAssignments(currentIndexInOrderedAssignmentIndices)
      positionOfAssignment(currentAssignment)=currentValuePosition
      currentIndexInOrderedAssignmentIndices = unmappedAssignmentsIndices.succs(currentIndexInOrderedAssignmentIndices)
      currentValuePosition += 1
    }
  }

  private def assignmentExceedsMaximumCost(variablePosition : Int, assignmentPosition : Int, variableIndex: Int, value: Int, nUnboundVars : Int) : Boolean = {
    var leftUnchanged = 0
    var rightUnchanged = 0
    val assignmentCost = consumptions(variableIndex) * assignmentPrices(value)
    var shiftedCost = 0

    if(nUnboundVars > 1){ //leftUnchanged, rightUnchanged and shiftedCost can only be > 0 if there is more than one variable that is unbound
      if(variablePosition < assignmentPosition) {

        if(variablePosition > 0) {
          leftUnchanged = cumulativeOptimumMapping(variablePosition - 1)
        }

        if(assignmentPosition < nUnboundVars - 1) {
          rightUnchanged = cumulativeOptimumMapping(nUnboundVars - 1) - cumulativeOptimumMapping(assignmentPosition)
        }

        shiftedCost = leftShiftedCumulativeOptimumMapping(math.min(assignmentPosition - 1, nUnboundVars - 2))
        if(variablePosition > 0) {
          shiftedCost -= leftShiftedCumulativeOptimumMapping(variablePosition - 1)
        }
      }
      else if(variablePosition > assignmentPosition) {
        if(assignmentPosition > 0) {
          leftUnchanged = cumulativeOptimumMapping(assignmentPosition - 1)
        }
        if(variablePosition < nUnboundVars - 1) {
          rightUnchanged = cumulativeOptimumMapping(nUnboundVars - 1) - cumulativeOptimumMapping(variablePosition)
        }

        shiftedCost = rightShiftedCumulativeOptimumMapping(variablePosition - 1)
        if(assignmentPosition > 0) {
          shiftedCost -= rightShiftedCumulativeOptimumMapping(assignmentPosition - 1)
        }
      }
    }

    if(leftUnchanged + assignmentCost + shiftedCost + rightUnchanged + assignedCost.getValue() > maxProduct.max) {
      true
    }
    else {
      false
    }
  }
}


class ReversibleDoublyLinkedList(val elementIds : Array[Int])(implicit val context: ReversibleContext) {

  private[this] val nElements = elementIds.length

  val size = ReversibleInt(nElements)

  val first = ReversibleInt(0)
  private[this] val last = ReversibleInt(nElements - 1)

  val UNDEF = -1

  private[this] val precs : Array[ReversibleInt] = Array.tabulate(nElements)(i => if(i == 0) ReversibleInt(UNDEF)else ReversibleInt(i-1))
  val succs : Array[ReversibleInt] = Array.tabulate(nElements)(i => if(i == nElements - 1) ReversibleInt(UNDEF) else ReversibleInt(i+1))

  private[this] val indexOfId = elementIds.zipWithIndex.sortBy(_._1).map(_._2)

  private[this] val isElementIdPresent : Array[ReversibleBoolean] = Array.fill(nElements)(new ReversibleBoolean(context, true))

  def remove(elementId: Int) = {

    //TODO: isElementIdPresent is not mandatory if we do not remove elements that are not inside the list
    if(isElementIdPresent(elementId).getValue()) {
      isElementIdPresent(elementId).setFalse()
      size.decr()

      val elementIndex = indexOfId(elementId)

      if(precs(elementIndex).getValue() != UNDEF && succs(elementIndex).getValue() != UNDEF) {
        succs(precs(elementIndex).getValue()).setValue(succs(elementIndex).getValue())
        precs(succs(elementIndex).getValue()).setValue(precs(elementIndex).getValue())
      }
      else if (precs(elementIndex).getValue() == UNDEF && succs(elementIndex).getValue() == UNDEF) {
        first.setValue(UNDEF)
        last.setValue(UNDEF)
      }
      else if(precs(elementIndex).getValue() == UNDEF) {
        first.setValue(succs(elementIndex))
        precs(succs(elementIndex).getValue()).setValue(precs(elementIndex).getValue())
      }
      else if(succs(elementIndex).getValue() == UNDEF) {
        last.setValue(precs(elementIndex))
        succs(precs(elementIndex).getValue()).setValue(succs(elementIndex).getValue())
      }
    }
  }


  override def toString() : String = {
    if(first.getValue() == UNDEF && last.getValue() == UNDEF) {
      "()"
    }
    else {
      var str = "( " + elementIds(first.getValue())
      var next = succs(first.getValue())
      while(next.getValue() != UNDEF) {

        str += " -> " + elementIds(next.getValue())

        next = succs(next.getValue())
      }

      str += " )"
      str
    }
  }

}
