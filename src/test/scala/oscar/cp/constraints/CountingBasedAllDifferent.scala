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
  *******************************************************************************/

package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.SortUtils._
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}

/**
 *
 * Counting-based all-different propagation,
 * from "A Parallel, Backjumping Subgraph Isomorphism Algorithm using Supplemental Graphs" (Prosser et alCP2015)
 * @author Sascha vancauwelaert
 */
class CountingBasedAllDifferent(constrainedVariables: Array[CPIntVar]) extends Constraint(constrainedVariables(0).store){

  //TODO : incremental mapping of bits using a sparse set (no need of the second table maintaining the positions of the values)

  override def associatedVars(): Iterable[CPVar] = constrainedVariables

  //general variables
  private[this] val nVariables = constrainedVariables.length
  private[this] val minValueOfAllDomains = constrainedVariables.map(_.min).min
  private[this] val maxValueOfAllDomains = constrainedVariables.map(_.max).max
  private[this] val maximumDomainCardinality = maxValueOfAllDomains - minValueOfAllDomains + 1

  private[this] val nBuckets : Int = maximumDomainCardinality/64 + 1
  private[this] var varIdx : Int = -1
  private[this] var bucketIdx : Int = -1

  // --- set variables ---
  // H in the paper ---
  private[this] val detectedHallSetUnion : Array[Long] = Array.fill(nBuckets)(0L)
  // A in the paper
  private[this] val currUnionOfDomains : Array[Long] = Array.fill(nBuckets)(0L)
  private[this] var nDomainsContributingToUnion : Int = 0 //n in the paper
  private[this] var currUnionDomainCardinality : Int = -1

  // --- internal domain variable (i.e. domain as bit sets) ---
  private[this] val bitSetDomains = Array.fill(nVariables,nBuckets)(0L) //domains with a bitset representation
  private[this] val bitSetDomainsBeforePropagation = Array.fill(nVariables,nBuckets)(0L) //domains before propagation, used to prevent removal of values already removed
  private[this] val temporalDomainValues = Array.ofDim[Int](maximumDomainCardinality) //temporal array used to fill the internal bit sets as well as to get the values to remove from the real domains
  private[this] var bitIdx = -1
  private[this] var domain : Array[Long] = null
  private[this] var domainEmpty : Boolean = false

  // --- variables for ordering ---
  private[this] val orderedVariablesIds : Array[Int] = Array.tabulate(nVariables)(i => i)
  private[this] val domainSizes = Array.tabulate(nVariables)(constrainedVariables(_).size)
  private[this] var currOrderedVariableId : Int = -1

  /* used to prevent using already processed variables
     keep track of number of variables to be skipped, 
     because their value is already removed from the other domains */
  private[this] val nBoundAndProcessedVariables = new ReversibleInt(constrainedVariables(0).store, 0) 
  
  final override def setup(l: CPPropagStrength): Unit = {
    priorityL2 = CPStore.MaxPriorityL2 - 2

    propagate()
    var i = nVariables
    while (i > 0) {
      i -= 1
      constrainedVariables(i).callPropagateWhenDomainChanges(this)
    }
  }

  final override def propagate(): Unit = {

    val nBoundAndProcessedVars = nBoundAndProcessedVariables.value
    var newnBoundAndProcessedVariables = 0

    //initialization
    nDomainsContributingToUnion = 0
    clearUnionSets()

    //sort by cardinality and set bit set domains as variable domains
    varIdx = nVariables
    while (varIdx > 0) {
      varIdx -= 1
      val domainSize = constrainedVariables(varIdx).size
      domainSizes(varIdx) = domainSize

      if (domainSize == 1)
        newnBoundAndProcessedVariables += 1

      //TODO : do not clearAndFill if domain size did not change compared to last call
      domain = bitSetDomains(varIdx)
      clearAndFillBitSetDomain()
    }

    mergeSort(orderedVariablesIds, domainSizes,  nBoundAndProcessedVars, nVariables)

    //loop on variables by non-decreasing domain cardinality
    varIdx = nBoundAndProcessedVars
    while (varIdx < nVariables) {
      currOrderedVariableId = orderedVariablesIds(varIdx)
      domain = bitSetDomains(currOrderedVariableId)
      nDomainsContributingToUnion += 1
      removeHallSetFromdomainAndUpdateDomainUnion()

      if (domainEmpty || currUnionDomainCardinality < nDomainsContributingToUnion)
        throw Inconsistency

      if (currUnionDomainCardinality == nDomainsContributingToUnion) {
        updateHallSetAndClearDomainUnion()
        nDomainsContributingToUnion = 0
      }

      varIdx += 1
    }

    updateDomainsWithBitSetDomains()

    //update number of bound and processed variables
    nBoundAndProcessedVariables.setValue(newnBoundAndProcessedVariables)
  }

  /* Bits operations */
  @inline private def bucketIndexForValue(value : Int) = (value - minValueOfAllDomains) >>> 6
  
  @inline private def bitNumberInBucketForValue(value : Int) = (value - minValueOfAllDomains) & 63

  @inline private def clearAndFillBitSetDomain() : Unit = {
    bucketIdx = nBuckets
    //clear domains
    while (bucketIdx > 0) {
      bucketIdx -= 1
      domain(bucketIdx) = 0L
    }
    //fill domains with curr values
    bitIdx = constrainedVariables(varIdx).fillArray(temporalDomainValues)

    while (bitIdx > 0) {
      bitIdx -= 1
      addToBitSetDomain(temporalDomainValues(bitIdx))
    }

    //save the domains before propagation to prevent removal of values already removed
    bucketIdx = nBuckets
    //clear domains
    while (bucketIdx > 0) {
      bucketIdx -= 1
      bitSetDomainsBeforePropagation(varIdx)(bucketIdx) = domain(bucketIdx)
    }
  }
  
  @inline private def clearUnionSets() : Unit = {
    bucketIdx = nBuckets
    //clear H and A sets
    while (bucketIdx > 0) {
      bucketIdx -= 1
      detectedHallSetUnion(bucketIdx) = 0L
      currUnionOfDomains(bucketIdx) = 0L
    }
  }
  
  @inline private def addToBitSetDomain(value : Int) : Unit = {
    domain(bucketIndexForValue(value)) |= (1L << bitNumberInBucketForValue(value))
  }

  @inline private def removeHallSetFromdomainAndUpdateDomainUnion() : Unit = {
    
    domainEmpty = true
    currUnionDomainCardinality = 0
    bucketIdx = nBuckets
    
    while (bucketIdx > 0) {
      bucketIdx -= 1
      
      //update domain
      domain(bucketIdx) &= ~detectedHallSetUnion(bucketIdx) 
      if (domain(bucketIdx) != 0)
        domainEmpty = false
      
      //update domain union
      currUnionOfDomains(bucketIdx) |= domain(bucketIdx) 
      currUnionDomainCardinality += java.lang.Long.bitCount(currUnionOfDomains(bucketIdx))
    }
  }

  @inline private def updateHallSetAndClearDomainUnion() : Unit = {
    bucketIdx = nBuckets
    
    while (bucketIdx > 0) {
      
      bucketIdx -= 1
      //update hall set with curr domain union
      detectedHallSetUnion(bucketIdx) |= currUnionOfDomains(bucketIdx)
      //clear domain union
      currUnionOfDomains(bucketIdx) = 0L 
      
    }
  }

  @inline private def updateDomainsWithBitSetDomains() : Unit = {

    val nBoundAndProcessedVars = nBoundAndProcessedVariables.value

    varIdx = nVariables
    
    while (varIdx > nBoundAndProcessedVars) {
      
      varIdx -= 1
      currOrderedVariableId = orderedVariablesIds(varIdx)
      domain = bitSetDomains(currOrderedVariableId)

      //get the values by bucket and remove them from the real domain of the curr variable
      bucketIdx = nBuckets
      var nValuesToRemove = 0
      
      while(bucketIdx > 0) {
        
        bucketIdx -= 1

        var valuesToBeRemovedBitSet = domain(bucketIdx) ^ bitSetDomainsBeforePropagation(currOrderedVariableId)(bucketIdx)

        //TODO : for now, linear to get the values, could be done more efficiently with a lookup table
        var bitIndex = 0
        var unaryBitSet = 0L // number of the form 0* 1 0*

        while(valuesToBeRemovedBitSet != 0) {
          unaryBitSet = 1L << bitIndex
          if((valuesToBeRemovedBitSet & unaryBitSet) != 0) {
            temporalDomainValues(nValuesToRemove) = minValueOfAllDomains + 64 * bucketIdx + bitIndex
            nValuesToRemove += 1
            valuesToBeRemovedBitSet ^= unaryBitSet
          }
          bitIndex += 1
        }
      }

      if(nValuesToRemove > 0) {
        //do not have to check for failure as domain wipe out would have been detected before
        constrainedVariables(currOrderedVariableId).removeValues(temporalDomainValues, nValuesToRemove)
      }

    }
  }
}
