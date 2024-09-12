package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.SortUtils._
import oscar.algo.reversible.ReversibleInt
import oscar.cp._
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}
import oscar.cp.scheduling.util.{ThetaLambdaTreeWithTransitionTimesAndFamilies, TransitionLowerBounds}

/**
 * Created on 21/01/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 * @author Sascha Van Cauwelaert (sascha.vancauwelaert@gmail.com)
 */
class UnaryResourceWithTransitionTimesAndFamilies(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], familyMatrix: Array[Array[Int]], family: Array[Int]) extends Constraint(starts(0).store) {
  priorityL2_=(CPStore.MaxPriorityL2 - 2)
  idempotent = true

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends

  protected[this] val nTasks = starts.length

  // Set of families on this resource
  val actualFamiliesOnResource = family.toSet.toArray
  // Number of families actually on the resource
  val nActualFamiliesOnResource = actualFamiliesOnResource.length
  // Matrix containing only the families using the resource
  protected[this] val filteredFamilyMatrix = Array.tabulate(nActualFamiliesOnResource, nActualFamiliesOnResource)((i, j) => familyMatrix(actualFamiliesOnResource(i))(actualFamiliesOnResource(j)))
  protected[this] val nFamilies = familyMatrix.length
  protected[this] val familyMatrixTransposed = Array.tabulate(nFamilies)(i => Array.tabulate(nFamilies)(j => familyMatrix(j)(i)))
  protected[this] val minTransitionTimeFrom = Array.tabulate(nFamilies)(i => if (familyMatrix(i).length > 1) familyMatrix(i).sorted.apply(1) else 0)
  protected[this] val minTransitionTimeTo = Array.tabulate(nFamilies)(i => if (familyMatrixTransposed(i).length > 1) familyMatrixTransposed(i).sorted.apply(1) else 0)
  protected[this] var minTT = 0

  protected[this] val transitionLowerBounds = new TransitionLowerBounds(filteredFamilyMatrix).getLowerBounds()
  protected[this] val thetaLambdaTree: ThetaLambdaTreeWithTransitionTimesAndFamilies = new ThetaLambdaTreeWithTransitionTimesAndFamilies(nTasks, transitionLowerBounds, family)

  protected[this] val indexByIncreasingMinStarts: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val indexByIncreasingMaxStarts: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val indexByIncreasingMinEnds: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val indexByIncreasingMaxEnds: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val indexByIncreasingMinStartMirror: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val indexByIncreasingMaxStartMirror: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val indexByIncreasingMinEndMirror: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val indexByIncreasingMaxEndMirror: Array[Int] = Array.tabulate(nTasks)(i => i)

  protected[this] val startsMirror: Array[_ <: CPIntVar] = ends map {-_}
  protected[this] val endsMirror: Array[_ <: CPIntVar] = starts map {-_}

  protected[this] val currentMinStarts: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMaxEnds: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMinEnds: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMaxStarts: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMinStartsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMaxEndsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMinEndsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMaxStartsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMinDurations: Array[Int] = Array.ofDim[Int](nTasks)

  protected[this] val newMinStarts: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val newMaxEnds: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val newMinStartsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val newMaxEndsMirror: Array[Int] = Array.ofDim[Int](nTasks)

  protected[this] val formerMinEnds : Array[ReversibleInt] = Array.fill(nTasks)(new ReversibleInt(starts(0).store,Int.MaxValue))
  protected[this] val formerMaxStarts : Array[ReversibleInt] = Array.fill(nTasks)(new ReversibleInt(starts(0).store,Int.MinValue))

  //Structures needed to represent a stack of the indexes of the activities whose bounds have changed
  protected[this] val stackOfBoundChangeIds = Array.fill(nTasks)(0)
  protected[this] var stackOfBoundChangeIdsSize = 0
  protected[this] val stackOfBoundChangeIdsContains = Array.fill(nTasks)(false)

  protected[this] var failure = false
  protected[this] var changed = true

  override def setup(l: CPPropagStrength): Unit = {
    for (i <- 0 until nTasks) {
      starts(i).callPropagateWhenBoundsChange(this)
      ends(i).callPropagateWhenBoundsChange(this)
    }

    propagate()
  }

  override def propagate(): Unit = {
    //    println("#" * 80)
    failure = false
    changed = true

    clearIdStack()

    var i = 0
    while(i < nTasks) {
      currentMinDurations(i) = durations(i).min
      currentMinStarts(i) = starts(i).min
      currentMaxEnds(i) = ends(i).max
      currentMinEnds(i) = ends(i).min
      currentMaxStarts(i) = starts(i).max
      currentMinStartsMirror(i) = -currentMaxEnds(i)
      currentMaxEndsMirror(i) = -currentMinStarts(i)
      currentMinEndsMirror(i) = -currentMaxStarts(i)
      currentMaxStartsMirror(i) = -currentMinEnds(i)
      newMinStarts(i) = currentMinStarts(i)
      newMaxEnds(i) = currentMaxEnds(i)
      newMinStartsMirror(i) = currentMinStartsMirror(i)
      newMaxEndsMirror(i) = currentMaxEndsMirror(i)

      if (formerMinEnds(i).value != currentMinEnds(i) || formerMaxStarts(i).value != currentMaxStarts(i)) {
        addIdToStack(i)
      }

      i += 1
    }

    while(!failure && changed) {
      changed = internalFixedPoint()
    }

    if(failure)
      throw Inconsistency
    else {
      i = 0
      while(i < nTasks) {
        formerMinEnds(i).setValue(currentMinEnds(i))
        formerMaxStarts(i).setValue(currentMaxStarts(i))
        i += 1
      }
    }

  }

  def internalFixedPoint(): Boolean = {
    binaryPropagate() || EF() || DP() || NFNL()
  }

  @inline
  def OC(): Boolean = overloadChecking(currentMinStarts, currentMaxEnds, thetaLambdaTree, indexByIncreasingMinStarts, indexByIncreasingMaxEnds) || overloadChecking(currentMinStartsMirror, currentMaxEndsMirror, thetaLambdaTree, indexByIncreasingMinStartMirror, indexByIncreasingMaxEndMirror)

  /**
    * returns true if some domain is changed (here failure), false otherwise
    */
  @inline
  protected def overloadChecking(startMins : Array[Int], endMaxs : Array[Int], tree : ThetaLambdaTreeWithTransitionTimesAndFamilies, orderedMinStartIds : Array[Int], orderedMaxEndIds : Array[Int]): Boolean = {
    // Clearing the tree
    mergeSort(orderedMinStartIds, startMins)
    tree.clearAndPlaceLeaves(orderedMinStartIds, startMins, currentMinDurations)

    //sorting activities in non-decreasing lct
    mergeSort(orderedMaxEndIds, endMaxs)

    var i = 0
    while (i < nTasks) {
      tree.insert(orderedMaxEndIds(i))
      val (ect, _) = tree.ect
      if(ect > endMaxs(orderedMaxEndIds(i))) {
        failure = true
        return true
      }
      i += 1
    }
    false
  }

  @inline
  def DP(): Boolean = {
    detectablePrecedences(currentMinStarts, currentMaxStarts, currentMinEnds,
      currentMaxStartsMirror, currentMaxEndsMirror, newMinStarts, newMaxEndsMirror,
      starts, ends, thetaLambdaTree,
      indexByIncreasingMinStarts, indexByIncreasingMaxStarts, indexByIncreasingMinEnds, minTransitionTimeTo) ||
      detectablePrecedences(currentMinStartsMirror, currentMaxStartsMirror, currentMinEndsMirror,
        currentMaxStarts, currentMaxEnds, newMinStartsMirror, newMaxEnds,
        startsMirror, endsMirror, thetaLambdaTree,
        indexByIncreasingMinStartMirror, indexByIncreasingMaxStartMirror, indexByIncreasingMinEndMirror, minTransitionTimeFrom)
  }

  /**
    * returns true if some domain is changed (here failure), false otherwise
    */
  @inline
  protected def detectablePrecedences(startMins : Array[Int], startMaxs: Array[Int], endMins : Array[Int],
                                      startMaxsMirror : Array[Int], endMaxsMirror : Array[Int], updatedMinStarts : Array[Int], updatedMaxEndsMirror : Array[Int],
                                      startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar], tree : ThetaLambdaTreeWithTransitionTimesAndFamilies,
                                      orderedMinStartIds : Array[Int], orderedMaxStartIds: Array[Int], orderedMinEndIds : Array[Int], minTransitionToFamily: Array[Int]): Boolean = {
    // Clearing the tree
    mergeSort(orderedMinStartIds, startMins)
    tree.clearAndPlaceLeaves(orderedMinStartIds, startMins, currentMinDurations)

    //sorting activities in non-decreasing lst
    mergeSort(orderedMaxStartIds, startMaxs)

    //sorting activities in non-decreasing ect
    mergeSort(orderedMinEndIds, endMins)

    var i, q = 0
    while (i < nTasks) {
      val ectIndex = orderedMinEndIds(i)
      while (q < nTasks && endMins(ectIndex) > startMaxs(orderedMaxStartIds(q))) {
        tree.insert(orderedMaxStartIds(q))
        q += 1
      }
      val (ectWithout, familiesInTheta) = tree.ectWithoutActivity(ectIndex)
      if (familiesInTheta != 0 && ((1 << family(ectIndex)) & familiesInTheta) == 0) {
        minTT = minTransitionToFamily(family(ectIndex))
      }
      else {
        minTT = 0
      }
      updatedMinStarts(ectIndex) = math.max(updatedMinStarts(ectIndex), ectWithout + minTT)
      i += 1
    }

    updateMins(startMins,endMins,startMaxsMirror,endMaxsMirror, updatedMinStarts, updatedMaxEndsMirror, startVars, endVars)
  }

  @inline
  def NFNL(): Boolean = {
    notLast(currentMinStarts, currentMaxStarts, currentMaxEnds,
      currentMinStartsMirror, currentMinEndsMirror, newMaxEnds, newMinStartsMirror,
      starts, ends, thetaLambdaTree,
      indexByIncreasingMinStarts, indexByIncreasingMaxStarts, indexByIncreasingMaxEnds, minTransitionTimeFrom) ||
      notLast(currentMinStartsMirror, currentMaxStartsMirror, currentMaxEndsMirror,
        currentMinStarts, currentMinEnds, newMaxEndsMirror, newMinStarts,
        startsMirror, endsMirror, thetaLambdaTree,
        indexByIncreasingMinStartMirror, indexByIncreasingMaxStartMirror, indexByIncreasingMaxEndMirror, minTransitionTimeTo)
  }

  /**
    * returns true if some domain is changed (here failure), false otherwise
    */
  @inline
  protected def notLast(startMins : Array[Int], startMaxs : Array[Int], endMaxs : Array[Int],
                        startMinsMirror : Array[Int], endMinsMirror : Array[Int], updatedMaxEnds : Array[Int], updatedMinStartsMirror : Array[Int],
                        startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar], tree : ThetaLambdaTreeWithTransitionTimesAndFamilies,
                        orderedMinStartIds : Array[Int], orderedMaxStartIds: Array[Int], orderedMaxEndIds : Array[Int], minTransitionFromFamily : Array[Int]) : Boolean = {
    // Clearing the tree
    mergeSort(orderedMinStartIds, startMins)
    tree.clearAndPlaceLeaves(orderedMinStartIds, startMins, currentMinDurations)

    //sorting activities in non-decreasing lst
    mergeSort(orderedMaxStartIds, startMaxs)

    //sorting activities in non-decreasing lct
    mergeSort(orderedMaxEndIds, endMaxs)

    var i, j = 0
    while(i < nTasks) {
      val lctIndex = orderedMaxEndIds(i)
      while(j < nTasks && endMaxs(lctIndex) > startMaxs(orderedMaxStartIds(j))) {
        val (ect, familiesInTheta) = tree.ect
        if(ect > startMaxs(orderedMaxStartIds(j))) {
          if (familiesInTheta != 0 && ((1 << family(orderedMaxStartIds(j))) & familiesInTheta) == 0) {
            minTT = minTransitionFromFamily(family(orderedMaxStartIds(j)))
          }
          else {
            minTT = 0
          }
          updatedMaxEnds(orderedMaxStartIds(j)) = startMaxs(orderedMaxStartIds(j - 1)) - minTT
        }
        tree.insert(orderedMaxStartIds(j))
        j += 1
      }

      val (ectWithout, familiesInTheta) = tree.ectWithoutActivity(lctIndex)
      if(ectWithout > startMaxs(lctIndex)) {
        val curFamily = family(lctIndex)
        if (familiesInTheta != 0 && ((1 << family(lctIndex)) & familiesInTheta) == 0) {
          minTT = minTransitionFromFamily(curFamily)
        }
        else {
          minTT = 0
        }
        if (orderedMaxStartIds(j - 1) != lctIndex) {
          updatedMaxEnds(lctIndex) = math.min(startMaxs(orderedMaxStartIds(j - 1)) - minTT, updatedMaxEnds(lctIndex))
        }
        else if (j > 1) {
          updatedMaxEnds(lctIndex) = math.min(startMaxs(orderedMaxStartIds(j - 2)) - minTT, updatedMaxEnds(lctIndex))
        }
      }

      i += 1
    }

    updateMaxs(startMaxs, endMaxs, startMinsMirror, endMinsMirror, updatedMaxEnds, updatedMinStartsMirror, startVars, endVars)

  }

  @inline
  def EF() : Boolean = {
    edgeFinding(currentMinStarts, currentMaxEnds, currentMinEnds,
      currentMaxStartsMirror,currentMaxEndsMirror, newMinStarts, newMaxEndsMirror,
      starts, ends, thetaLambdaTree,
      indexByIncreasingMinStarts, indexByIncreasingMaxEnds, minTransitionTimeTo) ||
      edgeFinding(currentMinStartsMirror, currentMaxEndsMirror, currentMinEndsMirror,
        currentMaxStarts, currentMaxEnds, newMinStartsMirror, newMaxEnds,
        startsMirror, endsMirror, thetaLambdaTree,
        indexByIncreasingMinStartMirror, indexByIncreasingMaxEndMirror, minTransitionTimeFrom)
  }

  /**
    * returns true if some domain is changed (here failure), false otherwise
    */
  @inline
  protected def edgeFinding(startMins : Array[Int], endMaxs : Array[Int], endMins : Array[Int],
                            startMaxsMirror : Array[Int], endMaxsMirror : Array[Int], updatedMinStarts : Array[Int], updatedMaxEndsMirror : Array[Int],
                            startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar], tree : ThetaLambdaTreeWithTransitionTimesAndFamilies,
                            orderedMinStartIds : Array[Int], orderedMaxEndIds : Array[Int], minTransitionToFamily: Array[Int]) : Boolean = {
    // Inserting all activities in the tree
    mergeSort(orderedMinStartIds, startMins)
    tree.fillAndPlaceLeaves(orderedMinStartIds, startMins, currentMinDurations) // true as we use gray nodes

    //sorting activities in non-decreasing lct
    //NOTE: we sort the array by increasing lct, we just will browse it from left to right
    mergeSort(orderedMaxEndIds, endMaxs)

    var estIndex = 0
    var j = nTasks - 1
    while (j > 0) {
      val (ect, _) = tree.ect
      if(ect > endMaxs(orderedMaxEndIds(j))) {
        failure = true
        return true
      }

      tree.grayActivity(orderedMaxEndIds(j))

      j -= 1
      while(tree.ectBar > endMaxs(orderedMaxEndIds(j))) {
        val responsibleEctBar = tree.responsibleEctBar
        if (tree.ectBar == tree.ect._1 || responsibleEctBar < 0) {
          failure = true
          return true
        }
        estIndex = orderedMinStartIds(responsibleEctBar)

        val (ect, familiesInTheta) = tree.ect
        if (familiesInTheta != 0 && ((1 << family(estIndex)) & familiesInTheta) == 0) {
          minTT = minTransitionToFamily(family(estIndex))
        }
        else {
          minTT = 0
        }

        updatedMinStarts(estIndex) = math.max(updatedMinStarts(estIndex), ect + minTT)
        tree.remove(estIndex)
      }
    }
    updateMins(startMins, endMins, startMaxsMirror, endMaxsMirror, updatedMinStarts, updatedMaxEndsMirror, startVars, endVars)
  }

  /**
    * returns true if some domain is changed, false otherwise
    */
  @inline
  def binaryPropagate(): Boolean = {
    //println("BP")
    var i, j = 0
    var domainModified = false
    while (!failure && stackOfBoundChangeIdsSize > 0) {
      while (stackOfBoundChangeIdsSize > 0) {
        i = popLastIdFromStack()
        j = 0
        while (j < nTasks) {
          if (j != i) {
            // activity i is after activity j
            if (currentMinEnds(i) + familyMatrix(family(i))(family(j)) > currentMaxStarts(j)) {
              //activity i is not after j nor before => failure
              if (currentMinEnds(j) + familyMatrix(family(j))(family(i)) > currentMaxStarts(i)) {
                failure = true
                return true
              }
              else {
                newMinStarts(i) = math.max(currentMinEnds(j) + familyMatrix(family(j))(family(i)), newMinStarts(i))
                newMaxEnds(j) = math.min(currentMaxStarts(i) - familyMatrix(family(j))(family(i)), newMaxEnds(j))
              }
            }
            // activity j is after activity i
            else {
              if (currentMinEnds(j) + familyMatrix(family(j))(family(i)) > currentMaxStarts(i)) {
                newMinStarts(j) = math.max(currentMinEnds(i) + familyMatrix(family(i))(family(j)), newMinStarts(j))
                newMaxEnds(i) = math.min(currentMaxStarts(j) - familyMatrix(family(i))(family(j)), newMaxEnds(i))
              }
            }
          }
          j += 1
        }
      }
      if (updateBounds())
        domainModified = true
    }
    domainModified
  }


  /**
    * returns true if some domain is changed, false otherwise
    */
  @inline
  protected def updateBounds(): Boolean = {
    var domainModified = false
    var i = 0
    while (i < nTasks) {
      if (newMinStarts(i) > currentMinStarts(i)) {
        if (isInconsistent({starts(i).updateMin(newMinStarts(i)); ends(i).updateMin(newMinStarts(i) + currentMinDurations(i))})) {
          failure = true
          return true
        }
        else {
          domainModified = true
          addIdToStack(i)
          currentMinStarts(i) = newMinStarts(i)
          currentMinEnds(i) = currentMinStarts(i) + currentMinDurations(i)
          currentMaxEndsMirror(i) = -currentMinStarts(i)
          currentMaxStartsMirror(i) = -currentMinEnds(i)
          newMaxEndsMirror(i) = - newMinStarts(i)
        }
      }
      if (newMaxEnds(i) < currentMaxEnds(i)) {
        if (isInconsistent({ends(i).updateMax(newMaxEnds(i));starts(i).updateMax(newMaxEnds(i) - currentMinDurations(i))})) {
          failure = true
          return true
        }
        else {
          domainModified = true
          addIdToStack(i)
          currentMaxEnds(i) = newMaxEnds(i)
          currentMaxStarts(i) = currentMaxEnds(i) - currentMinDurations(i)
          currentMinStartsMirror(i) = -currentMaxEnds(i)
          currentMinEndsMirror(i) = -currentMaxStarts(i)
          newMinStartsMirror(i) = - newMaxEnds(i)
        }
      }
      i += 1
    }
    domainModified
  }

  /**
    * returns true if some domain is changed, false otherwise
    */
  @inline
  protected def updateMins(startMins : Array[Int], endMins : Array[Int], startMaxsMirror : Array[Int], endMaxsMirror : Array[Int], updatedMinStarts : Array[Int], updatedMaxEndsMirror : Array[Int], startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar]): Boolean = {
    var domainModified = false
    var i = 0
    while (i < nTasks) {
      if (updatedMinStarts(i) > startMins(i)) {
        if (isInconsistent({startVars(i).updateMin(updatedMinStarts(i)); endVars(i).updateMin(updatedMinStarts(i) + currentMinDurations(i))})) {
          failure = true
          return true
        }
        else {
          domainModified = true
          addIdToStack(i)
          startMins(i) = updatedMinStarts(i)
          endMins(i) = startMins(i) + currentMinDurations(i)
          endMaxsMirror(i) = -startMins(i)
          startMaxsMirror(i) = -endMins(i)
          updatedMaxEndsMirror(i) = -updatedMinStarts(i)
        }
      }

      i += 1
    }
    domainModified
  }

  /**
    * returns true if some domain is changed, false otherwise
    */
  @inline
  protected def updateMaxs(startMaxs : Array[Int], endMaxs : Array[Int], startMinsMirror : Array[Int], endMinsMirror : Array[Int], updatedMaxEnds : Array[Int], updatedMinStartsMirror : Array[Int], startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar]): Boolean = {
    var domainModified = false
    var i = 0
    while (i < nTasks) {
      if (updatedMaxEnds(i) < endMaxs(i)) {
        if (isInconsistent({endVars(i).updateMax(updatedMaxEnds(i)); startVars(i).updateMax(updatedMaxEnds(i) - currentMinDurations(i))})) {
          failure = true
          return true
        }
        else {
          domainModified = true
          addIdToStack(i)
          endMaxs(i) = updatedMaxEnds(i)
          startMaxs(i) = endMaxs(i) - currentMinDurations(i)
          startMinsMirror(i) = -endMaxs(i)
          endMinsMirror(i) = -startMaxs(i)
          updatedMinStartsMirror(i) = -updatedMaxEnds(i)
        }
      }
      i += 1
    }
    domainModified
  }

  @inline
  private def addIdToStack(index: Int): Unit = {
    if (!stackOfBoundChangeIdsContains(index)) {
      stackOfBoundChangeIdsContains(index) = true
      stackOfBoundChangeIds(stackOfBoundChangeIdsSize) = index
      stackOfBoundChangeIdsSize += 1
    }
  }

  @inline
  private def popLastIdFromStack(): Int = {
    stackOfBoundChangeIdsSize -= 1
    val index = stackOfBoundChangeIds(stackOfBoundChangeIdsSize)
    stackOfBoundChangeIdsContains(index) = false
    index
  }

  @inline
  private def clearIdStack(): Unit = {
    stackOfBoundChangeIdsSize = 0
    var iter = nTasks - 1
    while (iter >= 0) {
      stackOfBoundChangeIdsContains(iter) = false
      iter -= 1
    }
  }
}

