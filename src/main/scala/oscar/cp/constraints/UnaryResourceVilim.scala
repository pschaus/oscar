package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.SortUtils._
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.scheduling.util.ThetaLambdaTree
import oscar.cp._

/**
 * Created on 21/01/15.
 *
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 * @author Sascha Van Cauwelaert (sascha.vancauwelaert@gmail.com)
 */
class UnaryResourceVilim(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar]) extends Constraint(starts(0).store) {
  idempotent = true

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends

  private[this] val nTasks = starts.length
  private[this] val thetaLambdaTree = new ThetaLambdaTree(nTasks)
  private[this] val thetaLambdaTreeMirror = new ThetaLambdaTree(nTasks)

  private[this] val indexByIncreasingMinStarts: Array[Int] = Array.tabulate(nTasks)(i => i)
  private[this] val indexByIncreasingMaxStarts: Array[Int] = Array.tabulate(nTasks)(i => i)
  private[this] val indexByIncreasingMinEnds: Array[Int] = Array.tabulate(nTasks)(i => i)
  private[this] val indexByIncreasingMaxEnds: Array[Int] = Array.tabulate(nTasks)(i => i)
  private[this] val indexByIncreasingMinStartMirror: Array[Int] = Array.tabulate(nTasks)(i => i)
  private[this] val indexByIncreasingMaxStartMirror: Array[Int] = Array.tabulate(nTasks)(i => i)
  private[this] val indexByIncreasingMinEndMirror: Array[Int] = Array.tabulate(nTasks)(i => i)
  private[this] val indexByIncreasingMaxEndMirror: Array[Int] = Array.tabulate(nTasks)(i => i)

  private[this] val startsMirror = ends map {-_}
  private[this] val endsMirror = starts map {-_}

  private[this] val currentMinStarts: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMaxEnds: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMinEnds: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMaxStarts: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMinStartsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMaxEndsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMinEndsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMaxStartsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMinDurations: Array[Int] = Array.ofDim[Int](nTasks)

  private[this] val newMinStarts: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val newMaxEnds: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val newMinStartsMirror: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val newMaxEndsMirror: Array[Int] = Array.ofDim[Int](nTasks)

  private[this] val formerMinEnds : Array[ReversibleInt] = Array.fill(nTasks)(new ReversibleInt(starts(0).store,Int.MaxValue))
  private[this] val formerMaxStarts : Array[ReversibleInt] = Array.fill(nTasks)(new ReversibleInt(starts(0).store,Int.MinValue))

  private[this] var failure = false
  private[this] var changed = true

  // Temporary arrays needed by the mergeSort Algorithm
  private[this] val mergeSortRuns = Array.ofDim[Int](nTasks + 1)
  private[this] val mergeSortAux = Array.ofDim[Int](nTasks)


  override def setup(l: CPPropagStrength): Unit = {
    for (i <- 0 until nTasks) {
      starts(i).callPropagateWhenBoundsChange(this)
      ends(i).callPropagateWhenBoundsChange(this)
    }
    propagate()
  }

  override def propagate(): Unit = {
    failure = false
    changed = true

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
      i += 1
    }

    while(!failure && changed) {
      if (DP() || NFNL() || EF()) {
        changed = true
      }
      else
        changed = false
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

  @inline
  private def DP(): Boolean = {
    (detectablePrecedences(currentMinStarts, currentMaxStarts, currentMinEnds,
      currentMaxStartsMirror, currentMaxEndsMirror, newMinStarts, newMaxEndsMirror,
      starts, ends, thetaLambdaTree,
      indexByIncreasingMinStarts, indexByIncreasingMaxStarts, indexByIncreasingMinEnds) && !failure) |
      detectablePrecedences(currentMinStartsMirror, currentMaxStartsMirror, currentMinEndsMirror,
        currentMaxStarts, currentMaxEnds, newMinStartsMirror, newMaxEnds,
        startsMirror, endsMirror, thetaLambdaTreeMirror,
        indexByIncreasingMinStartMirror, indexByIncreasingMaxStartMirror, indexByIncreasingMinEndMirror)
  }

  /**
   * returns true if some domain is changed, false otherwise
   */
  @inline
  private def detectablePrecedences(startMins : Array[Int], startMaxs: Array[Int], endMins : Array[Int],
                                    startMaxsMirror : Array[Int], endMaxsMirror : Array[Int], updatedMinStarts : Array[Int], updatedMaxEndsMirror : Array[Int],
                                    startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar], tree : ThetaLambdaTree,
                                    orderedMinStartIds : Array[Int], orderedMaxStartIds: Array[Int], orderedMinEndIds : Array[Int]): Boolean = {

    // Clearing the tree
    mergeSort(orderedMinStartIds, startMins, 0, nTasks, mergeSortRuns, mergeSortAux)
    tree.clearAndPlaceLeaves(orderedMinStartIds, startMins, currentMinDurations)

    //sorting activities in non-decreasing lst
    mergeSort(orderedMaxStartIds, startMaxs, 0, nTasks, mergeSortRuns, mergeSortAux)

    //sorting activities in non-decreasing ect
    mergeSort(orderedMinEndIds, endMins, 0, nTasks, mergeSortRuns, mergeSortAux)

    var i, j = 0
    while (i < nTasks) {
      val ectIndex = orderedMinEndIds(i)
      while (j < nTasks && endMins(ectIndex) > startMaxs(orderedMaxStartIds(j))) {
        tree.insert(orderedMaxStartIds(j))
        j += 1
      }
      updatedMinStarts(ectIndex) = math.max(updatedMinStarts(ectIndex), tree.ectWithoutActivity(ectIndex))
      i += 1
    }

    updateMins(startMins,endMins,startMaxsMirror,endMaxsMirror, updatedMinStarts, updatedMaxEndsMirror, startVars, endVars)
  }

  @inline
  private def NFNL(): Boolean = {
    (notLast(currentMinStarts, currentMaxStarts, currentMaxEnds,
            currentMinStartsMirror, currentMinEndsMirror, newMaxEnds, newMinStartsMirror,
            starts, ends, thetaLambdaTree,
            indexByIncreasingMinStarts, indexByIncreasingMaxStarts, indexByIncreasingMaxEnds) && !failure) |
    notLast(currentMinStartsMirror, currentMaxStartsMirror, currentMaxEndsMirror,
            currentMinStarts, currentMinEnds, newMaxEndsMirror, newMinStarts,
            startsMirror, endsMirror, thetaLambdaTreeMirror,
            indexByIncreasingMinStartMirror, indexByIncreasingMaxStartMirror, indexByIncreasingMaxEndMirror)
  }

  /**
   * returns true if some domain is changed, false otherwise
   */
  @inline
  private def notLast(startMins : Array[Int], startMaxs : Array[Int], endMaxs : Array[Int],
                      startMinsMirror : Array[Int], endMinsMirror : Array[Int], updatedMaxEnds : Array[Int], updatedMinStartsMirror : Array[Int],
                      startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar], tree : ThetaLambdaTree,
                      orderedMinStartIds : Array[Int], orderedMaxStartIds: Array[Int], orderedMaxEndIds : Array[Int]) : Boolean = {

    // Clearing the tree
    mergeSort(orderedMinStartIds, startMins, 0, nTasks, mergeSortRuns, mergeSortAux)
    tree.clearAndPlaceLeaves(orderedMinStartIds, startMins, currentMinDurations)

    //sorting activities in non-decreasing lst
    mergeSort(orderedMaxStartIds, startMaxs, 0, nTasks, mergeSortRuns, mergeSortAux)

    //sorting activities in non-decreasing lct
    mergeSort(orderedMaxEndIds, endMaxs, 0, nTasks, mergeSortRuns, mergeSortAux)

    var i, j = 0
    while(i < nTasks) {
      val lctIndex = orderedMaxEndIds(i)
      while(j < nTasks && endMaxs(lctIndex) > startMaxs(orderedMaxStartIds(j))) {
        if(tree.ect > startMaxs(orderedMaxStartIds(j))) {
          updatedMaxEnds(orderedMaxStartIds(j)) = startMaxs(orderedMaxStartIds(j - 1))
        }
        tree.insert(orderedMaxStartIds(j))
        j += 1
      }

      if(tree.ectWithoutActivity(lctIndex) > startMaxs(lctIndex)) {
        updatedMaxEnds(lctIndex) = math.min(startMaxs(orderedMaxStartIds(j - 1)), updatedMaxEnds(lctIndex))
      }

      i += 1
    }

    updateMaxs(startMaxs, endMaxs, startMinsMirror, endMinsMirror, updatedMaxEnds, updatedMinStartsMirror, startVars, endVars)

  }

  @inline
  private def EF() : Boolean = {
    (edgeFinding(currentMinStarts, currentMaxEnds, currentMinEnds,
                currentMaxStartsMirror,currentMaxEndsMirror, newMinStarts, newMaxEndsMirror,
                starts, ends, thetaLambdaTree,
                indexByIncreasingMinStarts, indexByIncreasingMaxEnds) && ! failure) |
    edgeFinding(currentMinStartsMirror, currentMaxEndsMirror, currentMinEndsMirror,
                currentMaxStarts, currentMaxEnds, newMinStartsMirror, newMaxEnds,
                startsMirror,endsMirror, thetaLambdaTreeMirror,
                indexByIncreasingMinStartMirror, indexByIncreasingMaxEndMirror)
  }

  /**
   * returns true if some domain is changed, false otherwise
   */
  @inline
  private def edgeFinding(startMins : Array[Int], endMaxs : Array[Int], endMins : Array[Int],
                          startMaxsMirror : Array[Int], endMaxsMirror : Array[Int], updatedMinStarts : Array[Int], updatedMaxEndsMirror : Array[Int],
                          startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar], tree : ThetaLambdaTree,
                          orderedMinStartIds : Array[Int], orderedMaxEndIds : Array[Int]) : Boolean = {

    // Inserting all activities in the tree
    mergeSort(orderedMinStartIds, startMins, 0, nTasks, mergeSortRuns, mergeSortAux)

    tree.fillAndPlaceLeaves(orderedMinStartIds, startMins, currentMinDurations) // true as we use gray nodes

    //sorting activities in non-decreasing lct
    //NOTE: we sort the array by increasing lct, we just will browse it from right to left
    mergeSort(orderedMaxEndIds, endMaxs, 0, nTasks, mergeSortRuns, mergeSortAux)

    var estIndex = 0
    var j = nTasks - 1
    while (j > 0) {

      if(tree.ect > endMaxs(orderedMaxEndIds(j))) {
        failure = true
        return true
      }

      tree.grayActivity(orderedMaxEndIds(j))

      j -= 1
      while(tree.ectBar > endMaxs(orderedMaxEndIds(j))) {
        if (tree.responsibleEctBar < 0) {
          failure = true
          return true
        }
        estIndex = orderedMinStartIds(tree.responsibleEctBar)
        updatedMinStarts(estIndex) = math.max(updatedMinStarts(estIndex), tree.ect)
        tree.remove(estIndex)
      }
    }

    updateMins(startMins, endMins, startMaxsMirror, endMaxsMirror, updatedMinStarts, updatedMaxEndsMirror, startVars, endVars)
  }

  /**
   * returns true if some domain is changed, false otherwise
   */
  @inline
  private def updateMins(startMins : Array[Int], endMins : Array[Int], startMaxsMirror : Array[Int], endMaxsMirror : Array[Int], updatedMinStarts : Array[Int], updatedMaxEndsMirror : Array[Int], startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar]): Boolean = {
    var domainModified = false
    var i = 0
    while (i < nTasks) {
      if (updatedMinStarts(i) > startMins(i)) {
        if (isInconsistent({startVars(i).updateMin(updatedMinStarts(i));endVars(i).updateMin(updatedMinStarts(i) + currentMinDurations(i))})) {
          failure = true
          return true
        }
        else {
          domainModified = true
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
  private def updateMaxs(startMaxs : Array[Int], endMaxs : Array[Int], startMinsMirror : Array[Int], endMinsMirror : Array[Int], updatedMaxEnds : Array[Int], updatedMinStartsMirror : Array[Int], startVars : Array[_ <: CPIntVar], endVars : Array[_ <: CPIntVar]): Boolean = {
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
}

object UnaryResourceVilim {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar]) =
    new UnaryResourceVilim(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar])
}
