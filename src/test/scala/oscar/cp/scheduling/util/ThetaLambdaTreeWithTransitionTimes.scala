package oscar.cp.scheduling.util

/**
 * Created on 21/01/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 * @author Sascha Van Cauwelaert (sascha.vancauwelaert@gmail.com)
 */
class ThetaLambdaTreeWithTransitionTimes(nElements: Int, transitionSetLowerBounds: Array[Int]) {

  private final val UNDEF = -1

  private[this] val maxDepth = math.ceil(math.log(nElements) / math.log(2.0)).toInt
  private[this] val nTotalLeaves = math.pow(2.0, maxDepth).toInt
  private[this] val nPositions = (nTotalLeaves * 2) - 1
  private[this] val firstUsedLeafPosition = nTotalLeaves - 1
  private[this] val lastUsedLeafPosition = firstUsedLeafPosition + nElements - 1

  private[this] val ests = Array.fill(nPositions)(Int.MinValue)
  private[this] val durations = Array.fill(nPositions)(0)
  private[this] val sumDurations = Array.fill(nPositions)(0)
  private[this] val ects = Array.fill(nPositions)(Int.MinValue)
  private[this] val nLeaves = Array.fill(nPositions)(0)
  private[this] val isActivityPresent = Array.fill(nPositions)(false)
  private[this] val sumDurationsBar = Array.fill(nPositions)(0)
  private[this] val ectsBar = Array.fill(nPositions)(Int.MinValue)
  private[this] val nLeavesBar = Array.fill(nPositions)(0)
  private[this] val responsibleEctsBar = Array.fill(nPositions)(UNDEF)
  private[this] val responsibleSumDurationsBar = Array.fill(nPositions)(UNDEF)

  private[this] val cumulativeTTMin = transitionSetLowerBounds

  private[this] val activityIndexToLeafIndex: Array[Int] = Array.fill(nElements)(UNDEF)

  // Variables used in update
  private[this] var useOfGrayNodes = false
  private[this] var currentPos = UNDEF
  private[this] var leftSon = UNDEF
  private[this] var rightSon = UNDEF
  private[this] var sumDurationBarIfLeftGray = UNDEF
  private[this] var sumDurationBarIfRightGray = UNDEF
  private[this] var ectBarIfRightEctResponsible = UNDEF
  private[this] var ectBarIfRightDurationSumResponsible = UNDEF
  private[this] var ectBarIfLeftEctResponsible = UNDEF
  private[this] var rootUpdated = false

  // Variables used in most functions
  private[this] var actiIndex = UNDEF
  private[this] var nodeIndex = UNDEF

  // Empties the tree and resets the node content to default values
  final def clearAndPlaceLeaves(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int]) = {
    useOfGrayNodes = false
    var p = 0
    while(p < nPositions) {
      if (p < nElements) {
        activityIndexToLeafIndex(sortedIndexes(p)) = p
      }
      // if it is a used leaf, we update the ests and durations of the leaf according to corresponding activity
      if (p >= firstUsedLeafPosition && p <= lastUsedLeafPosition) {
        actiIndex = p - firstUsedLeafPosition
        ests(p) = currentEsts(sortedIndexes(actiIndex))
        durations(p) = currentMinDurations(sortedIndexes(actiIndex))
      }
      // else default values
      else {
        ests(p) = Int.MinValue
        durations(p) = 0
      }

      //reset node
      sumDurations(p) = 0
      ects(p) = Int.MinValue
      nLeaves(p) = 0
      isActivityPresent(p) = false

      p += 1
    }
  }

  // Fills the tree and updates all the nodes to correct values
  final def fillAndPlaceLeaves(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int]) = {
    useOfGrayNodes = true

    //Setting the leaves up to date
    var p = firstUsedLeafPosition
    while(p < nPositions) {
      // if it is a used leaf, we update the ests and durations of the leaf according to corresponding activity
      if (p <= lastUsedLeafPosition) {
        actiIndex = p - firstUsedLeafPosition
        activityIndexToLeafIndex(sortedIndexes(actiIndex)) = actiIndex
        ests(p) = currentEsts(sortedIndexes(actiIndex))
        durations(p) = currentMinDurations(sortedIndexes(actiIndex))
        isActivityPresent(p) = true
        sumDurations(p) = durations(p)
        ects(p) = ests(p) + durations(p)
        nLeaves(p) = 1
        sumDurationsBar(p) = sumDurations(p)
        ectsBar(p) = ects(p)
        nLeavesBar(p) = 1
      }
      // else default values
      else {
        ests(p) = Int.MinValue
        durations(p) = 0
        isActivityPresent(p) = false
        sumDurations(p) = 0
        ects(p) = Int.MinValue
        nLeaves(p) = 0
        sumDurationsBar(p) = 0
        ectsBar(p) = Int.MinValue
        nLeavesBar(p) = 0
      }
      //reset node
      responsibleEctsBar(p) = UNDEF
      responsibleSumDurationsBar(p) = UNDEF
      p += 1
    }

    var nNodesAtLevel = nTotalLeaves / 2
    p = (firstUsedLeafPosition - 1) / 2
    while (nNodesAtLevel > 0) {
      updateLevelPositions(p, p + nNodesAtLevel)
      nNodesAtLevel = nNodesAtLevel / 2
      p = (p - 1) / 2
    }
  }

  // Inserts the activityIndex^th activity in the tree and recursively updates its ancestors
  final def insert(activityIndex: Int) = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurations(nodeIndex) = durations(nodeIndex)
    ects(nodeIndex) = ests(nodeIndex) + durations(nodeIndex)
    nLeaves(nodeIndex) = 1
    isActivityPresent(nodeIndex) = true
    if(useOfGrayNodes) {
      sumDurationsBar(nodeIndex) = sumDurations(nodeIndex)
      ectsBar(nodeIndex) = ects(nodeIndex)
      nLeavesBar(nodeIndex) = 1
    }
    update((nodeIndex - 1) / 2)
  }

  // Removes the activityIndex^th activity in the tree and recursively updates its ancestors
  final def remove(activityIndex: Int) = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurations(nodeIndex) = 0
    ects(nodeIndex) = Int.MinValue
    nLeaves(nodeIndex) = 0
    isActivityPresent(nodeIndex) = false
    if(useOfGrayNodes) {
      sumDurationsBar(nodeIndex) = 0
      ectsBar(nodeIndex) = Int.MinValue
      nLeavesBar(nodeIndex) = 0
      responsibleEctsBar(nodeIndex) = UNDEF
      responsibleSumDurationsBar(nodeIndex) = UNDEF
    }
    update((nodeIndex - 1) / 2)
  }

  // Computes the ECT of the tree without the activity at activityIndex (without modifying the tree)
  final def ectWithoutActivity(activityIndex: Int): Int = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    if (!isActivityPresent(nodeIndex)) {
      ects(0)
    }
    else {
      remove(activityIndex)
      val ectWithout = ects(0)
      insert(activityIndex)
      ectWithout
    }
  }

  final def grayActivity(activityIndex: Int): Unit = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurations(nodeIndex) = 0
    ects(nodeIndex) = Int.MinValue
    nLeaves(nodeIndex) = 0
    isActivityPresent(nodeIndex) = false
    responsibleEctsBar(nodeIndex) = nodeIndex
    responsibleSumDurationsBar(nodeIndex) = nodeIndex
    update((nodeIndex - 1) / 2)
  }

  @inline final def ect : Int = ects(0)
  @inline final def ectBar : Int = ectsBar(0)
  @inline final def sumDuration : Int = sumDurations(0)
  @inline final def sumDurationBar : Int = sumDurationsBar(0)
  @inline final def nLeavesRoot : Int = nLeaves(0)
  @inline final def nLeavesBarRoot : Int = nLeavesBar(0)
  @inline final def responsibleEctBar : Int = responsibleEctsBar(0) - firstUsedLeafPosition

  // Updates the values contained in node p
  @inline
  private def update(pos: Int) : Unit = {
    currentPos = pos
    rootUpdated = false

    while (!rootUpdated) {
      leftSon = 2 * currentPos + 1
      rightSon = 2 * currentPos + 2
      sumDurations(currentPos) = sumDurations(leftSon) + sumDurations(rightSon)
      ects(currentPos) = math.max(ects(rightSon), ects(leftSon) + sumDurations(rightSon) + cumulativeTTMin(nLeaves(rightSon)))
      nLeaves(currentPos) = nLeaves(leftSon) + nLeaves(rightSon)

      // Needed by EF and optional activities
      if (useOfGrayNodes) {
        sumDurationBarIfLeftGray = sumDurationsBar(leftSon) + sumDurations(rightSon)
        sumDurationBarIfRightGray = sumDurations(leftSon) + sumDurationsBar(rightSon)

        ectBarIfRightEctResponsible = ectsBar(rightSon)
        ectBarIfRightDurationSumResponsible = ects(leftSon) + sumDurationsBar(rightSon) + cumulativeTTMin(nLeavesBar(rightSon))
        ectBarIfLeftEctResponsible = ectsBar(leftSon) + sumDurations(rightSon) + cumulativeTTMin(nLeaves(rightSon))

        if(sumDurationBarIfLeftGray > sumDurationBarIfRightGray) {
          sumDurationsBar(currentPos) = sumDurationBarIfLeftGray
          responsibleSumDurationsBar(currentPos) = responsibleSumDurationsBar(leftSon)
        }
        else {
          sumDurationsBar(currentPos) = sumDurationBarIfRightGray
          responsibleSumDurationsBar(currentPos) = responsibleSumDurationsBar(rightSon)
        }

        if(ectBarIfRightEctResponsible >= ectBarIfRightDurationSumResponsible && ectBarIfRightEctResponsible >= ectBarIfLeftEctResponsible) {
          ectsBar(currentPos) = ectBarIfRightEctResponsible
          responsibleEctsBar(currentPos) = responsibleEctsBar(rightSon)
        }
        else if (ectBarIfRightDurationSumResponsible >= ectBarIfLeftEctResponsible) {
          ectsBar(currentPos) = ectBarIfRightDurationSumResponsible
          responsibleEctsBar(currentPos) = responsibleSumDurationsBar(rightSon)
        }
        else {
          ectsBar(currentPos) = ectBarIfLeftEctResponsible
          responsibleEctsBar(currentPos) = responsibleEctsBar(leftSon)
        }
        nLeavesBar(currentPos) = if (responsibleEctsBar(currentPos) == UNDEF) nLeaves(currentPos) else nLeaves(currentPos) + 1
      }
      //did we just update the root ?
      rootUpdated = currentPos == 0

      //go to father
      currentPos = (currentPos - 1) / 2
    }
  }

  // Updates the values contained in node from minPos until maxPos (updates a level of the tree)
  @inline
  private def updateLevelPositions(minPos: Int, maxPos: Int) : Unit = {
    currentPos = minPos
    leftSon = UNDEF
    rightSon = UNDEF

    while (currentPos < maxPos) {
      leftSon = 2 * currentPos + 1
      rightSon = 2 * currentPos + 2
      sumDurations(currentPos) = sumDurations(leftSon) + sumDurations(rightSon)
      ects(currentPos) = math.max(ects(rightSon), ects(leftSon) + sumDurations(rightSon) + cumulativeTTMin(nLeaves(rightSon)))
      nLeaves(currentPos) = nLeaves(leftSon) + nLeaves(rightSon)

      sumDurationsBar(currentPos) = sumDurations(currentPos)
      ectsBar(currentPos) = ects(currentPos)
      responsibleEctsBar(currentPos) = UNDEF
      responsibleSumDurationsBar(currentPos) = UNDEF
      nLeavesBar(currentPos) = nLeaves(currentPos)

      // go to next node
      currentPos += 1
    }
  }
}
