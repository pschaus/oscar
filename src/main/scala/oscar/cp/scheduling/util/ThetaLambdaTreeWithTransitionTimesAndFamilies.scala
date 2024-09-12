package oscar.cp.scheduling.util

/**
 * Created on 25/08/16.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 * @author Sascha Van Cauwelaert (sascha.vancauwelaert@gmail.com)
 */
class ThetaLambdaTreeWithTransitionTimesAndFamilies(nElements: Int, familyTransitionSetLowerBounds: Array[Int], family: Array[Int]) {
  //bitset representation on Int)
  assert(family.max < 32)

  protected final val UNDEF = -1

  protected[this] val maxDepth = math.ceil(math.log(nElements) / math.log(2.0)).toInt
  protected[this] val nTotalLeaves = math.pow(2.0, maxDepth).toInt
  protected[this] val nPositions = (nTotalLeaves * 2) - 1
  protected[this] val firstUsedLeafPosition = nTotalLeaves - 1
  protected[this] val lastUsedLeafPosition = firstUsedLeafPosition + nElements - 1

  protected[this] val ests = Array.fill(nPositions)(Int.MinValue)
  protected[this] val durations = Array.fill(nPositions)(0)
  protected[this] val sumDurations = Array.fill(nPositions)(0)
  protected[this] val ects = Array.fill(nPositions)(Int.MinValue)
  protected[this] val isActivityPresent = Array.fill(nPositions)(false)
  protected[this] val sumDurationsBar = Array.fill(nPositions)(0)
  protected[this] val ectsBar = Array.fill(nPositions)(Int.MinValue)

  protected[this] val activityIndexToLeafIndex: Array[Int] = Array.fill(nElements)(UNDEF)

  // Variables used in update
  protected[this] var useOfGrayNodes = false
  protected[this] var currentPos = UNDEF
  protected[this] var leftSon = UNDEF
  protected[this] var rightSon = UNDEF
  protected[this] var sumDurationBarIfLeftGray = UNDEF
  protected[this] var sumDurationBarIfRightGray = UNDEF
  protected[this] var ectBarIfRightEctResponsible = UNDEF
  protected[this] var ectBarIfRightDurationSumResponsible = UNDEF
  protected[this] var ectBarIfLeftEctResponsible = UNDEF
  protected[this] var rootUpdated = false

  // Variables used in most functions
  protected[this] var actiIndex = UNDEF
  protected[this] var nodeIndex = UNDEF

  protected[this] val families: Array[Int] = Array.fill(nPositions)(0)
  val familiesBar = Array.fill(nPositions)(0)

  var interSectSize = UNDEF
  var cardFamilies = UNDEF
  var interSectSizeWithGray = UNDEF
  var additionalGrayInRight = UNDEF
  var interSectSizeRightGray = UNDEF
  var cardFamiliesRightGray = UNDEF
  var cardFamiliesLeftGray = UNDEF

  // Empties the tree and resets the node content to default values
  @inline
  def clearAndPlaceLeaves(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int]) = {
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
      isActivityPresent(p) = false
      families(p) = 0

      p += 1
    }
  }

  @inline
  def fillLeavesOnly(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int]) = {
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
        families(p) = 1 << family(sortedIndexes(actiIndex))
        isActivityPresent(p) = true
        sumDurations(p) = durations(p)
        ects(p) = ests(p) + durations(p)
        sumDurationsBar(p) = sumDurations(p)
        ectsBar(p) = ects(p)
        familiesBar(p) = families(p)
      }
      // else default values
      else {
        ests(p) = Int.MinValue
        durations(p) = 0
        families(p) = 0
        isActivityPresent(p) = false
        sumDurations(p) = 0
        ects(p) = Int.MinValue
        sumDurationsBar(p) = 0
        ectsBar(p) = Int.MinValue
      }
      //reset node
      p += 1
    }
  }

  @inline
  def insertOnly(activityIndex: Int) = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurations(nodeIndex) = durations(nodeIndex)
    ects(nodeIndex) = ests(nodeIndex) + durations(nodeIndex)
    isActivityPresent(nodeIndex) = true
    families(nodeIndex) = 1 << family(activityIndex)
    if(useOfGrayNodes) {
      sumDurationsBar(nodeIndex) = sumDurations(nodeIndex)
      ectsBar(nodeIndex) = ects(nodeIndex)
      familiesBar(nodeIndex) = families(nodeIndex)
    }
  }

  @inline
  def removeOnly(activityIndex: Int) = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurations(nodeIndex) = 0
    ects(nodeIndex) = Int.MinValue
    isActivityPresent(nodeIndex) = false
    families(nodeIndex) = 0//HashSet[Int]()
    if(useOfGrayNodes) {
      sumDurationsBar(nodeIndex) = 0
      ectsBar(nodeIndex) = Int.MinValue
      familiesBar(nodeIndex) = 0
    }
  }

  @inline
  def grayActivityOnly(activityIndex: Int): Unit = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurations(nodeIndex) = 0
    ects(nodeIndex) = Int.MinValue
    families(nodeIndex) = 0 //HashSet[Int]()
    isActivityPresent(nodeIndex) = false
  }

  // Fills the tree and updates all the nodes to correct values
  @inline
  def fillAndPlaceLeaves(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int]) = {
    fillLeavesOnly(sortedIndexes, currentEsts, currentMinDurations)
    updateInternalNodesByLevel()
  }

  @inline
  final protected def updateInternalNodesByLevel() = {
    var nNodesAtLevel = nTotalLeaves / 2
    var p = (firstUsedLeafPosition - 1) / 2
    while (nNodesAtLevel > 0) {
      updateLevelPositions(p, p + nNodesAtLevel)
      nNodesAtLevel = nNodesAtLevel / 2
      p = (p - 1) / 2
    }
  }

  // Inserts the activityIndex^th activity in the tree and recursively updates its ancestors
  @inline
  def insert(activityIndex: Int) = {
    insertOnly(activityIndex)
    update((nodeIndex - 1) / 2)
  }

  // Removes the activityIndex^th activity in the tree and recursively updates its ancestors
  @inline
  def remove(activityIndex: Int) = {
    removeOnly(activityIndex)
    update((nodeIndex - 1) / 2)
  }

  // Computes the ECT of the tree without the activity at activityIndex (without modifying the tree)
  // Returns a pair (ect, bitset)
  @inline
  final def ectWithoutActivity(activityIndex: Int): (Int, Int) = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    if (!isActivityPresent(nodeIndex)) {
      ect
    }
    else {
      remove(activityIndex)
      val ectWithout = ect
      insert(activityIndex)
      ectWithout
    }
  }

  @inline
  final def grayActivity(activityIndex: Int): Unit = {
    grayActivityOnly(activityIndex)
    update((nodeIndex - 1) / 2)
  }

  // Returns a pair (ect, bitset)
  @inline def ect : (Int, Int) = (ects(0), families(0))
  @inline final def ectBar : Int = ectsBar(0)
  @inline final def sumDuration : Int = sumDurations(0)
  @inline final def sumDurationBar : Int = sumDurationsBar(0)
  @inline final def responsibleEctBar: Int = getResponsibleEctBar(0) - firstUsedLeafPosition

  @inline
  def getResponsibleSumDurationsBar(pos: Int): Int = {
    currentPos = pos
    while (currentPos < firstUsedLeafPosition || currentPos > lastUsedLeafPosition) {
      leftSon = 2 * currentPos + 1
      rightSon = 2 * currentPos + 2

      if (sumDurationsBar(currentPos) == sumDurationsBar(leftSon) + sumDurations(rightSon)) {
        currentPos = leftSon
      }
      else if (sumDurationsBar(currentPos) == sumDurations(leftSon) + sumDurationsBar(rightSon)) {
        currentPos = rightSon
      }
      else {
        println("An unexpected error has occurred")
        println("Responsible error at pos: " + currentPos)
        return UNDEF
      }
    }
    currentPos
  }

  // Updates the values contained in node p
  @inline
  final protected def update(pos: Int) : Unit = {
    currentPos = pos
    rootUpdated = false

    while (!rootUpdated) {
      updateCurrentPosition()

      // Needed by EF and optional activities
      if (useOfGrayNodes) {
        updateCurrentPositionGray()
      }
      //did we just update the root ?
      rootUpdated = currentPos == 0

      //go to father
      currentPos = (currentPos - 1) / 2
    }
  }

  @inline
  def computePotentialEctBar(): Unit = {
    // Computes ECTBar if gray activity is on the right
    interSectSizeRightGray = Integer.bitCount(familiesBar(rightSon) & families(leftSon))
    cardFamiliesRightGray = Integer.bitCount(familiesBar(rightSon)) - interSectSizeRightGray - 1
    if (interSectSizeRightGray == 0 && familiesBar(rightSon) > 0 && families(leftSon) > 0) {
      cardFamiliesRightGray += 1
    }
    if (cardFamiliesRightGray < 0) {
      cardFamiliesRightGray = 0
    }
    ectBarIfRightDurationSumResponsible = ects(leftSon) + sumDurationsBar(rightSon) + familyTransitionSetLowerBounds(cardFamiliesRightGray)

    // Computes ECTBar if gray activity is on the left
    interSectSizeWithGray = Integer.bitCount(families(rightSon) & familiesBar(leftSon))
    cardFamiliesLeftGray = Integer.bitCount(families(rightSon)) - interSectSizeWithGray - 1
    if (interSectSizeWithGray == 0 && families(rightSon) > 0 && familiesBar(leftSon) > 0) {
      cardFamiliesLeftGray += 1
    }
    if (cardFamiliesLeftGray < 0) {
      cardFamiliesLeftGray = 0
    }
    ectBarIfLeftEctResponsible = ectsBar(leftSon) + sumDurations(rightSon) + familyTransitionSetLowerBounds(cardFamiliesLeftGray)
  }

  @inline
  def getResponsibleEctBar(pos: Int): Int = {
    currentPos = pos
    while (currentPos < firstUsedLeafPosition || currentPos > lastUsedLeafPosition) {
      leftSon = 2 * currentPos + 1
      rightSon = 2 * currentPos + 2

      interSectSize = Integer.bitCount(families(rightSon) & families(leftSon))
      cardFamilies = Integer.bitCount(families(rightSon)) - interSectSize - 1
      if (interSectSize == 0 && families(rightSon) > 0 && families(leftSon) > 0) {
        cardFamilies += 1
      }
      if (cardFamilies < 0) {
        cardFamilies = 0
      }

      computePotentialEctBar()

      // This deals with the case where there are no gray activities in the tree anymore
      if (ects(currentPos) == ectsBar(currentPos)) {
        return UNDEF
      }
      else if (ectsBar(currentPos) == ectsBar(rightSon)) {
        currentPos = rightSon
      }
      else if (ectsBar(currentPos) == ectBarIfRightDurationSumResponsible) {
        return getResponsibleSumDurationsBar(rightSon)
      }
      else if (ectsBar(currentPos) == ectBarIfLeftEctResponsible) {
        currentPos = leftSon
      }
      else {
        println("An unexpected error occurred")
        UNDEF
      }
    }
    currentPos
  }

  @inline
  def updateCurrentPosition() = {
    leftSon = 2 * currentPos + 1
    rightSon = 2 * currentPos + 2
    sumDurations(currentPos) = sumDurations(leftSon) + sumDurations(rightSon)
    interSectSize = Integer.bitCount(families(rightSon) & families(leftSon))
    cardFamilies = Integer.bitCount(families(rightSon)) - interSectSize - 1
    if (interSectSize == 0 && families(rightSon) > 0 && families(leftSon) > 0) {
      cardFamilies += 1
    }
    if (cardFamilies < 0) {
      cardFamilies = 0
    }

    if (ects(rightSon) >= ects(leftSon) + sumDurations(rightSon) + familyTransitionSetLowerBounds(cardFamilies)) {
      ects(currentPos) = ects(rightSon)
      families(currentPos) = families(rightSon)
    }
    else {
      ects(currentPos) = ects(leftSon) + sumDurations(rightSon) + familyTransitionSetLowerBounds(cardFamilies)
      families(currentPos) = families(leftSon) | families(rightSon)
    }
  }

  @inline
  def updateCurrentPositionGray() = {
    sumDurationBarIfLeftGray = sumDurationsBar(leftSon) + sumDurations(rightSon)
    sumDurationBarIfRightGray = sumDurations(leftSon) + sumDurationsBar(rightSon)

    ectBarIfRightEctResponsible = ectsBar(rightSon)

    computePotentialEctBar()

    // Gray is on the right subtree and it is the right ectBar that is responsible
    if(ectBarIfRightEctResponsible >= ectBarIfRightDurationSumResponsible && ectBarIfRightEctResponsible >= ectBarIfLeftEctResponsible) {
      ectsBar(currentPos) = ectBarIfRightEctResponsible
      sumDurationsBar(currentPos) = sumDurationBarIfRightGray

      familiesBar(currentPos) = familiesBar(rightSon)
    }
    // Gray is on the right subtree and it is the right sumDurBar that is responsible
    else if (ectBarIfRightDurationSumResponsible >= ectBarIfLeftEctResponsible && sumDurationsBar(rightSon) > 0) { // the second condition checks whether the right theta-lambda tree is not empty
      ectsBar(currentPos) = ectBarIfRightDurationSumResponsible
      sumDurationsBar(currentPos) = sumDurationBarIfRightGray

      familiesBar(currentPos) = families(leftSon) | familiesBar(rightSon)
    }
    // Gray is on the left subtree and it is the left ectBar that is responsible
    else {
      ectsBar(currentPos) = ectBarIfLeftEctResponsible
      sumDurationsBar(currentPos) = sumDurationBarIfLeftGray

      familiesBar(currentPos) = familiesBar(leftSon) | families(rightSon)
    }
  }

  @inline
  def resetGrayCurrentPosition(): Unit = {
    sumDurationsBar(currentPos) = sumDurations(currentPos)
    ectsBar(currentPos) = ects(currentPos)
    familiesBar(currentPos) = families(currentPos)
  }

  // Updates the values contained in node from minPos until maxPos (updates a level of the tree)
  @inline
  protected def updateLevelPositions(minPos: Int, maxPos: Int) : Unit = {
    currentPos = minPos
    leftSon = UNDEF
    rightSon = UNDEF

    while (currentPos < maxPos) {
      updateCurrentPosition()
      resetGrayCurrentPosition()

      // go to next node
      currentPos += 1
    }
  }
}
