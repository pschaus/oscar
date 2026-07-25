package oscar.cp.constraints

import java.util.{Set => JSet}
import scala.jdk.CollectionConverters._

class Automaton(val nbStates: Int, val nbLetters: Int, val initialState: Int, acceptingStatesJava: JSet[Integer]) {
  
  private var posted: Boolean = false
  private val nullState: Int = -1
  
  private val T: Array[Array[Int]] = Array.fill(nbStates, nbLetters)(nullState)
  private val acceptingStates: Set[Int] = acceptingStatesJava.asScala.map(_.toInt).toSet

  for (q <- acceptingStates) {
    if (q >= nbStates || q < 0) {
      throw new RuntimeException(s"accepting states must be between 0 and ${nbStates - 1}")
    }
  }
  if (initialState >= nbStates || initialState < 0) {
    throw new RuntimeException(s"initial must be between 0 and ${nbStates - 1}")
  }
  if (nbStates <= 0 || nbLetters <= 0) {
    throw new RuntimeException("nbStates and nbLetters must be >0")
  }

  private[constraints] def setPosted(): Unit = {
    posted = true
  }

  def addTransition(state1: Int, state2: Int, letter: Int): Unit = {
    if (posted) {
      throw new RuntimeException("Automaton: automaton cannot be modified after being used in a constraint")
    }
    if (state1 >= nbStates || state1 < 0 ||
        state2 >= nbStates || state2 < 0 ||
        letter >= nbLetters || letter < 0) {
      println(s"$state1 $state2 $letter")
      throw new RuntimeException("Automaton: invalid transition according to Alphabet and States")
    }
    if (T(state1)(letter) != nullState) {
      throw new RuntimeException("Automaton: this transition already exists (automaton must be deterministic)")
    }
    T(state1)(letter) = state2
  }

  def getTransitionMatrix: Array[Array[Int]] = T

  def getNbStates: Int = nbStates

  def getNbLetters: Int = nbLetters

  def getNullState: Int = nullState

  def getInitialState: Int = initialState

  // Keep compatibility if it's accessed from Java
  def getAcceptingStates: JSet[Integer] = acceptingStatesJava
  
  // Scala friendly
  def getAcceptingStatesScala: Set[Int] = acceptingStates
}
