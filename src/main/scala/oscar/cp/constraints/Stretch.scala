package oscar.cp.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.util.ArrayUtils

import java.util.Set
import java.util.TreeSet

/**
 * Stretch Constraint: Constraint the maximum/minimum consecutive occurrences of numbers and transitions between them
 * @author Pierre Schaus pschaus@gmail.com
 */
object Stretch {

  /**
   * Creates an automaton that will ensure that value/letter i will appear in sequence of length of at least shortest[i]
   * and at most longest[i] in the sequence x. Such a sequence is called a stretch for the letter i.
   * @param x
   * @param shortest
   * @param longest
   * @return an automaton to pass as argument to the regular constraint
   * @see Regular
   *
   */
  def getStretchAutomaton(x: Array[CPIntVar], shortest: Array[Int], longest: Array[Int]): Automaton = {
    val maxval = math.max(shortest.length, longest.length) - 1
    val nbval = maxval + 1
    val transiFrom = new Array[Int](nbval * nbval - nbval)
    val transiTo = new Array[Int](nbval * nbval - nbval)
    var i = 0
    for (j <- 0 to maxval) {
      for (k <- 0 to maxval) {
        if (j != k) {
          transiFrom(i) = j
          transiTo(i) = k
          i += 1
        }
      }
    }
    getStretchAutomaton(x, shortest, longest, transiFrom, transiTo)
  }

  /**
   * Creates an automaton that will ensure that value/letter i will appear in sequence of length of at least shortest[i]
   * and at most longest[i] in the sequence x. Such a sequence is called a stretch for the letter i. <br>
   * Also the only possible transition to go from on stretch to the next are (transiFrom[j],transiTo[j]) for all j. <br>
   * Example  x= [1,1,0,0,3,3,3] shortest=[2,1,0,3] longest=[3,2,1,3] transiFrom=[1,0] transiTo=[0,3].
   * @param x
   * @param shortest
   * @param longest
   * @param transiFrom
   * @param transiTo
   * @return
   */
  def getStretchAutomaton(x: Array[CPIntVar], shortest: Array[Int], longest: Array[Int], transiFrom: Array[Int], transiTo: Array[Int]): Automaton = {
    if (transiFrom.length != transiTo.length) {
      throw new RuntimeException("getStretchAutomaton: transiFrom and transiTo must have the same length")
    }

    val maxval = math.max(shortest.length, longest.length) - 1
    val nbval = maxval + 1
    val sh = new Array[Int](nbval)
    val lo = new Array[Int](nbval)

    for (i <- lo.indices) {
      sh(i) = 1
      lo(i) = x.length
    }
    for (i <- shortest.indices) {
      if (shortest(i) > sh(i)) {
        sh(i) = shortest(i)
      }
    }
    for (i <- longest.indices) {
      if (longest(i) < lo(i)) {
        lo(i) = longest(i)
      }
    }
    val nbStates = ArrayUtils.sum(longest) + 1
    val stateStart = new Array[Int](nbval)
    val stateEnd = new Array[Int](nbval)

    val accepting: Set[Integer] = new TreeSet[Integer]()
    var i = 1
    for (k <- 0 until nbval) {
      stateStart(k) = i
      stateEnd(k) = i + lo(k) - 1
      for (j <- sh(k) to lo(k)) {
        accepting.add(i + j - 1)
      }
      i += lo(k)
    }

    val automaton = new Automaton(nbStates, nbval, 0, accepting)

    for (v <- 0 until nbval) {
      automaton.addTransition(0, stateStart(v), v)
      for (j <- stateStart(v) until stateEnd(v)) {
        automaton.addTransition(j, j + 1, v)
      }
    }

    for (j <- transiFrom.indices) {
      val from = transiFrom(j)
      val to = transiTo(j)
      for (s <- stateStart(from) + sh(from) - 1 to stateEnd(from)) {
        automaton.addTransition(s, stateStart(to), to)
      }
    }

    automaton
  }

}
