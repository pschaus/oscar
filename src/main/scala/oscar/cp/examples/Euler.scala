package oscar.cp.examples

import java.awt.Color

import oscar.cp._

/**
 * Euler Problem, a knight must visit every position of a chess board once and come back to its initial position
 * using only valid knight moves.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object Euler extends CPModel with App {

  val n = 6 // multiple of two

  def reachables(i: Int): Set[Int] = {
    val l = i % n
    val c = i / n

    (for ((dl,dc) <- Seq((1,2),(2,1),(-1,2),(-2,1),(1,-2),(2,-1),(-1,-2),(-2,-1))) yield {
      ((l+dl),(c+dc))
    }).filter{case(l,c) => l >= 0 && l < n && c >= 0 && c < n}.map{case(a,b) => b*n + a}.toSet

  }

  val x = (0 until n*n).map(v => CPIntVar(reachables(v)))

  add(circuit(x))

  search {
    binaryFirstFail(x)
  }

  val stats = start(1)

  println(stats)
}
