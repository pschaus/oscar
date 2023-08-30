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
 ******************************************************************************/

package oscar.cp.scheduling.search

import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.algo.reversible._
import oscar.algo.search.Branching
import oscar.cp.constraints.UnaryRank
import oscar.algo._

/**
 * Rank Branching
 * author: Pierre Schaus pschaus@gmail.com
 */
class RankBranching[T](starts: IndexedSeq[CPIntVar], durations: IndexedSeq[CPIntVar], ends: IndexedSeq[CPIntVar], f: Int => T)(implicit orderer: T => Ordered[T]) extends Branching {

 val ranker = new Ranker(starts,durations,ends) 
  
 def alternatives(): Seq[Alternative] = {
   if (ranker.isRanked.value) noAlternative
   else ranker.rankNext(f)
 }
    
  
}

class Ranker(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar]) {

  val rankCons = new UnaryRank(starts, durations, ends)
  rankCons.s.add(rankCons)
  val rankVar = rankCons.ranks

  def rankNext[T](f: Int => T)(implicit orderer: T => Ordered[T]): Seq[Alternative] = {
    val cr = rankCons.rank.value

    selectMin(0 until starts.size)(i => rankVar(i).hasValue(cr))(i => f(i)) match {
      case Some(i: Int) => branch(rankCons.s.add(rankVar(i) === cr))(rankCons.s.add(rankVar(i).diff(cr)))
      case None => {
        throw new Error("shoudl not happen, alldiff should fail")
      }
    }
  }

  def isRanked = rankCons.isRanked
}

