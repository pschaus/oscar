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
package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.constraints._
import oscar.cp._
import collection.immutable.SortedSet
import org.scalatest.Matchers
import oscar.util._
import oscar.cp.scheduling.search.Ranker
import oscar.cp.scheduling.search.RankBranching
import oscar.cp.scheduling.search.RankBranching

class TestUnaryRank extends FunSuite with Matchers  {


  
  test("UnaryRank1") {
      
    val cp = CPSolver()
    val n = 5
    val starts = Array.fill(n)(CPIntVar(0 to 10)(cp))
    val durs = Array.fill(n)(CPIntVar(1)(cp))
    val ends = starts.zip(durs).map{ case(s,d) => s+d }
    cp.add(unaryResource(starts, durs, ends))
    val ranker = new Ranker(starts,durs,ends)
    
    cp.search {
      if (ranker.isRanked.value) noAlternative
      else ranker.rankNext(i => ends(i).max)
    }
    val stat = cp.start()
    stat.nSols should be(120)
    
  }
  
  test("UnaryRank2") {
      
    val cp = CPSolver()
    val n = 5
    val starts = Array.fill(n)(CPIntVar(0 to 10)(cp))
    val durs = Array.fill(n)(CPIntVar(1)(cp))
    val ends = starts.zip(durs).map{ case(s,d) => s+d }
    cp.add(unaryResource(starts, durs, ends))

    cp.search {
      rank(starts,durs,ends,i => (ends(i).max,starts(i).max))
    }
    val stat = cp.start()
    stat.nSols should be(120)
    
  }
  
  test("UnaryRank3") {
      
    val cp = CPSolver()
    val n = 5
    val starts = Array.fill(n)(CPIntVar(0 to 10)(cp))
    val durs = Array.fill(n)(CPIntVar(1)(cp))
    val ends = starts.zip(durs).map{ case(s,d) => s+d }
    cp.add(unaryResource(starts, durs, ends))

    cp.search {
      rank(starts,durs,ends,i => (ends(i).max,starts(i).max))
    }
    val stat = cp.start()
    stat.nSols should be(120)
    
  }   
  

  


}
