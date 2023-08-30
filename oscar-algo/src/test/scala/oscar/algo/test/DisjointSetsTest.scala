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
package oscar.algo.test


import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.algo.search._
import oscar.algo.reversible._
import oscar.algo.DisjointSets
import oscar.algo.RangeMinQuery


class DisjointSetsTest extends FunSuite with Matchers  {

  test("test ds1") {
    val sets = new DisjointSets(1,4)
    sets.find(1).min should be(1)

    sets.union(1,2)
    sets.inSameSet(1,2) should be(true)
    sets.inSameSet(1,4) should be(false)
    sets.find(1).min should be(1)
    sets.find(1).max should be(2)
    
    
    sets.union(3,4)
    sets.inSameSet(3,4) should be(true)
    sets.inSameSet(1,4) should be(false)
    sets.find(4).min should be(3)
    sets.find(3).max should be(4)
    
    sets.union(1,1)
    sets.union(1,2)
    sets.inSameSet(1,2) should be(true)
    sets.inSameSet(1,4) should be(false)
    sets.find(1).min should be(1)
    sets.find(1).max should be(2)
    sets.inSameSet(3,4) should be(true)
    sets.inSameSet(1,4) should be(false)
    sets.find(4).min should be(3)
    sets.find(3).max should be(4)
    
    
    sets.union(2,3)
    sets.inSameSet(1,4) should be(true)
    sets.find(3).min should be(1)
    sets.find(1).max should be(4)
    sets.inSameSet(3,4) should be(true)
    sets.inSameSet(2,4) should be(true)
    sets.find(4).min should be(1)
    sets.find(3).max should be(4)   
  }
  



}

