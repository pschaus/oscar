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
import oscar.cp.testUtils.TestSuite

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._
import oscar.cp.scheduling._



/*class TestQuadraticCumulativeEdgeFinding extends TestSuite {
	
	test("Test 1: Example") {

		val horizon = 16
		val cp = CPScheduler(horizon)
	
		val nActivities = 6
		
		val durations = Array(4, 2, 1, 3, 1, 7)
		val starts    = Array(1, 5, 1, 1, 5, 0)
		val ends      = Array(8, 7, 8, 8, 7, 16)
		val cons      = Array(2, 2, 2, 1, 1, 1)
		
		val r = MaxResource(cp, 3)
		
		val activities = Array.tabulate(nActivities)({a => 
			val act = Activity(cp, durations(a))
			act startsEarlierAt starts(a)
			act endsLaterAt ends(a)
			act needs cons(a) ofResource r
			act
		})
		
		cp.add(new QuadraticCumulativeEdgeFinding(cp, r.cumulativeActivities, r.capacity, r.id))
		
		activities(5).est should be(6)
	}

	test("Test 2: LeftToRight vs RightToLeft") {

		val horizon = 16
		val cp = CPScheduler(horizon)
	
		val nActivities = 6
		
		val durations = Array(4, 2, 1, 3, 1, 7)
		val starts    = Array(1, 5, 1, 1, 5, 0)
		val ends      = Array(8, 7, 8, 8, 7, 16)
		val cons      = Array(2, 2, 2, 1, 1, 1)
		
		val r1 = MaxResource(cp, 3)
		val r2 = MaxResource(cp, 3)
		
		val activities1 = Array.tabulate(nActivities)({a => 
			val act = Activity(cp, durations(a))
			act startsEarlierAt starts(a)
			act endsLaterAt ends(a)
			act needs cons(a) ofResource r1
			act
		})
		
		val activities2 = Array.tabulate(nActivities)({a => 
			val act = Activity(cp, durations(a))
			act startsEarlierAt horizon-ends(a)
			act endsLaterAt horizon-starts(a)
			act needs cons(a) ofResource r2
			act
		})
		
		cp.add(new QuadraticCumulativeEdgeFinding(cp, r1.cumulativeActivities, r1.capacity, r1.id))
		cp.add(new QuadraticCumulativeEdgeFinding(cp, r2.cumulativeActivities, r2.capacity, r2.id))
		
		for (a <- 0 until nActivities) {
			activities1(a).est should be(horizon - activities2(a).lct)
			activities1(a).lst should be(horizon - activities2(a).ect)
			activities1(a).ect should be(horizon - activities2(a).lst)
			activities1(a).lct should be(horizon - activities2(a).est)
		}
	}
	
	test("Test 3: Example") {

		val horizon = 10
		val cp = CPScheduler(horizon)
	
		val nActivities = 3
		
		val durations = Array(3, 4, 5)
		val starts    = Array(1, 1, 2)
		val ends      = Array(5, 5, 10)
		val cons      = Array(1, 2, 1)
		
		val r = MaxResource(cp, 3)
		
		val activities = Array.tabulate(nActivities)({a => 
			val act = Activity(cp, durations(a))
			act startsEarlierAt starts(a)
			act endsLaterAt ends(a)
			act needs cons(a) ofResource r
			act
		})
		
		cp.add(new QuadraticCumulativeEdgeFinding(cp, r.cumulativeActivities, r.capacity, r.id))
		
		activities(2).est should be(4)
	}
	
	test("Test 4: LeftToRight vs RightToLeft") {

		val horizon = 16
		val cp = CPScheduler(horizon)
	
		val nActivities = 3
		
		val durations = Array(3, 4, 5)
		val starts    = Array(1, 1, 2)
		val ends      = Array(5, 5, 10)
		val cons      = Array(1, 2, 2)
		
		val r1 = MaxResource(cp, 3)
		val r2 = MaxResource(cp, 3)
		
		val activities1 = Array.tabulate(nActivities)({a => 
			val act = Activity(cp, durations(a))
			act startsEarlierAt starts(a)
			act endsLaterAt ends(a)
			act needs cons(a) ofResource r1
			act
		})
		
		val activities2 = Array.tabulate(nActivities)({a => 
			val act = Activity(cp, durations(a))
			act startsEarlierAt horizon-ends(a)
			act endsLaterAt horizon-starts(a)
			act needs cons(a) ofResource r2
			act
		})
		
		cp.add(new QuadraticCumulativeEdgeFinding(cp, r1.cumulativeActivities, r1.capacity, r1.id))
		cp.add(new QuadraticCumulativeEdgeFinding(cp, r2.cumulativeActivities, r2.capacity, r2.id))
		
		for (a <- 0 until nActivities) {
			activities1(a).est should be(horizon - activities2(a).lct)
			activities1(a).lst should be(horizon - activities2(a).ect)
			activities1(a).ect should be(horizon - activities2(a).lst)
			activities1(a).lct should be(horizon - activities2(a).est)
		}
	}
}*/
