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

package oscar.algo

/**
 * Bin-Packing Lower Bounds
 * @author Pierre Schaus
 */
object BinPacking extends App {
  
    println(labbeLB(Array(4,3,2,2,1),5))
    
    
    /**
     * For each index, the bin where the corresponding item is placed using a first-fit decreasing heuristic
     * The number of bin used is result.max+1
     * The items placed in bin i are result.filter(_ == i).
     */
    def firstFitDecreasing(win: Array[Int], c: Int): Array[Int] = {
      if (win.exists(w => w > c || w < 0)) throw new IllegalArgumentException("Item's weight should be less than capa and non negative")
      val items = win.zipWithIndex.sortBy(-_._1)
      var position = Array.fill(win.size)(0)
      var slack = Array.fill(win.size)(c)
      for ((w,i) <- items) {
        var j = 0
        while (slack(j) < w) j += 1
        slack(j) -= w
        position(i) = j
      }
      position
    }
    
    
    
    
  
	/**
	 * Lower-Bound introduced in:
	 * 
	 * "Capacitated vehicle routing on trees." 
	 *  Labbe Martine, Gilbert Laporte, and Helene Mercure. 
	 *  Operations Research 39.4 (1991): 616-622.
	 *
	 * @param w are the decreasing positive weights, 
	 * @param c is the capacity
	 */
	def labbeLB(w: Array[Int], c: Int): Int = {


		val n = w.length
		//val w = win ++ Array(0)

		//compute the number of items > c/2
		var ind1 = 0
		while (ind1 < w.length && w(ind1) > c/2) {
			ind1 += 1
		}
		var L3 = ind1
		//println(L3)
		//now we place items with  c/3 < w(i) <= c/2
		var ind2 = ind1
		while (ind2 < w.length && w(ind2) > c/3) {
			ind2 += 1
		}
		var ind3 = ind2-1
		var ind4 = 0
		while (ind4 < ind1 && ind3 >= ind1) {
			//we place item at index ind3 in the subsequence [ind4,ind1-1]
			do {
				ind4 += 1
			} while (c-w(ind4-1) < w(ind3))
			if (ind4 <= ind1)  ind3 -= 1 // could we place the item?
		}
		val H = ind3 - (ind1-1)
		L3 += (H+1)/2

		//computation of p(v)

		var a = 0
		var b = w.length - 1
		var v = 0
		var sum_ab = w.sum
		var p_v = 0

		/*
		 * invariant:
		 *
		 *   . > c-v     |a          c-v >= . >= v           b|  v > .
		 * +-------------|----------------|-------------------|----------0+
		 *                  . > c/2      e|      c/2 >= .
		 *
		 * sum_ab= sum_{i in [a..b]} w[i]
		 * v<=c/3
		 * v=w[b]
		 * b>=a
		 */

		var e = w.length - 1
		while (e >= 0 && w(e) <= c/2) {
			e -= 1
		}
		while (b >= a && v <= c/3){
		   
		    val tmp = Math.ceil(sum_ab.toDouble/c).toInt - (0.max(e-a+1)) - (H+1)/2
			p_v = p_v max tmp
			//reduce b
			while (b >= a && w(b) == v) {
				sum_ab -= w(b)
				b -= 1
			}
			if (b >= a) {
				v = w(b)
				//increase a
				while (b >= a && w(a) > c-v) {
					sum_ab -= w(a)
					a += 1
				}
			}
		}
		L3 += p_v
		return L3
	}//end of labbeLBB  

}