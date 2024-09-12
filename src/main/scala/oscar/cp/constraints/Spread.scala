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
package oscar.cp.constraints

import oscar.cp._
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar


/**
 * Implementation Spread Constraint
 * A Spread Constraint is the conjunction of two constraints
 * 1) Sum(i) x(i) == sum (a constant)
 * 2) Sum(i) x(i)*x(i) = sum2 (a variable)
 * And you generally want to maximize to minimize sum2 variable knowing the sum
 * @author Pierre Schaus pschaus@gmail.com
 */
class Spread(val x: Array[CPIntVar], val sum: Int, val sum2: CPIntVar, val reverse: Boolean = true) extends Constraint(x(0).store, "Spread") {
  val n = x.size
  val xmin = Array.fill(n)(0)
  val xmax = Array.fill(n)(0)
  val bnds = Array.fill(2*n)((0,0)) // will contain the lower and upper bounds of the n domains + bounds type (1 = lower, 0 = upper)
 
  val epsilon = 10e-6

  override def associatedVars(): Iterable[CPVar] = x ++ Array(sum2)

  override def setup(l: CPPropagStrength): Unit = {
    if (reverse) {
      s.post(new Spread(x.map(-_),-sum,sum2,false))
      decomposition()
    }
    
    x.foreach(_.callPropagateWhenBoundsChange(this))
    sum2.callPropagateWhenBoundsChange(this)
    propagate()
  }
  
  def decomposition(): Unit = {
    s.post(new Sum(x.map(i => i*i),sum2))
    s.post(new Sum(x,CPIntVar(sum)(s)))
	}
  
  def propagateSum(): Unit = {
    val summin = x.map(_.min).sum
    val summax = x.map(_.max).sum
    // check that sum constraint is feasible and prune it (fix point on it)
    for (i <- 0 until n) {
      x(i).updateMin(sum - (summax - x(i).max))
      x(i).updateMax(sum - (summin - x(i).min))
    }
  }

 
  override def propagate(): Unit = {
    // first make sure the the sum is consistent
    propagateSum()
    // check that not every variable is bound otherwise we can assign the sum of square directly
    if (x.forall(_.isBound)) {
      return sum2.assign(x.map(i => i.value*i.value).sum)
    }
    
    // let's go ...
    
    // --- step1: compute some useful values like extrema sums, intervals ... required by the algo ---
    
    for (i <- 0 until n) {
      xmin(i) = x(i).min
      xmax(i) = x(i).max
    }

    for(i <- 0 until n) {
      bnds(i) = (x(i).min, 1)
      bnds(n+i) = (x(i).max, 0)
    }
    def bound(i: Int) = bnds(i)._1
    def isLower(i: Int) = bnds(i)._2
    def isUpper(i: Int) = 1 - bnds(i)._2

    //sort the bounds
    scala.util.Sorting.quickSort(bnds)(Ordering.by[(Int, Int), Int](_._1))

    //compute |I| in O(n)
    val Isize = (1 until 2*n).count(i => bound(i) != bound(i-1))
    val bndSize = Isize + 1;

    //compute bounds counters
    val bndCptMin = Array.fill(bndSize)(0) //for each bound value, number of times it is a lower bound
    val bndCptMax = Array.fill(bndSize)(0) //for each bound value, number of times it is an upper bound
    val bounds = Array.fill(bndSize)(0)

    var k = 0
      bndCptMin(0) += isLower(0)
    bndCptMax(0) += isUpper(0)
    bounds(0) = bound(0)
    for(i <- 1 until 2*n){
      if (bound(i) != bound(i-1)) {
        k += 1
      }
      bounds(k) = bound(i)
      bndCptMin(k) +=  isLower(i) // +1 iff lower bound at i
      bndCptMax(k) += isUpper(i)  // +1 iff upper bound at i
    }

    //compute l(I), r(I) and m(I) = n - r(I) - l(I) for every I
    val Imin = Array.tabulate(Isize)(i => bounds(i))
    val Imax = Array.tabulate(Isize)(i => bounds(i+1))


    val l = Array.fill(Isize)(0)
    val r = Array.fill(Isize)(0)
    val m = Array.fill(Isize)(0)

    var lc = bndCptMax(0) // left count
    var rc = n-bndCptMin(0) // right count

    for (i <- 1 until bndSize) {
      l(i-1) = lc
      r(i-1) = rc
      m(i-1) = n-lc-rc
      lc += bndCptMax(i)
      rc -= bndCptMin(i)
    }

    //compute es and es2 in O(n) with a sweep like algo
    val es = Array.fill(Isize)(0)
    val es2 = Array.fill(Isize)(0)
    for (i <- 0 until n) {
        val xm = if (xmin(i) >= Imax(0)) xmin(i) else 0
      val xM = if (xmax(i) <= Imin(0)) xmax(i) else 0
      es(0) += xm + xM
      es2(0) += xm*xm + xM*xM
    }
    for (k <- 1 until Isize) {
      val pk = l(k) - l(k-1)
      val qk = r(k-1) - r(k)
      es(k) = es(k-1) + (pk-qk) * Imax(k-1)
      es2(k) = es2(k-1) + (pk-qk) * Imax(k-1) * Imax(k-1)
    }

    var Iopt = -1
    for (i <- 0 until Isize) {
      val minSi = es(i)+m(i)*Imin(i)
      val maxSi = es(i)+m(i)*Imax(i)
      if (sum >= minSi && sum <= maxSi) {
        Iopt = i
      }
    }

    // --- step2: compute minimal spread in O(n) and filter es2 ---

    var minopt =
       if (m(Iopt) == 0) {
         es2(Iopt)
       } else {
           val offset = (sum - es(Iopt)).abs / m(Iopt) + 2
           val y = (sum - es(Iopt) + offset*m(Iopt)) % m(Iopt)
         val vinf:  Int = ((sum - es(Iopt)) - y) / m(Iopt)
         val vsup = vinf + (if (y > 0) 1 else 0)
         (es2(Iopt) + y * (vsup * vsup) + (m(Iopt) - y) * (vinf * vinf))
       }

    val v: Double = (sum - es(Iopt)).toDouble / m(Iopt)
    //println("v:"+v)
    //println("Imin:"+Imin.mkString(","))
    //println("Imax:"+Imax.mkString(","))
    //println("Iopt:"+Iopt)
    //println("es:"+es(Iopt))
    //minopt = scala.math.ceil(es2(Iopt) + m(Iopt) * v * v).toInt

    sum2.updateMin(minopt)


    // --- step3: filtering of x(i)'s max ---

    // return true if and only if Dom(x(i)) subsumes interval I
    def inDomain(I: Int, i: Int): Boolean = xmin(i) <= Imin(I) && xmax(i) >= Imax(I)

    // assuming x(i) now is assigned to xi compute the updated values (m*, es*, es2*)
    def updateValues(i: Int, xi: Double, I: Int): (Int,Double,Double) = {
        val d: Double = xi - xmin(i)
        val mStar: Int = if (inDomain(I,i)) m(I)-1 else m(I)
        val esStar: Double = if (inDomain(I,i)) es(I)+xi else es(I)+d;
        val es2Star: Double = if (inDomain(I,i)) es2(I)+xi*xi else es2(I)+d*d+2*d*xmin(i)
        (mStar,esStar,es2Star)
    }

      def pruneMax(i: Int, xiq: Double, I: Int): Unit = {
        if (I >= 0 && xiq < xmax(i)) {
          val (mStar,esStar,es2Star) = updateValues(i,xiq,I)
          val isStar = esStar + mStar * Imin(I)
          if (mStar > 0) {
              val vStar = (sum - esStar) / mStar
                  val deltaQ = (es2Star + mStar * vStar * vStar)
                  val d1 = sum - isStar
                  val a= (1.0 + 1.0 / mStar)
                  val b= (xiq - vStar)
                  val c = deltaQ - sum2.max
                  val d2 = (-b + scala.math.sqrt(b*b-a*c))/a
                  if(d2 <= d1) {
                    var xiZ = scala.math.floor(xiq+d2+epsilon).toInt

                    // Z-bound consistent pruning

                    val (mStar,esStar,es2Star) = updateValues(i,xiZ,I)

                    //println("mStar:"+mStar+" eStar:"+esStar+" es2Star:"+es2Star)

                    val offset = (sum-esStar).toInt.abs / mStar +2
                    val y = (sum-esStar + mStar * offset) % mStar
                    //println("y:"+y+ " sum-esStar:"+(sum-esStar)+" mod:"+(sum-esStar + mStar * offset)+ " offset:"+offset)
                    val vinf = ((sum-esStar) - y) / mStar
                    val vsup = if (y > 0) vinf+1 else vinf
                    //println("vinf:"+vinf+" vsup:"+vsup)
                    var deltaZ = es2Star + y * (vsup*vsup) + (mStar-y) * (vinf*vinf)

                    while (deltaZ > sum2.max) {
                      deltaZ += 2 * (vsup-xiZ);
                      xiZ -= 1
                    }
                    //println("deltaZ:"+deltaZ+" xiZ:"+xiZ)
                    x(i).updateMax(xiZ)
                  }
                  else pruneMax(i,xiq+ d1, I-1)
          } else pruneMax(i,xiq, I-1)
        }
    }


    for (i <- 0 until n; if xmax(i) > Imin(Iopt)) {
      pruneMax(i,if (inDomain(Iopt,i)) v else xmin(i), Iopt)
    }
  }

}


