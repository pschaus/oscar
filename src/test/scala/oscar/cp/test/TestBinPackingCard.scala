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

import oscar.cp.constraints._
import oscar.cp._
import oscar.cp.core.NoSolutionException
import oscar.cp.testUtils._

import scala.collection.mutable.ArrayBuffer


class TestBinPackingCard extends TestSuite {


  test("BP 1") {

    for (s <- 1 until 200) {
      //println("seed:"+s)
      val rand = new scala.util.Random(s)


      //val n = 9
      //val m = 4


      val n = 8
      val m = 3



      val w_ = Array.fill(n)(rand.nextInt(20))
      val w = w_.sortBy(-_)
      val capa = w.sum / m + 4

      val avgLoad = w.sum / m * capa

      implicit val cp = CPSolver()
      val x = Array.fill(n)(CPIntVar(0 until m))
      val l = Array.fill(m)(CPIntVar(((capa - 10) max 0) until capa))
      val c = Array.fill(m)(CPIntVar(2 until n))

      try {
        add(binPacking(x, w, l))




        search {
          binaryStatic(x)
        }


        val stat1 = cp.startSubjectTo() {
          add(gcc(x, (0 until m) zip c),Strong)
        }

        val stat2 = cp.startSubjectTo() {
          add(new BinPackingFlow(x, w, l, c))
          add(gcc(x, (0 until m) zip c),Strong)
        }

        val stat3 = cp.startSubjectTo() {
          add(new BinPackingFlowExtended(x, w, l, c))
          add(gcc(x, (0 until m) zip c),Strong)
        }

        val stat4 = cp.startSubjectTo() {
          add(new GCCBinPacking(x, w, l, c))
        }


        //println(stat1.nSols + "=?=" + stat2.nSols + "=?=" + stat3.nSols)
        //println(stat1.nSols + "=?=" + stat2.nSols + "=?=" +stat3.nSols + "=?=" + stat4.nSols)
        assert(stat1.nSols == stat2.nSols)
        assert(stat1.nSols == stat3.nSols)
        assert(stat1.nSols == stat4.nSols)
        //println("bk:" + stat1.nFails + "<?=" + stat2.nFails + "<?=" +stat3.nFails + "<?=" + stat4.nFails)
        assert(stat4.nFails <= stat3.nFails)


      }

      catch {
        case e: NoSolutionException => //println("no sol")
      }
    }

  }


  test("BP 2") {
    val cp = new CPSolver()
    val sizes = Array(3,3,3,1,1,1)
    val c = Array(CPIntVar(1 to 6)(cp),CPIntVar(1 to 6)(cp),CPIntVar(0 to 6)(cp),CPIntVar(0 to 6)(cp))

    val l = Array(CPIntVar(0 to 20)(cp),CPIntVar(0 to 20)(cp),CPIntVar(6 to 20)(cp),CPIntVar(0 to 20)(cp))

    val x = Array(CPIntVar(0 to 2)(cp),CPIntVar(0 to 2)(cp),CPIntVar(0 to 2)(cp),CPIntVar(2 to 3)(cp),CPIntVar(2 to 3)(cp),CPIntVar(2 to 3)(cp))

    //[1, 2],[1, 2],[0, 4],[0, 3]

    //[1, 2],[1, 3],[2, 4],[0, 2]
    //cp.add(new GCCBinPacking(x,sizes,l,c))

    //cp.add(new BinPackingFlowExtended(x,sizes,l,c))
    //cp.add(gcc(x,(0 to 4).zip(c)))

    //println(c.mkString(","))
    //println(l.mkString(","))
  }

  test("BP 4") {
    def readDom(s: String) = {
      if (s.trim().contains(",")) {
        if (s.startsWith("[")) {
          val vals = s.trim.take(s.size-1).drop(1).split(",")
          (vals(0).trim.toInt to vals(1).trim.toInt).toSet
        } else {
          s.trim.take(s.size-1).drop(1).split(",").map(_.trim.toInt).toSet
        }

      } else {
        Set(s.trim.toInt)
      }
    }

    val cp = CPSolver()
    val xs = "6* 1* 5* 2* 5* 3* 6* 11* 11* 10* 3* 10* {8, 9}* {9, 11}* {8, 9, 11}* {8, 9}* 7* 8* 1* {8, 9, 11}* 0* 0* 10* 7* 9* 2* 6* 5* 4* 1* 2* 1* 4* 0* 4* 4* 6* 5* {7, 8, 9, 11}* {7, 8, 9, 11}* 3* 8* 4* 4* 7* 2* 8* 7* 3* 7* 3* 10* 2* 2* 0* 1* 5* 0* 0* 11* 3* 9* 8* {6, 7, 8, 9, 10, 11}* {6, 7, 8, 9, 10, 11}* {6, 7, 8, 9, 10}"
    val w = Array(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    val cs = "6* 5* 6* 6* 6* 5* 5* 6* 6* 5* 5* 5"
    val ls = "14* 18* 17* 18* 17* 18* 18* 18* 18* 18* 18* 18"



    val x = xs.split("\\*").map(_.trim).map(s => CPIntVar(readDom(s))(cp))
    val c = cs.split("\\*").map(_.trim).map(s => CPIntVar(readDom(s))(cp))
    val l = ls.split("\\*").map(_.trim).map(s => CPIntVar(readDom(s))(cp))


    //println("packed:" + (0 until x.size).filter(i => x(i).isBoundTo(10)).map(w(_)).sum)
    //println("#packed:" + (0 until x.size).filter(i => x(i).isBoundTo(10)).size)
    var ok = false
    try {
      cp.add(new GCCBinPacking(x,w,l,c))
    } catch {
      case e: NoSolutionException => ok = true
    }
    assert(ok)


  }


  test("BP 5") {

   implicit val cp = CPSolver()
    val w = Array(18,16,12,11,11,10,4,1)

    //before:(2,18),(1,16),([0, 2],12),({0, 1, 2},11),({0, 1, 2},11),({0, 1, 2},10),({0, 1},4),({0, 1, 2},1) loads:[23, 28],[26, 30],[28, 30] card:[2, 4],[2, 3],[2, 3]

    val x = Array(CPIntVar(2),CPIntVar(1),CPIntVar(Set(1,2)),CPIntVar(Set(0,1,2)),CPIntVar(Set(0,1,2)),CPIntVar(Set(0,1,2)),CPIntVar(Set(0,1)),CPIntVar(Set(0,1,2)))
    val l = Array(CPIntVar(23 to 27),CPIntVar(26 to 30),CPIntVar(28 to 30))
    val c = Array(CPIntVar(3 to 4),CPIntVar(2 to 3),CPIntVar(2,3))


    add(binPacking(x, w, l))
    cp.add(new GCCBinPacking(x,w,l,c))

    add(x(2) !== 0)
    assert(c(2).max == 3)





  }

}

