package oscar.cp.examples

import java.awt.Color

import oscar.cp.{CPIntVar, CPModel, add, binPacking, binaryFirstFail, minimize, onSolution, start, sum}

import scala.io.Source

/**
 * P-Median Problem
 *
 * Let us consider a set I={1,..., n} of potential locations for p facilities,
 * a set J={1,..., m} of customers, and  n x m matrix of transportations costs
 * for satisfying the demands of the customers from the facilities.
 * The p-median problem is to locate the p facilities at locations of I in order
 * to minimize the total transportation cost for satisfying the demand of the customers.
 * Also each location has fixed capacity for the demand that cannot be exceeded.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object PMedian extends CPModel with App {

  val lines = Source.fromFile("data/pmed.txt").getLines.reduceLeft(_ + " " + _)

  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }

  val nbCust = next()
  val nbMed = next()
  val capa = next()
  val cust =
    for (i <- 0 until nbCust) yield {
      next()
      (next(), next(), next())
    }
  val dist = Array.tabulate(nbCust, nbCust) { (i, j) =>
    val xdist = (cust(i)._1 - cust(j)._1)
    val ydist = (cust(i)._2 - cust(j)._2)
    Math.sqrt(xdist * xdist + ydist * ydist).toInt
  }
  val demand = for (i <- 0 until nbCust) yield cust(i)._3

  val cost = Array.tabulate(nbCust, nbCust)((i, j) => dist(i)(j))

  val x = Array.fill(nbCust)(CPIntVar(0 until nbCust))
  val xsol = Array.fill(nbCust)(0)
  val load = Array.fill(nbCust)(CPIntVar(0 until capa))



  val rnd = new scala.util.Random(0)

  val costs = Array.tabulate(nbCust)(i => cost(i)(x(i)))
  val totCost = sum(costs)

  onSolution {
    for (i <- 0 until nbCust) xsol(i) = x(i).value
    println("\n" + totCost)
  }

  add(binPacking(x, demand, load))
  add(sum(0 until nbCust)(i => load(i) ?> 0) <= nbMed)

  minimize(totCost) search {
    binaryFirstFail(x, _.randomValue)
  }

  val stats = start()
  println(stats)

}
