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
package oscar.cp.multiobjective


class ListPareto[Sol](objMax: Array[Boolean]) extends Pareto[Sol](objMax) {
  
  private var sols: List[ParetoSol] = List()

  var nadir: Array[Int] = Array.fill(nObjs)(Int.MaxValue) 
  var ideal: Array[Int] = Array.fill(nObjs)(Int.MinValue)

  def insert(sol: Sol, objValues: IndexedSeq[Int]): Boolean = { 
    
    val newSol = ParetoSol(objValues, sol)
    
    val inserted = if (sols.exists(s => dominate(s, newSol))) false
    else {     
      var newList: List[ParetoSol] = List()
      for (s <- sols if !dominate(newSol, s)) newList = s::newList 
      val nRemoved = sols.size - newList.size
      sols = newSol :: newList
      true
    }
    notifyObservers() // TODO: Only if inserted ?
    inserted
  }
  
  def getDominant(objValues: Array[Int]): Option[Sol] = {
    for (s <- sols if dominate(s.objValues, objValues)) return Some(s.sol)
    None
  }
  
  def objectiveSols: List[IndexedSeq[Int]] = for(s <- sols) yield s.objValues

  def size: Int = sols.size

  def foreach[B](f: (Sol) => B): Unit = sols.foreach(s => f(s.sol))

  def removeAll(): Unit = { sols = List() }

  def sortByObj(obj: Int): List[Sol] = sols.sortBy(_.objValues(obj)).map(_.sol)
}

object ListPareto {
  def apply[Sol](nObjs: Int, maximization: Boolean = false) = new ListPareto[Sol](Array.fill(nObjs)(maximization))
  def apply[Sol](maximizations: Boolean*) = new ListPareto[Sol](maximizations.toArray)
}
