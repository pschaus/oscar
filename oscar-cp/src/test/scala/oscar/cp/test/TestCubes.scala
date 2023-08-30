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
import oscar.cp.testUtils._
import oscar.cp.constraints._
import oscar.cp._
import collection.immutable.SortedSet
import oscar.cp.core.CPPropagStrength


class TestCubes extends TestSuite  {


  test("Cubes") {
    def nSol(cons: CPPropagStrength): Int = {
      implicit val cp = CPSolver()

      val numCubes = 4
      val numFaces = 6

      val words = "BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, JUNK, LIMN, QUIP, SWAG, VISA, WISH".split(", ")
      val letters = words.foldLeft(SortedSet.empty[Char]) { _ ++ _ }.toSeq // Set of all 24 letters
      val numLetters = letters.size
      def letterToInt(letter: Char): Int = letters.indexOf(letter) // Letter from letter index

      val placement = for (i <- 0 until numLetters) yield CPIntVar(0 until numCubes)(cp) // The cube (0 to 3) on which each letter is placed
      var nbSol = 0

      cp.add(gcc(placement, 0 until numCubes, numFaces, numFaces), cons) // There must be exactly 6 letters on each cube
      for (word <- words)
        cp.add(allDifferent( // The 4 letters of each word must be placed on different cubes
          for (letter <- word.toCharArray()) yield placement(letterToInt(letter))), cons)

      search { // Each letter will be assigned different cubes during the search
        binaryFirstFail(placement)
      }
      cp.start().nSols
    }
    for (cons <- CPPropagStrength.values) {     
      assert(nSol(cons) == 24)
    }
    
  }  
  

  


}
