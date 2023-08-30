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
package oscar.cp.examples.hakank

import oscar.cp._
/**
  Discrete Tomography problem in Oscar.
  Problem from http://eclipse.crosscoreop.com/examples/tomo.ecl.txt
  """
  This is a little "tomography" problem, taken from an old issue
  of Scientific American.
  A matrix which contains zeroes and ones gets "x-rayed" vertically and
  horizontally, giving the total number of ones in each row and column.
  The problem is to reconstruct the contents of the matrix from this
  information. Sample run:
  ?- go.
     0 0 7 1 6 3 4 5 2 7 0 0
  0                         
  0                         
  8      * * * * * * * *    
  2      *             *    
  6      *   * * * *   *    
  4      *   *     *   *    
  5      *   *   * *   *    
  3      *   *         *    
  7      *   * * * * * *    
  0                         
  0                         
  Eclipse solution by Joachim Schimpf, IC-Parc
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 */
object Tomography extends CPModel with App  {
    // data
    // 
    //
    // The following three examples are from the ECLiPSe program cited above.
    // 
    val p1 = Array(Array(0,0,8,2,6,4,5,3,7,0,0),
                   Array(0,0,7,1,6,3,4,5,2,7,0,0))
    val p2 = Array(Array(10,4,8,5,6),
                   Array(5,3,4,0,5,0,5,2,2,0,1,5,1))
    // This give three slightly different solutions.
    val p3 = Array(Array(11,5,4),
                   Array(3,2,3,1,1,1,1,2,3,2,1))
    // This is my (hakank's) own problem.
    val p4 = Array(Array(0,2,2,2,2,2,8,8,4,4,4,4,4,0),
                   Array(0,0,0,12,12,2,2,2,2,7,7,0,0,0))
    val problems = Array(p1,p2,p3,p4)
    val p = if (args.length > 0) args(0).toInt else 3;
    val problem = problems(p)
    val row_sums = problem(0)
    val col_sums = problem(1)
    val r = row_sums.length
    val c = col_sums.length
    //
    // decicion variables
    //
    val x = Array.fill(r,c)(CPIntVar(0 to 1))
    val x_t = x.transpose
    //
    // constraints
    //
    var numSols = 0
  
      for(i <- 0 until r) {
       add(sum( x(i) ) === row_sums(i))
      }
      for(j <- 0 until c) {
       add(sum( x_t(j) ) === col_sums(j))
      }
     search{
       binaryFirstFail(x.flatten.toSeq)
     }
onSolution {
       println("\nSolution:")
       print("     ")
       for(j <- 0 until c) {
         print("%2d".format(col_sums(j)) )
       }
       println()
       for(i <- 0 until r) {
         print(" " + "%2d".format(row_sums(i)) + "   ")
           for(j <- 0 until c) {
             print(if (x(i)(j).value == 1) "X " else "  ")
           }
           println()
       }
       println()
       numSols += 1
     } 
     println(start())
  }
