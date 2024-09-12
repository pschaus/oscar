package oscar.cp.examples

import oscar.cp._
import oscar.cp.examples.Cubes.{letters, placement}

object Debug extends CPModel with App {

  val vars = Array.fill(4)(CPIntVar(Set(-1,4,5)))

  var y = CPIntVar(3 to 5)

  add(atLeastNValue(vars,y), Strong)

  var nSol = 0

  onSolution {
    println(vars.map(_.value).mkString(","))
    nSol += 1
  }
  search { binaryStatic(vars) }

  val stats = start()
  println("nSols:"+nSol)
  println(stats)

}
