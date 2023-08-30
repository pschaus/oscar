package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, binaryFirstFail}
import oscar.cp.constraints.diagrams._
import oscar.cp.constraints.diagrams.diagramrepresentation.GroundDiagram

object DiagramExample extends CPModel with App {

  val vars = Array.fill(3)(CPIntVar(0 until 3))

  val diag = Array(
    Array((0,0,0),(0,1,1)),
    Array((0,0,0),(1,1,1)),
    Array((0,0,0),(1,1,0))
  )

  solver.post(diagram(vars,new GroundDiagram(diag, Array(1,2,2,1)),DiagramAlgo.CompactDiagram))

  solver.search(binaryFirstFail(vars))
  solver.onSolution{
    println(vars.map(_.value).mkString(","))
  }

  print(solver.start())
}
