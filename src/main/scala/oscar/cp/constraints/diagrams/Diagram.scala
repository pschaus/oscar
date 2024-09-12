/** *****************************************************************************
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
 * **************************************************************************** */

package oscar.cp.constraints.diagrams

import oscar.cp.constraints.diagrams.diagramrepresentation.{BasicSmartDiagram, GroundDiagram}
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar

/**
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */

/**
 * All the diagram constraint algorithms available
 */
object DiagramAlgo extends Enumeration {
  type DiagramAlgo = Value
  val CompactDiagram = Value("CompactDiagram (Verhaeghe et al)")
}

object diagram {

  def apply(X: Array[CPIntVar], diagram: GroundDiagram, algo: DiagramAlgo.Value = DiagramAlgo.CompactDiagram): Constraint = {
    import oscar.cp.constraints.diagrams.DiagramAlgo._

    algo match {
      case CompactDiagram => compactDiagram(X, diagram)
      case _ => compactDiagram(X, diagram)
    }
  }

  def compactDiagram(X: Array[CPIntVar], diagram: GroundDiagram): Constraint = DiagramCompactDiagram(X, diagram)


}

object BasicSmartDiagramAlgo extends Enumeration {
  type BasicSmartDiagramAlgo = Value
  val CompactDiagramBs = Value("CompactDiagram for basic smart diagram (Verhaeghe et al)")
}

object basicsmartdiagram {

  def apply(X: Array[CPIntVar], diagram: BasicSmartDiagram, algo: BasicSmartDiagramAlgo.Value = BasicSmartDiagramAlgo.CompactDiagramBs): Constraint = {
    import oscar.cp.constraints.diagrams.BasicSmartDiagramAlgo._

    algo match {
      case CompactDiagramBs => compactDiagram(X, diagram)
      case _ => compactDiagram(X, diagram)
    }
  }

  def compactDiagram(X: Array[CPIntVar], diagram: BasicSmartDiagram): Constraint = DiagramCompactDiagram(X, diagram)


}