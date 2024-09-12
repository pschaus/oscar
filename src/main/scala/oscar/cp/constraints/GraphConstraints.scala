package oscar.cp.constraints

import oscar.cp.core.variables.{CPGraphVar, CPVar}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Create Graph Basic Constraints to modify domains and call propagate when domain change
 */

class RequiresNode(val G: CPGraphVar, n: Int) extends Constraint(G.s, "Node required") {

  override def associatedVars(): Iterable[CPVar] = Array(G)

  override def setup(l: CPPropagStrength): Unit = {
    G.addNodeToGraph(n)
  }

}

class ExcludesNode(val G: CPGraphVar, n: Int) extends Constraint(G.s, "Node excluded") {

  override def associatedVars(): Iterable[CPVar] = Array(G)

  override def setup(l: CPPropagStrength): Unit = {
    G.removeNodeFromGraph(n)
  }

}

class RequiresEdge(val G: CPGraphVar, src: Int, dest: Int) extends Constraint(G.s, "Edge required") {

  override def associatedVars(): Iterable[CPVar] = Array(G)

  override def setup(l: CPPropagStrength): Unit = {
    G.addEdgeToGraph(src,dest)
  }

}

class ExcludesEdge(val G: CPGraphVar, src: Int, dest: Int) extends Constraint(G.s, "Edge excluded") {

  override def associatedVars(): Iterable[CPVar] = Array(G)

  override def setup(l: CPPropagStrength): Unit = {
    G.removeEdgeFromGraph(src,dest)
  }

}
