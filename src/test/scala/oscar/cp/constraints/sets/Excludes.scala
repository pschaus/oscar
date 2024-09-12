package oscar.cp.constraints.sets

import oscar.cp.core.variables.{CPBoolVar, CPIntVar, CPSetVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength

/** 
 *  @author Renaud Hartert ren.hartert@gmail.com
 *  @author Pierre Schaus pschaus@gmail.com
 */

class Excludes(val set: CPSetVar, elem: Int) extends Constraint(set.store, "Set excludes") {
  override def associatedVars(): Iterable[CPVar] = Array(set)

  override def setup(l: CPPropagStrength): Unit = set.excludes(elem)
}

class ExcludeElem(set: CPSetVar, elem: Int, b: CPBoolVar) extends Constraint(set.store, "RequiredElem") {

  override def associatedVars(): Iterable[CPVar] = Array(set, b)

  override def setup(l: CPPropagStrength): Unit = {
    propagate()
    if(isActive) {
      set.callValExcludedWhenExcludedValue(this)
      set.callValRequiredWhenRequiredValue(this)
      b.callValBindWhenBind(this)
    }
  }

  override def propagate(): Unit = {
    if (b.isBound) valBind(b)
    else if (set.isRequired(elem)) setFalse()
    else if (!set.isPossible(elem)) setTrue()
  }

  @inline
  private def setTrue(): Unit = {
    b.assign(1)
    deactivate()
  }

  @inline
  private def setFalse(): Unit = {
    b.assign(0)
    deactivate()
  }

  @inline
  private def requires(elem: Int): Unit = {
    set.requires(elem)
    deactivate()
  }
  
  @inline
  private def excludes(elem: Int): Unit = {
    set.excludes(elem)
    deactivate()
  }

  override def valRequired(cpSet: CPSetVar, reqElem: Int): Unit = {
    if (reqElem == elem)
      setFalse()
  }

  override def valExcluded(cpSet: CPSetVar, exElem: Int): Unit = {
    if (exElem == elem)
      setTrue()
  }

  override def valBind(cpVar: CPIntVar): Unit = {
    if (b.isTrue) excludes(elem)
    else requires(elem)
  }
}

object Excludes {
  def apply(set: CPSetVar, elem: Int, reifBool: CPBoolVar): Constraint = new ExcludeElem(set, elem, reifBool)
  def apply(set: CPSetVar, elem: Int): Constraint = new Excludes(set, elem)
}