
package oscar.cp.constraints.tables

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversibleSparseSetManual
import oscar.algo.array.ArraySet

import scala.collection.mutable.ArrayBuffer
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore
import oscar.algo.array.ArrayMap

/**
 * Implementation of the STR3 algorithm for the table constraint.
 * @param vars the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 * @author Guillaume Perez memocop@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class TableSTR3(vars: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(vars(0).store, "TableSTR3") {

  override def associatedVars(): Iterable[CPVar] = vars

  private[this] val arity = vars.length
  private[this] val store = vars(0).store

  // Array used to copy variable's domain for fast iterations
  private[this] val tmpArray = new Array[Int](vars.map(_.size).max)

  // Invalid tuples 
  private[this] val invalidTuples = new ReversibleSparseSetManual(store, table.length)
  private[this] val invalidTuplesArray = invalidTuples.exposeArray

  // Subtables and separators
  private[this] val initMins = Array.tabulate(arity)(i => vars(i).min)
  private[this] val subtables = Array.tabulate(arity)(i => new Array[Array[Int]](vars(i).max - initMins(i) + 1))
  private[this] val separators = Array.tabulate(arity)(i => new Array[ReversibleInt](vars(i).max - initMins(i) + 1))
  private[this] val deps: Array[ArrayMap] = Array.fill(table.length)(new ArrayMap(arity, true))

  override def setup(l: CPPropagStrength): Unit = {
    invalidTuples.trail() // save state

    val tempSupport = Array.tabulate(arity)(i => {
      Array.fill(vars(i).max - initMins(i) + 1)(new ArrayBuffer[Int]())
    })

    var t = 0
    while (t < table.length) {
      if (isValidTuple(table(t))) {
        var i = 0
        while (i < arity) {
          val a = table(t)(i)
          tempSupport(i)(a - initMins(i)) += t
          i += 1
        }
      }
      t += 1
    }

    var i = 0
    while (i < arity) {
      var j = vars(i).fillArray(tmpArray)
      while (j > 0) {
        j -= 1
        val value = tmpArray(j)
        val valueId = value - initMins(i)
        if (tempSupport(i)(valueId).isEmpty) {
          vars(i).removeValue(value)
        } else {
          separators(i)(valueId) = new ReversibleInt(store, tempSupport(i)(valueId).size - 1)
          val subtable = tempSupport(i)(valueId).toArray
          subtables(i)(valueId) = subtable
          deps(subtable(0)).add(i, value)
        }
      }
      vars(i).callOnChangesIdx(i, delta => valuesRemoved(delta), true)
      i += 1
    }
  }

  private def valuesRemoved(delta: DeltaIntVar): Boolean = {
    invalidTuples.trail() // save state
    val varId = delta.id
    var i = delta.fillArray(tmpArray)
    while (i > 0) {
      i -= 1
      val value = tmpArray(i)
      valueRemoved(varId, value)
    }
    false
  }

  private def valueRemoved(varId: Int, value: Int): Unit = {

    val membersBefore = invalidTuples.size
    val valueId = value - initMins(varId)
    val subtable = subtables(varId)(valueId)
    val separator = separators(varId)(valueId).value

    // Seek for new invalid tuples
    var supportId = 0
    while (supportId <= separator) {
      val tupleId = subtable(supportId)
      invalidTuples.add(tupleId)
      supportId += 1
    }

    // All tuples remain valid
    val membersAfter = invalidTuples.size
    if (membersAfter == membersBefore) return

    var i = membersBefore
    while (i < membersAfter) {

      val tupleId = invalidTuplesArray(i)
      val dep = deps(tupleId)
      val depVars = dep.keys
      val depVals = dep.values
      
      var j = dep.size
      while (j > 0) {
        j -= 1

        val varId = depVars(j)
        val value = depVals(j)
        val valueId = value - initMins(varId)
        val x = vars(varId)

        if (x.hasValue(value)) {

          val subtable = subtables(varId)(valueId)
          val separatorRev = separators(varId)(valueId)
          val separator = separatorRev.value

          var p = separator
          while (p >= 0 && invalidTuples.contains(subtable(p))) p -= 1

          if (p < 0) {
            x.removeValue(value)
          } else {
            separatorRev.setValue(p)
            deps(tupleId).removeIncluded(varId)
            deps(subtable(p)).addExcluded(varId, value)
          }
        }
      }
      i += 1
    }
  }

  // Return true if the tuple is valid
  @inline private def isValidTuple(tuple: Array[Int]): Boolean = {
    var i = arity
    while (i > 0) {
      i -= 1
      if (!vars(i).hasValue(tuple(i))) return false
    }
    true
  }
}