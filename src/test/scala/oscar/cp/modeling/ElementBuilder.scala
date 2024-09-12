package oscar.cp.modeling

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.CPPropagStrength.Weak
import oscar.cp.constraints.ElementCst
import oscar.cp.constraints.ElementVar
import oscar.cp.constraints.ElementCst2D

/**
 *  Simple and natural modeling functionnalities for element constraints.
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */

trait ElementBuilder {

  import ElementBuilder._

  /** Element on an array of Int. */
  implicit def arrayIntToElement(array: Array[Int]) = new ArrayIntElementConstraintBuilder(array)

  /** Element on an array of CPIntVar. */
  implicit def arrayIntVarToElement(array: Array[CPIntVar]) = new ArrayCPIntVarElementConstraintBuilder(array)

  /** Element on an array of CPBoolVar. */
  implicit def arrayBooleanVarToElement(array: Array[CPBoolVar]) = new ArrayCPBoolVarElementConstraintBuilder(array)

  /** Element on an indexed sequence of Int. */
  implicit def seqIntToElement(seq: IndexedSeq[Int]) = new ArrayIntElementConstraintBuilder(seq.toArray)

  /** Element on an indexed sequence of CPIntVar. */
  implicit def seqIntVarToElement(seq: IndexedSeq[CPIntVar]) = new ArrayCPIntVarElementConstraintBuilder(seq.toArray)

  /** Element on an indexed sequence of CPBoolVar. */
  implicit def seqBooleanVarToElement(seq: IndexedSeq[CPBoolVar]) = new ArrayCPBoolVarElementConstraintBuilder(seq.toArray)

  /** Element on an array of array of Int. */
  implicit def matricIntToElement(matrix: Array[Array[Int]]) = new ElementIntMatrixConstraintBuilderLine(matrix)
}

object ElementBuilder {

  /** Element on an array of Int. */
  class ArrayIntElementConstraintBuilder(val array: Array[Int]) extends AnyVal {
    def apply(id: CPIntVar): CPIntVar = {
      val minval = array.min
      val maxval = array.max
      val z = CPIntVar(minval, maxval)(id.store)
      id.store.post(new ElementCst(array, id, z), Weak)
      z
    }
  }

  /** Element on an array of CPIntVar. */
  class ArrayCPIntVarElementConstraintBuilder(val array: Array[CPIntVar]) extends AnyVal {
    def apply(id: CPIntVar): CPIntVar = {
      val minValue = array.map(_.min).min // FIXME performance issue
      val maxValue = array.map(_.max).max // FIXME performance issue
      val z = CPIntVar(minValue, maxValue)(id.store)
      id.store.post(new ElementVar(array, id, z), Weak)
      z
    }
  }

  /** Element on an array of CPBoolVar. */
  class ArrayCPBoolVarElementConstraintBuilder(val array: Array[CPBoolVar]) extends AnyVal {
    def apply(id: CPIntVar): CPIntVar = {
      val minValue = array.map(_.min).min // FIXME performance issue
      val maxValue = array.map(_.max).max // FIXME performance issue
      val z = CPIntVar(minValue, maxValue)(id.store)
      val intVarArray = array.asInstanceOf[Array[CPIntVar]]
      id.store.post(new ElementVar(intVarArray, id, z), Weak)
      z
    }
  }

  /** Element on an array of array of Int. */
  class ElementIntMatrixConstraintBuilderLine(val a: Array[Array[Int]]) extends AnyVal {
    def apply(i: CPIntVar) = new ElementIntMatrixConstraintBuilderCol(i, a)
  }

  /** Element on an array of array of Int. */
  class ElementIntMatrixConstraintBuilderCol(i: CPIntVar, matrix: Array[Array[Int]]) {
    def apply(j: CPIntVar): CPIntVar = { 
      val minVal = matrix.flatten.min // FIXME performance issue
      val maxVal = matrix.flatten.max // FIXME performance issue
      val z = CPIntVar(minVal, maxVal)(i.store)
      i.store.post(new ElementCst2D(matrix, i, j, z))
      z
    }
  }
}