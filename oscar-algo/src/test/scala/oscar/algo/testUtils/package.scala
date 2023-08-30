package oscar.algo

/**
 * The `testUtils` package provides useful functionnalities to test variables 
 * and constraints of the OscaR Constraint Programming Library.
 * 
 * @author Renaud Hartert ren.hartert@gmail.com
 */
package object testUtils {

  @inline private def assert(assertion: Boolean, message: => Any): Unit = {
    if (!assertion) throw new java.lang.AssertionError("assertion failed: " + message)
  }

  @inline private def assert(assertion: Boolean): Unit = {
    if (!assertion) throw new java.lang.AssertionError("assertion failed.")
  }


}