package oscar.algo.testUtils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertions

/** @author Renaud Hartert ren.hartert@gmail.com */
abstract class TestSuite extends AnyFunSuite with Matchers with Assertions {
  
  /** Perform the test only if the condition is true. */
  protected def test(testCondition: Boolean, testName: String)(testFun: => Unit): Unit = {
    if (testCondition) test(testName)(testFun)
  }
}