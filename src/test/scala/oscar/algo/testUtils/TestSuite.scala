package oscar.algo.testUtils

cp.src.test.scala.oscar.algo.testUtils

/** @author Renaud Hartert ren.hartert@gmail.com */
abstract class TestSuite extends FunSuite with Matchers with Assertions {
  
  /** Perform the test only if the condition is true. */
  protected def test(testCondition: Boolean, testName: String)(testFun: => Unit): Unit = {
    if (testCondition) test(testName)(testFun)
  }
}