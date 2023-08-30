package oscar.algo.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.algo.RangeMinQuery

class TestRangeMinQuery extends FunSuite with Matchers {

  test("test 1") {
    
    val values = Array(2,4,3,1,6,7,8,9,1,7)
    val rmq = new RangeMinQuery(values) 
    rmq(1,3) should be(3)
    rmq(2,5) should be(3)
    rmq(0,values.length-1) should (be(3) or be(8))

  }
  
  test("test 2") {
    
    
    for (i <- 0 until 1000) {
    	val values = Array.fill(50)(scala.util.Random.nextInt(100))
    	val rmq = new RangeMinQuery(values) 
    	val a = scala.util.Random.nextInt(20)
    	val b = (a + scala.util.Random.nextInt(20)).max(49)
    	val min = values.drop(a).take(b-a+1).min
        values(rmq(a,b)) should be(min)
    }
  }  
  
  test("test 3") {
    
    val a = Array(1, 5, -2, 3)
    
    val rmq = new RangeMinQuery(a) 

    rmq(0, 3) should be(2)
  }    


 
}
