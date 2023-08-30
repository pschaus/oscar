package oscar.algo.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.algo.RangeMinQuery
import oscar.algo.HeightProfile
import scala.collection.mutable.ArrayBuffer

class TestHeightProfile extends FunSuite with Matchers {


  
  test("test 1") {
    
    
     val a = Array((5,10,6),(11,10,6),(100,2,1),(100,2,1))
     
     val res = HeightProfile.computeProfile(a)
     println(res.mkString(","))
     res should be(Array((5,6,6),(11,4,12),(15,6,6),(21,79,0),(100,2,2)))

  }  

}
