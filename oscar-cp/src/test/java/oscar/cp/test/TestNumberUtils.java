/*******************************************************************************
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
 ******************************************************************************/
package oscar.cp.test;


import junit.framework.TestCase;
import oscar.cp.util.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestNumberUtils extends TestCase {



    public TestNumberUtils(String name) {
        super(name);
        
    }
    	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();

    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();

    }
    
    private boolean isPerfectSquare(int v) {
    	int r = (int) Math.sqrt(v);
    	return r*r == v;
    }
    
    public void testPerfectSquare() {
    	assertTrue(NumberUtils.isPerfectSquare(8*8));
    	assertFalse(NumberUtils.isPerfectSquare(8*9));
    }
    
    public void testCeilDiv() {
    	assertEquals(NumberUtils.ceilDiv(7, 2),4);
    	assertEquals(NumberUtils.ceilDiv(-7, -2),4);
    	assertEquals(NumberUtils.ceilDiv(-7, 2),-3);
    	assertEquals(NumberUtils.ceilDiv(7, -2),-3);
    }
    
    public void testFloorDiv() {    	
    	assertEquals(NumberUtils.floorDiv(385810, 100000),3);
    	assertEquals(NumberUtils.floorDiv(495700, 100000),4);
    	assertEquals(NumberUtils.floorDiv(-5, 2),-3);
    	
    }
    
    

    
    
}
