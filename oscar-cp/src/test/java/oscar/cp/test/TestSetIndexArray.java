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

import java.util.Arrays;

import oscar.algo.reversible.*;
import oscar.cp.core.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestSetIndexArray extends TestCase {

	private CPStore s;


    /**
     * Constructor for TestTrailInt.
     * @param name
     */
    public TestSetIndexArray(String name) {
        super(name);
        
    }
    	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();
        s = new CPStore();
    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        s = null;
        
    }


    
    public void test1(){
    	SparseSet set = new SparseSet(0,10,true);
        assertTrue(set.getSize() == 0);
        for (int v: set) {
            assertTrue(false);
        }
        set.insert(1);
        int [] valArray = set.getSortedVals();
        assertTrue(valArray.length == 1);
        assertTrue(valArray[0] == 1);

        set.insert(2);
        set.insert(4);
        valArray = set.getSortedVals();
        System.out.println(Arrays.toString(valArray));
        assertTrue(valArray[0] == 1);
        assertTrue(valArray[1] == 2);
        assertTrue(valArray[2] == 4);
        assertFalse(set.hasValue(0));
        assertFalse(set.hasValue(3));
        assertFalse(set.hasValue(5));


    }

}
