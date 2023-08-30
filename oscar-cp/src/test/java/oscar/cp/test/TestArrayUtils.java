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

import oscar.cp.util.ArrayUtils;

import junit.framework.TestCase;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestArrayUtils extends TestCase {



    public TestArrayUtils(String name) {
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
    
    public void testSort() {
    	Integer [] vals1 = new Integer[]{1,3,6,1,3};
        int [] vals2 = new int[]{1,3,6,1,3};
        ArrayUtils.sort(vals1,vals2);
        for (int i = 0; i < vals2.length-1; i++) {
            assertTrue( vals1[i] <= vals1[i+1]);
        }
    }
    /*
    public void testGetRandomVar1() {
    	CPStore cp = new CPStore();
    	int [] freq = new int[5]; 
        CPIntVar [] x = CPIntVar[]{CPIntVar.apply(cp,0,1,2,3),CPIntVar.apply(cp,2),CPIntVar.apply(cp,0,1,2,3),CPIntVar.apply(cp,3),CPIntVar.apply(cp,1,3,9)};
        for (int i = 0; i < 600; i++) {
        	freq[ArrayUtils.getRandomNotBound(x)]++;
        }
        assertTrue(freq[0] > 100);
        assertTrue(freq[1] == 0);
        assertTrue(freq[2] > 100);
        assertTrue(freq[3] == 0);
        assertTrue(freq[4] > 100);      
    }
    
    public void testGetRandomVar2() {
    	CPStore cp = new CPStore();
    	int [] freq = new int[5]; 
        CPIntVar [] x = CPIntVar[]{CPIntVar.apply(cp,3),CPIntVar.apply(cp,2),CPIntVar.apply(cp,2),CPIntVar.apply(cp,3),CPIntVar.apply(cp,9)};
        
        assertTrue(ArrayUtils.getRandomNotBound(x) ==  -1);     
    }
    */

}
