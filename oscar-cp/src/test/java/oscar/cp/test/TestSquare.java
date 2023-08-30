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
import oscar.algo.Inconsistency;
import oscar.cp.constraints.Square;
import oscar.cp.core.CPStore;
import oscar.cp.core.variables.CPIntVar;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestSquare extends TestCase {
	
    public TestSquare(String name) {
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
    

    
    public void testSquare1()  throws Inconsistency {
    	CPStore s = new CPStore();
    	CPIntVar x = CPIntVar.apply(s,-5,5);
    	CPIntVar y = CPIntVar.apply(s,-5,16);
    	s.post(new Square(x, y));
    	assertTrue(!s.isFailed());
    	assertTrue(x.getMin() == -4);
    	assertTrue(x.getMax() == 4);
    	assertTrue(y.getMax() == 16);
    	assertTrue(y.getMin() == 0);
    }

}
