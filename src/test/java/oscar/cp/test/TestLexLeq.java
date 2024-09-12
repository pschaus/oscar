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
import oscar.cp.constraints.EqCons;
import oscar.cp.constraints.LexLeq;
import oscar.cp.core.CPStore;
import oscar.cp.core.variables.CPIntVar;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestLexLeq extends TestCase {
	
	private CPStore s;	
	
    public TestLexLeq(String name) {
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
    
    public void testLexLeq() throws Inconsistency {
    	CPIntVar [] x = new CPIntVar[5];
    	for (int i = 0; i < x.length; i++) {
			x[i] = CPIntVar.apply(s,0,1);
		}
    	CPIntVar [] y = new CPIntVar[5];
    	for (int i = 0; i < y.length; i++) {
			y[i] = CPIntVar.apply(s,0,1);
		}
    	
    	
    	s.post(new LexLeq(x,y));
    	
    	
    	
    	s.post(new EqCons(y[0],0));  	
    	s.post(new EqCons(y[1],0));
    	s.post(new EqCons(x[2],1));
    	
    	System.out.println(x[0]+" "+x[1]+" "+x[2]);
    	System.out.println(y[0]+" "+y[1]+" "+y[2]);
    	assertTrue(!s.isFailed());
    	assertTrue(x[0].min()==0);
    	assertTrue(x[1].min()==0);
    	assertTrue(y[2].min()==1);
    }
    
    
}
