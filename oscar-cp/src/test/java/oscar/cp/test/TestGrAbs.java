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
import oscar.cp.constraints.GrEq;
import oscar.cp.core.CPStore;
import oscar.cp.core.variables.CPIntVar;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestGrAbs extends TestCase {
	
	private CPStore s;	
	
    public TestGrAbs(String name) {
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
    
    public void testGrAbs() throws Inconsistency {
    	CPIntVar [] x = new CPIntVar[2];
    	for (int i = 0; i < x.length; i++) {
			x[i] = CPIntVar.apply(s,1,256);
		}
    	CPIntVar tmp = oscar.cp.modeling.constraint.absolute(oscar.cp.modeling.constraint.minus(x[0],x[1]));
    	
    	s.post(new GrEq(tmp,0));
    	
    	
    	
    	s.post(new EqCons(x[0],1));
    	
    	assertTrue(!s.isFailed());


    }

}
