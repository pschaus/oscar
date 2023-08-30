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
import oscar.algo.reversible.*;
import oscar.cp.core.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestTrailBoundedSet extends TestCase {
	
	private CPStore s;
	
	
    /**
     * Constructor for TestTrailInt.
     * @param name
     */
    public TestTrailBoundedSet(String name) {
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
    
    public void testEquals(){
    	
    	//a = null, b = null
    	s.pushState();
    	
    	ReversibleBoundedSet set = new ReversibleBoundedSet(s,10);
    	
    	s.pushState();
    	
    	set.insert(5);
    	set.insert(10);
    	set.insert(3);
    	set.remove(10);
    	
    	s.pushState();
    	
    	set.insert(6);
    	
    	assertTrue(set.getSize() == 3);
    	
    	set.remove(5);
    	
    	s.pop();
    	
    	
    	assertTrue(set.contains(3));
    	assertTrue(set.contains(5));
    	assertTrue(set.getSize()==2);

    }

}
