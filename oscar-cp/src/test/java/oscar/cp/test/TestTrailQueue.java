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
public class TestTrailQueue extends TestCase {
	
	private CPStore s;
	private ReversibleQueue<Integer> a;
	private ReversibleQueue<String>  b;
	

    public TestTrailQueue(String name) {
        super(name);
    }
    
	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();
        s = new CPStore();
        a = new ReversibleQueue<Integer>(s);
        b = new ReversibleQueue<String>(s);
    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        s = null;
        a = null;
        b = null;
    }
    
    public void testEquals(){
    	
    	
    	assertTrue(a.getValue()==null);
    	assertTrue(b.getValue()==null);
    	
    	//a = null, b = null
    	s.pushState();
    	a.setValue(new Queue<Integer>(a.getValue(),1));
    	a.setValue(new Queue<Integer>(a.getValue(),2));
    	a.setValue(new Queue<Integer>(a.getValue(),3));
    	b.setValue(new Queue<String>(b.getValue(),"a"));
    	b.setValue(new Queue<String>(b.getValue(),"b"));
    	b.setValue(new Queue<String>(b.getValue(),"c"));
    	
    	    	
    	//a = 3->2->1    b = c->b->a
    	s.pushState();
    	b.setValue(new Queue<String>(b.getValue(),"d"));
    	  	
    	//a = 3->2->1    b = d->c->b->a
    	s.pushState();
    	a.setValue(new Queue<Integer>(a.getValue(),4));
    	a.setValue(new Queue<Integer>(a.getValue(),5));
    	
    	//a = 5->4->3->2->1    b= d->c->b->a
    	s.pushState();
    	  	
    	s.pop();
    	assertTrue(a.getValue().toString().equals("5->4->3->2->1"));
    	assertTrue(b.getValue().toString().equals("d->c->b->a"));
    	
 	
    	s.pop();  
    	assertTrue(a.getValue().toString().equals("3->2->1"));
    	assertTrue(b.getValue().toString().equals("d->c->b->a"));
    	
    	s.pop();  
    	assertTrue(a.getValue().toString().equals("3->2->1"));
    	assertTrue(b.getValue().toString().equals("c->b->a"));
    	
    	s.pop();  
    	assertTrue(a.getValue()==null);
    	assertTrue(b.getValue()==null); 	
    	
    }


}
