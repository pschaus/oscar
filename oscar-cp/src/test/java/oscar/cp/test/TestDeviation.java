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
import oscar.cp.constraints.Deviation;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.CPStore;



/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestDeviation extends TestCase {



    public TestDeviation(String name) {
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



    public void testDeviation1() throws Inconsistency {
        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = CPIntVar.apply(cp,-2,2);
		}
    	CPIntVar nd = CPIntVar.apply(cp,0,0);
    	cp.post(new Deviation(x,0,nd));
        assertTrue(!cp.isFailed());
        for (int i = 0; i < x.length; i++) {
            assertTrue(x[i].isBound() && x[i].min() == 0);
        }
    }

    public void testDeviation2() throws Inconsistency{
        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = CPIntVar.apply(cp,-2,2);
		}
    	CPIntVar nd = CPIntVar.apply(cp,0,6);
    	cp.post(new Deviation(x,1,nd));
        assertTrue(!cp.isFailed());

    }



    public void testDeviation3() throws Inconsistency{

        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[4];
    	x[0] = CPIntVar.apply(cp,3,7);
        x[1] = CPIntVar.apply(cp,0,5);
        x[2] = CPIntVar.apply(cp,5,6);
        x[3] = CPIntVar.apply(cp,5,7);

    	CPIntVar nd = CPIntVar.apply(cp,0,18);
    	cp.post(new Deviation(x,17,nd));
        assertTrue(!cp.isFailed());
        assertEquals(x[0].getMax(),5);

    }

    public void testDeviation4() throws Inconsistency{

        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[4];
    	x[0] = CPIntVar.apply(cp,3,7);
        x[1] = CPIntVar.apply(cp,0,5);
        x[2] = CPIntVar.apply(cp,5,6);
        x[3] = CPIntVar.apply(cp,5,7);

    	CPIntVar nd = CPIntVar.apply(cp,0,12);
    	cp.post(new Deviation(x,17,nd));
        assertTrue(!cp.isFailed());
        assertEquals(x[0].getMax(),4);

    }

    public void testDeviation5()  throws Inconsistency{

        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[4];
    	x[0] = CPIntVar.apply(cp,3,10);
        x[1] = CPIntVar.apply(cp,4,5);
        x[2] = CPIntVar.apply(cp,3,6);
        x[3] = CPIntVar.apply(cp,0,2);

    	CPIntVar nd = CPIntVar.apply(cp,0,45);
    	cp.post(new Deviation(x,17,nd));
        assertTrue(!cp.isFailed());
        assertEquals(x[0].getMin(),4);
        assertEquals(x[0].getMax(),9);

    }

    public void testDeviation6() throws Inconsistency {

        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[4];
    	x[0] = CPIntVar.apply(cp,3,10);
        x[1] = CPIntVar.apply(cp,4,5);
        x[2] = CPIntVar.apply(cp,3,6);
        x[3] = CPIntVar.apply(cp,0,2);

    	CPIntVar nd = CPIntVar.apply(cp,0,22);
    	cp.post(new Deviation(x,17,nd));
        assertTrue(!cp.isFailed());
        assertEquals(x[0].getMin(),4);
        assertEquals(x[0].getMax(),7);

    }

    public void testDeviation7() throws Inconsistency {

        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[4];
    	x[0] = CPIntVar.apply(cp,3,10);
        x[1] = CPIntVar.apply(cp,4,5);
        x[2] = CPIntVar.apply(cp,3,6);
        x[3] = CPIntVar.apply(cp,0,2);

    	CPIntVar nd = CPIntVar.apply(cp,0,30);
    	cp.post(new Deviation(x,17,nd));
        assertTrue(!cp.isFailed());
        assertEquals(x[0].getMin(),4);
        assertEquals(x[0].getMax(),8);

    }

    public void testDeviation8() throws Inconsistency {

        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[4];

        x[0] = CPIntVar.apply(cp,4,5);
        x[1] = CPIntVar.apply(cp,3,6);
        x[2] = CPIntVar.apply(cp,0,2);
        x[3] = CPIntVar.apply(cp,3,10);

    	CPIntVar nd = CPIntVar.apply(cp,0,30);
    	cp.post(new Deviation(x,17,nd));
        assertTrue(!cp.isFailed());
        assertEquals(x[3].getMin(),4);
        assertEquals(x[3].getMax(),8);

    }

    public void testDeviation9() throws Inconsistency {

        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[6];

        x[0] = CPIntVar.apply(cp,11,16);
        x[1] = CPIntVar.apply(cp,9,11);
        x[2] = CPIntVar.apply(cp,12,14);
        x[3] = CPIntVar.apply(cp,13,14);
        x[4] = CPIntVar.apply(cp,10,12);
        x[5] = CPIntVar.apply(cp,12,15);


    	CPIntVar nd = CPIntVar.apply(cp,0,1000);
    	cp.post(new Deviation(x,74,nd));
        assertTrue(!cp.isFailed());
        assertEquals(nd.getMin(),24);
    }


  
    
    public void testDeviation11() throws Inconsistency{
        CPStore cp = new CPStore();
    	CPIntVar [] x = new CPIntVar[8];

        x[0] = CPIntVar.apply(cp,-27,-25);
        x[1] = CPIntVar.apply(cp,-27,-27);
		x[2] = CPIntVar.apply(cp,-27,-25);
        x[3] = CPIntVar.apply(cp,-27,-25);
		x[4] = CPIntVar.apply(cp,-30,-30);
        x[5] = CPIntVar.apply(cp,-27,-25);
		x[6] = CPIntVar.apply(cp,-27,-25);
        x[7] = CPIntVar.apply(cp,-27,-23);


    	CPIntVar nd = CPIntVar.apply(cp,0,75);
    	cp.post(new Deviation(x,-213,nd));
        assertTrue(!cp.isFailed());
        assertEquals(x[7].getMax(),-24);

    }
    
  public void testDeviation12() throws Inconsistency{
	CPStore cp = new CPStore();
	CPIntVar [] x = new CPIntVar[6];

	x[0] = CPIntVar.apply(cp,11,16);
	x[1] = CPIntVar.apply(cp,9,11);
	x[2] = CPIntVar.apply(cp,12,14);
	x[3] = CPIntVar.apply(cp,13,14);
	x[4] = CPIntVar.apply(cp,10,12);
	x[5] = CPIntVar.apply(cp,12,15);



	CPIntVar nd = CPIntVar.apply(cp,0,34);
	cp.post(new Deviation(x,74,nd));
	assertTrue(!cp.isFailed());
    assertEquals(x[0].getMax(),14);

 }
    
  public void testDeviation13() throws Inconsistency{
	CPStore cp = new CPStore();
	CPIntVar [] x = new CPIntVar[6];
	
	x[0] = CPIntVar.apply(cp,-14,-12);
	x[1] = CPIntVar.apply(cp,-11,-9);
	x[2] = CPIntVar.apply(cp,-14,-12);
	x[3] = CPIntVar.apply(cp,-14,-13);
	x[4] = CPIntVar.apply(cp,-12,-10);
	x[5] = CPIntVar.apply(cp,-14,-12);



	CPIntVar nd = CPIntVar.apply(cp,0,34);
	cp.post(new Deviation(x,-74,nd));
	assertTrue(!cp.isFailed());
    assertEquals(x[1].getMax(),-10);

 }
    

}
