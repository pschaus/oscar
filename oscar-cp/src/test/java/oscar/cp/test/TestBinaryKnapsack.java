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
import oscar.cp.constraints.BinaryKnapsack;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPBoolVar;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.CPStore;
import oscar.cp.util.ArrayUtils;

import junit.framework.TestCase;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestBinaryKnapsack extends TestCase {
	

	

    public TestBinaryKnapsack(String name) {
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
    
    public void testa() {

        CPStore s = new CPStore();
        CPBoolVar [] b = new CPBoolVar[111];
        for (int i = 0; i < b.length; i++) {
			b[i] = CPBoolVar.apply(s);
		}

        CPIntVar l = CPIntVar.apply(s,12,44);
        int [] w = new int[]{2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 12, 12, 12, 13, 13, 13, 13, 13, 14, 14, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 17, 17, 18, 18, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 25, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 30, 30, 30, 30};

    	//propagate binary knapsack
    	BinaryKnapsack bkp = new BinaryKnapsack(b,w,l);
        s.add(bkp, CPPropagStrength.Weak);
        s.add(bkp,CPPropagStrength.Strong);

        s.add(new oscar.cp.constraints.DiffVal(b[0],0));
        assertFalse(s.isFailed());
    }



    public void testb() {
        CPStore cp = new CPStore();
        int n = 20;
        CPBoolVar [] x = new CPBoolVar[n];
        for (int i = 0; i < n; i++) {
        	x[i] = CPBoolVar.apply(cp);
        }
        int [] values = new int[n];
        int [] values2 = new int[n];
        for (int i = 0; i < n; i++) {
            values[i] = i+1;
            values2[i] = values[i]*values[i];
        }
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Strong);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Strong);
        boolean [] sol = new boolean[]{true, false, false, true, false, false, true, true, true, true, false, true, false, false, false, true, false, true, false, true};
        for (int i = 0; i < sol.length; i++) {
            if (i == sol.length/2) {
                cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Weak);
                cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Weak);
                cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Strong);
                cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Strong);
            }

            if (sol[i]) {
                cp.add(x[i].constraintTrue());
            } else {
                cp.add(x[i].constraintFalse());
            }
        }
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Strong);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Strong);

        assertFalse(cp.isFailed());
    }

    public void testc() {
        CPStore cp = new CPStore();
        CPBoolVar [] x = new CPBoolVar[3];
        for (int i = 0; i < 3; i++) {
        	x[i] = CPBoolVar.apply(cp);
        }
        int [] values = new int[]{43,23,23};
        CPIntVar c = CPIntVar.apply(cp,1,82);

        cp.add(new BinaryKnapsack(x,values, c), CPPropagStrength.Strong);

        assertFalse(cp.isFailed());
        assertEquals(c.getMin(),23);
        assertEquals(c.getMax(),66);
    }




    
   
}

