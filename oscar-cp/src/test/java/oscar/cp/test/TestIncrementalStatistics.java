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

import oscar.cp.*;
import oscar.cp.util.*;
import oscar.util.IncrementalStatistics;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestIncrementalStatistics extends TestCase {
	
	
	
    public TestIncrementalStatistics(String name) {
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
    
    public static double getAverage(double [] vals) {
    	double sum = ArrayUtils.sum(vals);
    	return sum/vals.length;
    }
    
    public static double getVariance(double [] vals) {
    	double res = 0;
    	double avg = getAverage(vals);
    	for (double v : vals) {
    		res += (v-avg)*(v-avg);
    	}
    	return res/vals.length;
    }
    
    public static int round(double a) {
    	return (int)(1000*a);
    }

    public void test0(){  	
    	double [] vals = new double[]{-3.5};
    	IncrementalStatistics stat = new IncrementalStatistics();
    	for (double v : vals) {
    		stat.addPoint(v);
    	}
    	assertEquals(round(stat.average()),round(getAverage(vals)));
    	assertEquals(round(stat.variance()),round(getVariance(vals)));
    }
    
    public void test2(){  	
    	double [] vals = new double[]{-3,-2,2,3,9,10};
    	IncrementalStatistics stat = new IncrementalStatistics();
    	for (double v : vals) {
    		stat.addPoint(v);
    	}
    	assertEquals(round(stat.average()),round(getAverage(vals)));
    	assertEquals(round(stat.variance()),round(getVariance(vals)));
    }
    
    
}
