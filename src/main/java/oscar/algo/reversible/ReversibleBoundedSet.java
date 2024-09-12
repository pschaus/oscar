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
package oscar.algo.reversible;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;



/**
 * Reversible Bounded Set, that is a set that can only contains values between 0 and a maxvalue known at the construction.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ReversibleBoundedSet implements Iterable<Integer>, Iterator<Integer>{
	
	private ReversibleContext node;
	
	private ReversibleInt first;
	
	private int n; //initial size of the set
	private ReversibleInt [] next;
	private ReversibleInt [] prev;
	
	private ReversibleInt size;
	
	private int iteVal;

    /**
     * Creates a reversible set following a reversible node that can only contain values from 0..maxval
     * @param node
     * @param maxval
     */
	public ReversibleBoundedSet(ReversibleContext node, int maxval) {
		if (maxval <0) {
			throw new  RuntimeException("maxval must be >= 0");
		}
		
		this.node = node;
		this.n = maxval+1;
		
		size = new ReversibleInt(this.node, 0);
		size.setValue(0);
		
		first = new ReversibleInt(this.node, 0);
		first.setValue(n);
		
		next = new ReversibleInt[n];
		prev = new ReversibleInt[n];
		for (int i = 0; i < next.length; i++) {
			next[i] =  new ReversibleInt(this.node, 0);
			next[i].setValue(n);
			prev[i] =  new ReversibleInt(this.node, 0);
			prev[i].setValue(n);
		}	
	}
	
	
	public void insert(int val){
		if (val <0 || val >= n) {
			throw new  RuntimeException("val must be between 0 and "+(n-1));
		}
		int i = first.getValue();
		if (i == n) { //currently empty
			first.setValue(val);
			size.incr();
		} else if (!contains(val)) {
			next[val].setValue(first.getValue());
			prev[first.getValue()].setValue(val);
			first.setValue(val);
			size.incr();
		}
	}
	
	public void remove(int val){
		int fv = first.getValue();
		if (fv != n) { //otherwise the list is empty
			if (fv == val ) { //first element
				if (next[val].getValue() == n) { //the set has only this element
					first.setValue(n);
				} else { //the set has more than one element
					first.setValue(next[val].getValue());
					prev[next[val].getValue()].setValue(val);
				}
				size.decr();
			} else if (next[val].getValue() != n) { //in the middle
				next[prev[val].getValue()].setValue(next[val].getValue());
				prev[next[val].getValue()].setValue(prev[val].getValue());
				next[val].setValue(n);
				prev[val].setValue(n);
				size.decr();
			} else if (prev[val].getValue() != n) { //last element
				next[prev[val].getValue()].setValue(n);
				prev[val].setValue(n);
				size.decr();
			}
		}
		assert(size.getValue() >= 0);
	}
	
	public int getSize() {
		return size.getValue();
	}
	
	public int first() {
		return first.getValue();
	}
	
	public int getNext(int val) {
		return next[val].getValue();
	}
	
	public boolean contains(int val){
		if (first.getValue() != n) { //otherwise the list is empty
			if (first.getValue() == val) {
				return true;
			} else if (prev[val].getValue() != n) { //at least two elements and val is not the first
				return true;
			}
		}
		return false;
	}
	
	public Set<Integer> getValues() {
		Set<Integer> res = new TreeSet<Integer>();
		int curr = first.getValue();
		while (curr != n) {
			res.add(curr);
			curr = next[curr].getValue();
		}
		return res;
	}
	
	public boolean isEmpty() {
		return size.getValue() == 0;
	}


	public Iterator<Integer> iterator() {
		iteVal = first();
		return this;
	}


	public boolean hasNext() {
		return iteVal != n;
	}


	public Integer next() {
		int oldVal = iteVal;
		iteVal = getNext(iteVal);
		return oldVal;
	}


	public void remove() {
		throw new RuntimeException("not implemented");
	}

}
