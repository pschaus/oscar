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



/**
 * Implementation of an Ordered set using double linked list (prev and succ array) on which you can only remove value (in constant time). 
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ReversibleOrderedSet extends AbstractOrderedSet {
	
	private ReversibleInt size;
	private ReversibleInt first;
	private ReversibleInt last;
	private ReversibleInt [] prev;
	private ReversibleInt [] next;
	private ReversibleContext n;
	
	public ReversibleOrderedSet(ReversibleContext n, int min, int max) {
		super(min,max);
		this.n = n;
		prev = new ReversibleInt [max-min+1];
		next = new ReversibleInt [max-min+1];
		for (int i = 0; i < max-min+1; i++) {
			prev[i] = new ReversibleInt(n, 0);
			next[i] = new ReversibleInt(n, 0);
		}
		size = new ReversibleInt(n, 0);
		first = new ReversibleInt(n, 0);
		last = new ReversibleInt(n, 0);
		init();
	}
	


	@Override
	protected void setSize(int size) {
		this.size.setValue(size);
	}

	@Override
	public int getSize() {
		int siz =  this.size.getValue();
		return siz;
	}

	@Override
	protected void setFirst(int f) {
		this.first.setValue(f);
	}

	@Override
	public int getFirst() {
		return first.getValue();
	}

	@Override
	protected void setLast(int l) {
		this.last.setValue(l);
	}

	@Override
	public int getLast() {
		return last.getValue();
	}
	
	@Override
	protected void setNext(int i, int v) {
		this.next[i].setValue(v);
	}

	@Override
	public int getNext(int i) {
		return next[i].getValue();
	}

	@Override
	protected void setPrev(int i, int v) {
		prev[i].setValue(v);
	}

	@Override
	public int getPrev(int i) {
		return prev[i].getValue();
	}


}
