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

/**
 * Implementation of an Ordered set using double linked list (prev and succ array) on which you can only remove value (in constant time).
 * Optimally efficient iterator to iterate increasingly on values inside the set 
 * @author Pierre Schaus pschaus@gmail.com
 */
public abstract class AbstractOrderedSet implements Iterable<Integer> {


	private int min;
	private int max;


	
	public AbstractOrderedSet(int min, int max) {
		assert(max >= min);
		
		this.min = min;
		this.max = max;
		
	}
	
	protected void init() {
		setSize(max-min+1);
		setFirst(0);
		setLast(getSize()-1);
		for (int i = 0; i < getSize(); i++) {
			setNext(i,i+1);
			setPrev(i,i-1);
		}
		setNext(getSize()-1,-1);
	}
	

	protected abstract void setSize(int size);
	public abstract int getSize();
	protected abstract void setFirst(int f);
	public abstract int getFirst();
	protected abstract void setLast(int l);
	public abstract int getLast();
	protected abstract void setNext(int i,int v);
	public abstract int getNext(int i);
	protected abstract void setPrev(int i,int v);
	public abstract int getPrev(int i);
	
	
	
	public boolean hasValue(int v) {
		return getSize() > 0 && v >= min && v <= max && (getFirst()== v-min || (getNext(v-min) >= 0) || (getPrev(v-min) >= 0));
	}
	

	private boolean isFirst(int v) {
		return (v-min) == getFirst();
	}
	
	private boolean isLast(int v) {
		return (v-min) == getLast();
	}
	
	public void removeValue(int v) {
		if (hasValue(v)) {
			if (getSize() == 1) { // suppressing the only element
				setFirst(getSize());
				setLast(-1);
			}
			else if (isFirst(v)) { // suppressing the first element
				assert (getSize() >= 2);
				setPrev(getNext(getFirst()),-1);
				setFirst(getNext(getFirst()));
			} 
			else if (isLast(v)) {
				assert (getSize() >= 2);
				setNext(getPrev(getLast()),-1);
				setLast(getPrev(getLast()));
			} else { // in the middle
				assert (getSize() > 2);
				int next_old = getNext(v-min);
				int prev_old = getPrev(v-min);
				setNext(prev_old,next_old);
				setPrev(next_old,prev_old);
			}
			setPrev(v-min,-1);
			setNext(v-min,-1);
			setSize(getSize()-1);
		}
	}
	

	public boolean hasNext(int i) {
		return getNext(i) != -1;
	}
	
	public boolean hasPrev(int i) {
		return getPrev(i) != -1;
	}

	@Override
	public Iterator<Integer> iterator() {
		
		return new Iterator<Integer>() {
			private int iterIndex = getFirst();
			@Override
			public boolean hasNext() {
				return getSize() > 0 && iterIndex != -1;
			}

			@Override
			public Integer next() {
				int res = iterIndex + min;
				iterIndex = getNext(iterIndex);
				return res;
			}

			@Override
			public void remove() {
				throw new RuntimeException("not implemented");
				
			}
		};
	}




}
