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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

/**
 * Implementation of a TrailSet using an indexed array.
 * 
 * An indexed array represented with:
 * 
 * n , only values [0,...,n-1] are potentially in the set
 * int [] values , is a permutation of [0,...,n-1]
 * int [] indexes , is a permutation of [0,...,n-1]
 * int size ,the cardinality of the set (between 0 and n)
 * 
 * This representation encodes the values: {values[0],values[1],...,values[size-1]}
 * indexes[v] gives the position of the value v =>  values[indexes[v]] == v
 * 
 * When a value is removed, its position swapped with the values[size-1] and 
 * the indexes are updated to keep track in constant time of any position of any value.
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
public abstract class AbstractSparseSet implements Iterable<Integer> {
	
	private int _min;
	private int [] values;
	private int [] indexes;
	
	//these abstract methods are used to encapsulate operations that could be used
	//for int or ReversibleInt to make the set reversible
	protected abstract void createSizeMinMax();
	protected abstract void setSize(int size);
	protected abstract void setMin(int min);
	protected abstract void setMax(int max);
	public abstract int getSize();
	public abstract int getMin();
	public abstract int getMax();
	
	private void incrSize() {
		setSize(getSize()+1);
	}
	
	private void decrSize() {
		setSize(getSize()-1);
	}
	
	private int iterIndex; // to implement the iterator

	
	protected void initIndexes(int min, int max, boolean empty) {
		assert(max >= min);
		createSizeMinMax();
		this._min = min;
		values = new int [max-min+1];
		indexes = new int [max-min+1];
		for (int i = 0; i < values.length; i++) {
			values[i] = i;
			indexes[i] = i;
		}
		if (!empty)
			setSize(values.length);
		else 
			setSize(0);
		setMax(max);
		setMin(min);
	}
	
	public void insert(int val) {
		assert(checkVal(val));
		if (hasValue(val)) return;
		else if (isEmpty()) {
			setMin(val);
			setMax(val);
		} else {
			if (val > getMax()) setMax(val);
			if (val < getMin()) setMin(val);
		}
        int s = getSize();
		exchangePositions(val, values[s]+_min);
		incrSize();
		assert(getSize() <= values.length);
	}
	
	private void exchangePositions(int val1, int val2) {

		assert(checkVal(val1));
		assert(checkVal(val2));
		int v1 = val1-_min;
		int v2 = val2-_min;
		int i1 = indexes[v1];
		int i2 = indexes[v2];
		values[i1] = v2;
		values[i2] = v1;
		indexes[v1] = i2;
		indexes[v2] = i1;
	}
	
	private boolean checkVal(int val) {
		assert(val >= _min);
		assert(val <= _min+values.length-1);
		return true;
	}

	/**
	 * @return an array representation of values present in the set
	 */
	public int[] toArray()  {
		int [] res = new int[getSize()];
		fillArray(res);
		return res;
	}

	/**
	 * @return set the first values of dest to the ones
	 *         of the set and return the size of the set
	 */
	public int fillArray(int [] dest) {
		int size = getSize();
		System.arraycopy(values, 0, dest, 0, size);
		if (_min != 0) {
			int i = size;
			while (i > 0) {
				i -= 1;
				dest[i] += _min;

			}
		}
		return size;
	}


	/**
	 * remove all elements in the set
	 */
	public void empty() {
		setSize(0);
	}
	
	/**
	 * @return true if the set is empty
	 */
	public boolean isEmpty() {
		return getSize() == 0;
	}
	
	private void updateBoundsValRemoved(int val) {
		updateMaxValRemoved(val);
		updateMinValRemoved(val);
	}
	
	private void updateMaxValRemoved(int val) {
		if (!isEmpty() && getMax() == val) {
			assert(!hasValue(val));
			//the maximum was removed, search the new one
			for (int v = val-1; v >= getMin(); v--) {
				if (hasValue(v)) {
					setMax(v);
					return;
				}
			}
		}
	}
	
	private void updateMinValRemoved(int val) {
		if (!isEmpty() && getMin() == val) {
			assert(!hasValue(val));
			//the minimum was removed, search the new one
			for (int v = val+1; v <= getMax(); v++) {
				if (hasValue(v)) {
					setMin(v);
					return;
				}
			}
		}
	}
	
	public boolean removeValue(int val) {
		assert(checkVal(val));
		if (!hasValue(val)) return false; //the value has already been removed
		int s = getSize();
		exchangePositions(val, values[s-1]+_min);
		decrSize();
		updateBoundsValRemoved(val);
		return true;
	}

	public boolean hasValue(int val) {
		if (val < _min || val >= _min+indexes.length) return false;
		return indexes[val-_min] < getSize();
	}

	public int getNextValue(int val) {
		assert(checkVal(val));
		assert(!isEmpty());
		for (int v = val; v <= getMax(); v++) {
			if (hasValue(v)) {
				return v;
			}
		}
		return val-1;
	}

	public int getPreValue(int val) {
		assert(checkVal(val));
		assert(!isEmpty());
		for (int v = val; v >= getMin(); v--) {
			if (hasValue(v)) {
				return v;
			}
		}
		return val+1;
	}
	
	public void removeAllBut(int v) {
		// we only have to put in first position this value and set the size to 1
		assert(checkVal(v));
		assert(hasValue(v));
		int val = values[0];
		int index = indexes[v-_min];
		indexes[v-_min] = 0;
		values[0] = v -_min;
		indexes[val] = index;
		values[index] = val;
		setMin(v);
		setMax(v);
		setSize(1); 
	}
	
	public int setMinVal(int min) {
		assert(checkVal(min));
		assert(!isEmpty());
		if (min < getMin()) {
			return getMin(); // the min does not change
		}
		else if (min > getMax()) {
			setSize(0);
			return Integer.MAX_VALUE; // the set becomes empty since the new min is larger than the current max
		} 
		else if (min == getMax()) {
			// the new min is equal to the current max hence only one value in the set
			removeAllBut(min);
		} 
		else { 
			for (int v = getMin(); v < min; v++ ) {
				removeValue(v);
			}
		}
		return getMin();
	}

	public int setMaxVal(int max) {
		assert(checkVal(max));
		assert(!isEmpty());
		if (max >= getMax()) {
			return getMax(); // the max does not change
		}
		else if (max < getMin()) {
			setSize(0);
			return Integer.MIN_VALUE; // the set becomes empty since the new max is smaller than the current min
		} 
		else if (max == getMin()) {
			// the new max is equal to the current min hence only one value in the set
			removeAllBut(max);
		} 
		else { 
			for (int v = getMax(); v > max; v-- ) {
				removeValue(v);
			}
		}
		return getMax();
	}

	public Integer[] getValues() {
		if (isEmpty()) return new Integer[]{};
		ArrayList<Integer> vals = new ArrayList<Integer>();
		for (int v = getMin(); v <= getMax(); v++) {
			if (hasValue(v)) {
				vals.add(v);
			} else {
				//System.out.println("do not have value"+v);
			}
		}
		Integer [] values = vals.toArray(new Integer[]{});
		Arrays.sort(values);
		return values;
	}
	
	@Override
	public String toString() {
		return Arrays.toString(getValues());
	}

	
	public Iterator<Integer> iterator() {
		iterIndex = 0;
		return new Iterator<Integer>() {

			@Override
			public boolean hasNext() {
				return iterIndex < getSize();
			}

			@Override
			public Integer next() {
				assert(hasNext());
				int i = iterIndex;
				iterIndex++;
				return values[i]+_min;
			}

			@Override
			public void remove() {
				if(removeValue(values[iterIndex-1]+_min)) {
					iterIndex--;
				}
				//throw new RuntimeException("not implemented");
			}
		};
	}

    /**
     * @return  The values in the set sorted increasingly
     */
    public int [] getSortedVals() {
        int [] vals = new int [getSize()];
        int i = 0;
        Iterator<Integer> ite = this.iterator();
        while (ite.hasNext()) {
             vals[i++] = ite.next() ;
        }
        Arrays.sort(vals);
        return vals;
    }

}
