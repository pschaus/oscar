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

public class SparseSet extends AbstractSparseSet {

	public SparseSet(int min, int max) {
		this(min, max, false);
	}
	
	public SparseSet(int min, int max, boolean empty) {
		initIndexes(min, max, empty);
	}

	private int size;
	private int maxV;
	private int minV;
	
	@Override
	protected void createSizeMinMax() {
		size = 0;
		maxV = 0;
		minV = 0;	
	}
	
	@Override
	protected void setSize(int size) {
		this.size = size;
		
	}
	
	@Override
	protected void setMin(int min) {
		this.minV = min;
	}
	
	@Override
	protected void setMax(int max) {
		this.maxV = max;
	}
	
	@Override
	public int getSize() {
		return size;
	}
	
	@Override
	public int getMin() {
		return minV;
	}
	
	@Override
	public int getMax() {
		return maxV;
	}
	
}
