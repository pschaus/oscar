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
 * Recursive linked queue. <br>
 * A queue contains an element and a pointer to a next queue. <br>
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Queue<T>{
	protected T elem;
	protected Queue<T> next = null;

    /**
     * Creates a new queue with one element inside <br>
     *
     * @param next queue. <br>
     *        Set it to null to create an initial queue with just one element. <br>
     *        To add an element in front of an existing queue q, you have to do: q = new Queue(q,elem).
     * @param elem
     */
	public Queue(Queue<T> next,T elem) {
		this.elem = elem;
		this.next = next;
	}

    /**
     *
     * @return  true if there is an element after the one in this queue
     */
	public boolean hasNext() {
		return this.next != null;
	}

    /**
     * @return the next queue, or null if no next queue (last one).
     */
	public Queue<T> getNext() {
		return next;
	}

    /**
     *
     * @return the element in the current position of the queue
     */
	T getElem() {
		return elem;
	}
	
	@Override
	public String toString() {
		String res = "";
		Queue<T> q = this;
		do {	
			T e = q.getElem();
			res += e.toString() + (q.hasNext() ? "->" : "");
			q = q.getNext();
		} while (q != null);
		return res;
	}
	
	/**
	 * @return The size of the queue in Theta(n)
	 */
	public int getSize() {
		int size = 0;
		Queue<T> q = this;
		do {
			size++;
			q = q.getNext();
		} while(q != null);
		return size;
	}
}
