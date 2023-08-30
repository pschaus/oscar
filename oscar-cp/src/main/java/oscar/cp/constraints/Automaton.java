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
package oscar.cp.constraints;

import java.util.Set;

/**
 * Class to build an automaton to be used as parameter of a Regular constraint.
 * @see Regular
 * @see Stretch
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Automaton {
	
	private boolean posted;
	private int nbStates;
	private int nbLetters;
	private int [][] T; //transition matrix
	private int initialState;
	private Set<Integer> acceptingStates;
	private int nullState;

    /**
     *
     * @param nbStates  The number of states in the automaton. Ids of states are 0..nbStates-1.
     * @param nbLetters The number of letters of alphabet on which transitions can be defined. Ids of letters are 0..nbLetters-1.
     * @param initialState The Id of the initial state of the automaton.
     * @param acceptingStates The set of accepting states of the automaton.
     */
	public Automaton(int nbStates, int nbLetters,int initialState, Set<Integer> acceptingStates) {
		posted = false;
		for (Integer q : acceptingStates) {
			if (q >= nbStates || q < 0) {
				throw new RuntimeException("accepting states must be between 0 and "+(nbStates-1));
			}
		}
		if (initialState >= nbStates || initialState < 0) {
			throw new RuntimeException("initial must be between 0 and "+(nbStates-1));
		}
		if (nbStates <= 0 || nbLetters <= 0) {
			throw new RuntimeException("nbStates and nbLetters must be >0");
		}

		this.nbStates = nbStates;
		this.nbLetters = nbLetters;
		this.nullState = -1;
		T = new int [nbStates][nbLetters];
		for (int i = 0; i < T.length; i++) {
			for (int j = 0; j < T[i].length; j++) {
				T[i][j] = nullState;
			}
		}
		this.initialState = initialState;
		this.acceptingStates = acceptingStates;
	}
	
	protected void setPosted() { 
		posted = true;
	}

    /**
     * Adds a transition from state1 to state2 emitting letter
     * @param state1 a state in 0..nbStates-1
     * @param state2 a state in 0..nbStates-1
     * @param letter a state in 0..nbLetters-1
     */
	public void addTransition(int state1,int state2,int letter) {
		if (posted) {
			throw new RuntimeException("Automaton: automaton cannot be modified after being used in a constraint");
		}
		if (state1 >= nbStates || state1 < 0 ||
				state2 >= nbStates || state2< 0 ||
				letter >= nbLetters || letter < 0){
			System.out.println(state1+" "+state2+" "+letter);
			throw new RuntimeException("Automaton: invalid transition according to Alphabet and States");
		}
		if(T[state1][letter] != nullState){
			throw new RuntimeException("Automaton: this transition already exists (automaton must be deterministic)");
		}
		T[state1][letter] =  state2;
	}
	
	public int[][] getTransitionMatrix() {
		return T;
	}
	
	public int getNbStates() {
		return nbStates;
	}
	
	public int getNbLetters() {
		return nbLetters;
	}
	
	public int getNullState() {
		return nullState;
	}
	
	public int getInitialState() {
		return initialState;
	}
	
	public Set<Integer> getAcceptingStates() {
		return acceptingStates;
	}
}


