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
import java.util.TreeSet;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.util.ArrayUtils;

/**
 * Stretch Constraint: Constraint the maximum/minimum consecutive occurrences of numbers and transitions between them
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Stretch{

    /**
     * Creates an automaton that will ensure that value/letter i will appear in sequence of length of at least shortest[i]
     * and at most longest[i] in the sequence x. Such a sequence is called a stretch for the letter i.
     * @param x
     * @param shortest
     * @param longest
     * @return an automaton to pass as argument to the regular constraint
     * @see Regular
     *
     */
	public static Automaton getStretchAutomaton(CPIntVar [] x , int [] shortest, int [] longest) {
		int maxval = Math.max(shortest.length,longest.length) - 1;
		int nbval = maxval+1;
		int [] transiFrom = new int [nbval*nbval-nbval];
		int [] transiTo = new int [nbval*nbval-nbval];
		int i = 0;
		for (int j = 0; j <= maxval; j++) {
			for (int k = 0; k <= maxval; k++) {
				if (j != k) {
					transiFrom[i] = j;
					transiTo[i] = k;
					i++;
				}
			}
		}
		return getStretchAutomaton(x,shortest,longest,transiFrom,transiTo); 
	}


    /**
     * Creates an automaton that will ensure that value/letter i will appear in sequence of length of at least shortest[i]
     * and at most longest[i] in the sequence x. Such a sequence is called a stretch for the letter i. <br>
     * Also the only possible transition to go from on stretch to the next are (transiFrom[j],transiTo[j]) for all j. <br>
     * Example  x= [1,1,0,0,3,3,3] shortest=[2,1,0,3] longest=[3,2,1,3] transiFrom=[1,0] transiTo=[0,3].
     * @param x
     * @param shortest
     * @param longest
     * @param transiFrom
     * @param transiTo
     * @return
     */
	public static Automaton getStretchAutomaton(CPIntVar [] x, int [] shortest, int [] longest, int [] transiFrom, int [] transiTo) {
		if (transiFrom.length != transiTo.length) {
			throw new RuntimeException("getStretchAutomaton: transiFrom and transiTo must have the same length");
		}
		
		int maxval = Math.max(shortest.length,longest.length) - 1;
		int nbval = maxval+1;
		int [] sh = new int [nbval]; 
		int [] lo = new int [nbval];
		
		for (int i = 0; i < lo.length; i++) {
			sh[i] = 1;
			lo[i] = x.length;
		}
		for (int i = 0; i < shortest.length; i++) {
			if (shortest[i] > sh[i])
				sh[i] = shortest[i];
		}
		for (int i = 0; i < longest.length; i++) {
			if (longest[i] < lo[i])
				lo[i] = longest[i];
		}
		int nbStates = ArrayUtils.sum(longest) + 1 ;
		int [] stateStart = new int[nbval];
		int [] stateEnd = new int[nbval];
		
		Set<Integer> accepting = new TreeSet<Integer>();
		int i = 1;
		for (int k = 0; k < nbval; k++) {
			stateStart[k] = i;
			stateEnd[k] = i+lo[k]-1;
			for (int j = sh[k]; j <= lo[k]; j++) {
				accepting.add(i+j-1);
			}
			i += lo[k];
		}
		//System.out.println("nbStates:"+nbStates+" nbLetters:"+nbval+" accepting:"+accepting);
		Automaton automaton = new Automaton(nbStates,nbval,0,accepting);
		
		for (int v = 0; v < nbval; v++) {
			automaton.addTransition(0, stateStart[v], v);
			for (int j = stateStart[v]; j < stateEnd[v]; j++) {
				automaton.addTransition(j, j+1, v);
			}
		}
		
		//System.out.println("minval:"+minval);
		//System.out.println("TransiFrom "+Arrays.toString(transiFrom));
		//System.out.println("TransiTo "+Arrays.toString(transiTo));
		
		for (int j = 0; j < transiFrom.length; j++) {
			int from = transiFrom[j];
			int to = transiTo[j];
			for (int s = stateStart[from]+sh[from]-1; s <= stateEnd[from]; s++) {
				//System.out.println("=>"+Arrays.toString(stateStart)+" "+to);
				automaton.addTransition(s,stateStart[to],to);
			}
		}
		
		return automaton;
	}

	
}
