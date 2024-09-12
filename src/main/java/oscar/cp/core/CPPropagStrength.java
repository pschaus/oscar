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
package oscar.cp.core;

/**
 * A CPPropagStrength is used to ask for strength of filtering when adding/posting a constraint
 * to a Constraint Programming CPStore.
 * Three levels are possible, it doesn't mean all of them are implemented for every constraint. <br>
 * Note that usually, the strongest is the filtering the slowest is the propagation algorithm. <br>
 * Carefully choosing the filtering is usually done through experimentation (trade-off time and pruning power).
 */
public enum CPPropagStrength {
	Weak, Medium, Strong, Automatic
}
