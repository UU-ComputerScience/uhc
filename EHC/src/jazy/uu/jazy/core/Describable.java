package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

import java.util.* ;

/**
 * Something which can be described
 */
public interface Describable
{
	/**
	 * Return description of internals.
	 */
	public String getInternalInfo() ;

	/**
	 * Return description of internals.
	 */
	public String getParentInfo() ;

	/**
	 * Return description of internals.
	 */
	public Enumeration getChildrenInfo() ;

}
