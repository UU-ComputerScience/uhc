package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

import java.util.* ;
import java.io.* ;

/**
 * Base class for functions.
 * A Function is an object than can be evaluated, and accepts parameters for its evaluation.
 * This is done by invoking evalX() methods.
 * A Function object can be applied to parameters.
 * This is done by invoking applyX() methods.
 * These methods return another Eval, which -when evaluated- will
 * use the bound parametervalues.
 * Subclasses FunctionX are specialized w.r.t. the nr of parameters.
 *
 * @see uu.jazy.core.Eval
 * @see uu.jazy.core.Apply
 * @see uu.jazy.core.Function1
 * @see uu.jazy.core.Function2
 * @see uu.jazy.core.Function3
 * @see uu.jazy.core.Function4
 * @see uu.jazy.core.FunctionN
 */
public abstract class Function extends Eval
//    implements Serializable, GraphAlike
{
    protected int nrParams ;
    //protected short nrLocals = 0 ;
    
    public final static String defaultNoName = "??" ;
    
    private String name = defaultNoName ;
    
    protected void setName( String nm )
    {
    	name = nm ;
    }
    
    protected String getName( )
    {
    	return name ;
    }
    
    /**
     * Either evaluate or apply with arguments. Choice is made here.
     */
    protected Object evalOrApplyN( Object[] vn )
    {
        Object res = null ;
        //int np = getNrParams() ;
        if ( nrParams > vn.length )
        {
            res = applyN( vn ) ;
        }
        else
        {
            switch( nrParams )
            {
                case 1  : res = ((Function1)this).eval1( vn[0] ) ; break ;
                case 2  : res = ((Function2)this).eval2( vn[0], vn[1] ) ; break ;
                case 3  : res = ((Function3)this).eval3( vn[0], vn[1], vn[2] ) ; break ;
                case 4  : res = ((Function4)this).eval4( vn[0], vn[1], vn[2], vn[3] ) ; break ;
                case 5  : res = ((Function5)this).eval5( vn[0], vn[1], vn[2], vn[3], vn[4] ) ; break ;
                default : res = ((FunctionN)this).evalN( Utils.arrayTake( nrParams, vn ) ) ; break ;
            }
            if ( nrParams != vn.length )
            {
                //res = ((Eval)res).evalOrApplyN( Utils.arrayDrop( np, vn ) ) ;
                res = ((Eval)res).applyN( Utils.arrayDrop( nrParams, vn ) ) ;
            }
        }
        return res ;
    }
    
    /**
     * @see uu.jazy.core.Eval#applyN
     */
    public Apply applyN( Object[] vn )
    {
        switch ( vn.length )
        {
            case 1  : return apply1( vn[0] ) ;
            case 2  : return apply2( vn[0], vn[1] ) ;
            case 3  : return apply3( vn[0], vn[1], vn[2] ) ;
            case 4  : return apply4( vn[0], vn[1], vn[2], vn[3] ) ;
            case 5  : return apply5( vn[0], vn[1], vn[2], vn[3], vn[4] ) ;
            default : return new ApplyN( this, vn ) ;
        }
    }

    public int getNrParams()
    {
        return nrParams ;
    }

    /**
     * @see uu.jazy.core.Eval#getBoundParams
     */
    public Object[] getBoundParams()
    {
        return Utils.zeroArray ;
    }

    protected Function getOrigFunction()
    {
        return this ;
    }
    
    /**
     * Get type info.
     * A rudimentary attempt to catch some of the information needed to debug an exception.
     * Meant to be overridden.
     * @return	array with name, returntype, parametertypes.
     */
    // protected String[] getTypeInfo( )
    //{
    //    return null ;
    //}

	/*
	public String getContentString()
	{
	    String myName ;
	    String myResult ;
	    Enumeration myParams ;
		String ti[] = null ; // getTypeInfo() ;
		StringBuffer b = new StringBuffer() ;
		b.append( Utils.getAfterLastDot( getClass().getName() ) ) ;
		if ( ti == null )
		{
	        myParams = Utils.zeroEnumeration() ;
	        myName = "??" ;
	        myResult = myName ;
		}
		else
		{
	        myParams = Utils.arrayEnumeration( ti ) ;
	        myName = (String)myParams.nextElement() ;
	        myResult = (String)myParams.nextElement() ;
    	}
		b.append( "[=" + myName + "/#needs=" + getNrParams() ) ;
		b.append( "/formals=" ) ;
		Utils.printAsListOn( myParams, b, "(", ",", ")" ) ;
		b.append(  "->" + myResult ) ;
		b.append( "]" ) ;
		return b.toString() ;
	}
	*/
	
	public String getParentInfo()
	{
		return Utils.getClassClassName(this) + "." + getName() + " np=" + getNrParams() ;
	}

	public String getInternalInfo()
	{
		return getParentInfo() ;
	}

	public Enumeration getChildrenInfo()
	{
	    return Utils.zeroEnumeration() ;
	}

}

