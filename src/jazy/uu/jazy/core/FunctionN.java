package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

import java.util.* ;
import java.io.* ;

/**
 * Function accepting/expecting n parameter(s).
 * @see uu.jazy.core.Function
 */
public abstract class FunctionN extends Function
{
    public FunctionN( int np )
    {
        nrParams = np ;
    }
    
    public FunctionN( int np, String nm )
    {
        this( np ) ;
        setName ( nm ) ;
    }
    
    public FunctionN( int np, Function prev, String nm )
    {
        this( np, prev.getName() + "." + nm ) ;
    }
    
    protected abstract Object evalN( Object[] vn ) ;

    /**
     * Either evaluate or apply with arguments. Choice is made here.
     */
    protected Object evalOrApplyN( Object[] vn )
    {
        Object res = null ;
        if ( nrParams > vn.length )
        {
            res = applyN( vn ) ;
        }
        else
        {
            res = ((FunctionN)this).evalN( Utils.arrayTake( nrParams, vn ) ) ;
            if ( nrParams != vn.length )
                res = ((Eval)res).applyN( Utils.arrayDrop( nrParams, vn ) ) ;
        }
        return res ;
    }
    
    public Apply apply1( Object v1 )
    {
        return applyN( new Object[] {v1} ) ;
    }
    
    public Apply apply2( Object v1, Object v2 )
    {
        return applyN( new Object[] {v1,v2} ) ;
    }
    
    public Apply apply3( Object v1, Object v2, Object v3 )
    {
        return applyN( new Object[] {v1,v2,v3} ) ;
    }
    
    public Apply apply4( Object v1, Object v2, Object v3, Object v4 )
    {
        return applyN( new Object[] {v1,v2,v3,v4} ) ;
    }
    
    public Apply apply5( Object v1, Object v2, Object v3, Object v4, Object v5 )
    {
        return applyN( new Object[] {v1,v2,v3,v4,v5} ) ;
    }
    
    public Apply applyN( Object[] vn )
    {
    	if ( vn.length < nrParams )
    		return new ApplyNFNeedsMore( this, vn, nrParams-vn.length ) ;
    	else if ( vn.length == nrParams )
    		return new ApplyNFN( this, vn ) ;
    	else
    		return new ApplyNFN( this, Utils.arrayTake( nrParams, vn ) ).applyN( Utils.arrayDrop( nrParams, vn ) ) ;
    		//return new ApplyNFTooMuch( this, vn ) ;
    }
    
}

class ApplyNFNeedsMore extends ApplyN
{
	public ApplyNFNeedsMore( Object f, Object[] pn, int np )
	{
		super( f, pn ) ;
		nrNeededParams = np ;
	}
	
	/*
	protected Object eval0()
	{
	    return this ;
	}
	*/
	
	public Apply applyN( Object[] vn )
	{
		if ( vn.length == 0 )
			return this ;
		else
		    return ((FunctionN)funcOrVal).applyN( Utils.arrayConcat( pN, vn ) ) ;
	}
	
}

class ApplyNFN extends ApplyN
{
	public ApplyNFN( Object f, Object[] pn )
	{
		super( f, pn ) ;
	}
	
	protected void evalSet()
	{
		funcOrVal = ((FunctionN)funcOrVal).evalN( pN ) ;
		pN = null ;
	}
	
}

class ApplyNFTooMuch extends ApplyN
{
	public ApplyNFTooMuch( Object f, Object[] pn )
	{
		super( f, pn ) ;
	}
	
	protected void evalSet()
	{
	    FunctionN f = (FunctionN)funcOrVal ;
		funcOrVal =
		    ((Eval)f.evalN
		        ( Utils.arrayTake( f.nrParams, pN )
		        )
		    ).applyN( Utils.arrayDrop( f.nrParams, pN ) ) ;
	    pN = null ;
	}
	
}

