package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

import java.util.* ;
import java.io.* ;

/**
 * Function accepting/expecting 5 parameter(s).
 * @see uu.jazy.core.Function
 */
public abstract class Function5 extends Function
{
    public Function5( )
    {
        nrParams = 5 ;
    }
    
    public Function5( String nm )
    {
        this() ;
        setName ( nm ) ;
    }
    
    public Function5( Function prev, String nm )
    {
        this( prev.getName() + "." + nm ) ;
    }
            
    abstract protected Object eval5( Object v1, Object v2, Object v3, Object v4, Object v5 ) ;

    public Apply apply1( Object v1 )
    {
        return new Apply1F5( this, v1 ) ;
    }
    
    public Apply apply2( Object v1, Object v2 )
    {
        return new Apply2F5( this, v1, v2 ) ;
    }
    
    public Apply apply3( Object v1, Object v2, Object v3 )
    {
        return new Apply3F5( this, v1, v2, v3 ) ;
    }
    
    public Apply apply4( Object v1, Object v2, Object v3, Object v4 )
    {
        return new Apply4F5( this, v1, v2, v3, v4 ) ;
    }
    
    public Apply apply5( Object v1, Object v2, Object v3, Object v4, Object v5 )
    {
        return new Apply5F5( this, v1, v2, v3, v4, v5 ) ;
    }
    
}

class Apply1F5 extends Apply1
{
	public Apply1F5( Object f, Object p1 )
	{
		super( f, p1 ) ;
		nrNeededParams = 4 ;
	}
	
    public Apply apply1( Object v1 )
    {
        return new Apply2F5( funcOrVal, p1, v1) ;
    }
    
    public Apply apply2( Object v1, Object v2 )
    {
        return new Apply3F5( funcOrVal, p1, v1, v2) ;
    }
    
    public Apply apply3( Object v1, Object v2, Object v3 )
    {
        return new Apply4F5( funcOrVal, p1, v1, v2, v3) ;
    }
    
    public Apply apply4( Object v1, Object v2, Object v3, Object v4 )
    {
        return new Apply5F5( funcOrVal, p1, v1, v2, v3, v4 ) ;
    }
    
}

class Apply2F5 extends Apply2
{
	public Apply2F5( Object f, Object p1, Object p2 )
	{
		super( f, p1, p2 ) ;
		nrNeededParams = 3 ;
	}
	
    public Apply apply1( Object v1 )
    {
        return new Apply3F5( funcOrVal, p1, p2, v1 ) ;
    }
    
    public Apply apply2( Object v1, Object v2 )
    {
        return new Apply4F5( funcOrVal, p1, p2, v1, v2) ;
    }
    
    public Apply apply3( Object v1, Object v2, Object v3 )
    {
        return new Apply5F5( funcOrVal, p1, p2, v1, v2, v3) ;
    }
    
}

class Apply3F5 extends Apply3
{
	public Apply3F5( Object f, Object p1, Object p2, Object p3 )
	{
		super( f, p1, p2, p3 ) ;
		nrNeededParams = 2 ;
	}
	
    public Apply apply1( Object v1 )
    {
        return new Apply4F5( funcOrVal, p1, p2, p3, v1 ) ;
    }
    
    public Apply apply2( Object v1, Object v2 )
    {
        return new Apply5F5( funcOrVal, p1, p2, p3, v1, v2) ;
    }
    
}

class Apply4F5 extends Apply4
{
	public Apply4F5( Object f, Object p1, Object p2, Object p3, Object p4 )
	{
		super( f, p1, p2, p3, p4 ) ;
		nrNeededParams = 1 ;
	}
	
    public Apply apply1( Object v1 )
    {
        return new Apply5F5( funcOrVal, p1, p2, p3, p4, v1 ) ;
    }
    
}

class Apply5F5 extends Apply5
{
	public Apply5F5( Object f, Object p1, Object p2, Object p3, Object p4, Object p5 )
	{
		super( f, p1, p2, p3, p4, p5 ) ;
	}
	
    protected void evalSet(  )
    {
        funcOrVal = ((Function5)funcOrVal).eval5( p1, p2, p3, p4, p5 ) ;
        p1 = p2 = p3 = p4 = p5 = null ;
    }
    
}

