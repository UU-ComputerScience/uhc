package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

import java.util.* ;
import java.io.* ;

/**
 * Function accepting/expecting 2 parameter(s).
 * @see uu.jazy.core.Function
 */
public abstract class Function2 extends Function
{
    public Function2( )
    {
        nrParams = 2 ;
    }
    
    public Function2( String nm )
    {
        this() ;
        setName ( nm ) ;
    }
    
    public Function2( Function prev, String nm )
    {
        this( prev.getName() + "." + nm ) ;
    }
    
    /*
    protected Object eval1( Object v1 )
    {
        return apply1( v1 ) ;
    }
    */
        
    abstract protected Object eval2( Object v1, Object v2 ) ;
    
    public Apply apply1( Object v1 )
    {
        return new Apply1F2( this, v1) ;
    }
    
    public Apply apply2( Object v1, Object v2 )
    {
        return new Apply2F2( this, v1, v2 ) ;
    }
    
    public Apply apply3( Object v1, Object v2, Object v3 )
    {
        return apply2( v1, v2 ).apply1( v3 ) ;
    }
    
    public Apply apply4( Object v1, Object v2, Object v3, Object v4 )
    {
        return apply2( v1, v2 ).apply2( v3, v4 ) ;
    }
    
    public Apply apply5( Object v1, Object v2, Object v3, Object v4, Object v5 )
    {
        return apply2( v1, v2 ).apply3( v3, v4, v5 ) ;
    }
    
}

class Apply1F2 extends Apply1
{
	public Apply1F2( Object f, Object p1 )
	{
		super( f, p1 ) ;
		nrNeededParams = 1 ;
	}
	
    /*
    protected Object eval1( Object v1 )
    {
        return ((Function2)funcOrVal).eval2( p1, v1 ) ;
    }
    */
    
    public Apply apply1( Object v1 )
    {
        return new Apply2F2( funcOrVal, p1, v1) ;
        /*
        return new Apply1( this, v1 )
        {
            protected void evalSet( )
            {
                Apply1 app = ((Apply1)funcOrVal) ;
                funcOrVal =
                    ((Function)app.funcOrVal)
                        .eval2
                            ( app.p1
                            , p1
                            ) ;
                p1 = null ;
            }
        } ;
        */
    }
    
}

class Apply2F2 extends Apply2
{
	public Apply2F2( Object f, Object p1, Object p2 )
	{
		super( f, p1, p2 ) ;
	}
	
    protected void evalSet()
    {
        funcOrVal = ((Function2)funcOrVal).eval2( p1, p2 ) ;
        p1 = p2 = null ;
    }
    
}

