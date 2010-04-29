package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 *
 * $Header:     $
 * $Archive:    $
 * $NoKeywords: $
 */

import java.util.* ;
import java.io.* ;

//import uu.jazy.prelude.* ;

/**
 * Miscellaneous utilities.
 * Currently public, will be hidden in future.
 */
public final class Utils
{
    private static final String zeroChars = "00000000" ;
    
    public static String asHex( int i, int nDigs, boolean hexIsDefault )
    {
        String s = Integer.toHexString( i ) ;
        int extra = 0 ;
        int len = s.length() ;
        if ( nDigs < len )
        	s = s.substring( len - nDigs ) ;
        else
        	extra = nDigs - len ;
        return (hexIsDefault ? zeroChars.substring( 0, extra ) : "0x") + s ;
    }
    
    public static String asHex( int i, int nDigs )
    {
        return asHex( i, nDigs, true ) ;
    }
    
    public final static Object[] zeroArray = new Object[0] ;
    
    public static Enumeration zeroEnumeration(  )
    {
        return arrayEnumeration( zeroArray ) ;
    }
    
    public static Enumeration oneEnumeration( Object v )
    {
        return arrayEnumeration( new Object[] {v}, false ) ;
    }
    
    public static Enumeration twoEnumeration( Object v1, Object v2 )
    {
        return arrayEnumeration( new Object[] {v1,v2}, false ) ;
    }
    
    public interface EnumerationMapper
    {
    	public Object map( Object v ) ;
    }
    
    public static Enumeration mapEnumeration( final EnumerationMapper f, final Enumeration e )
    {
    	return 
    		new Enumeration()
    			{
    				public boolean hasMoreElements()
    				{
    					return e.hasMoreElements() ;
    				}
    				
    				public Object nextElement()
    				{
    					return f.map( e.nextElement() ) ;
    				}
    			} ;
    }
    
    public static Enumeration arrayEnumeration( Object[] v, boolean makeCopy )
    {
    	if ( v == null )
    		return zeroEnumeration() ;
    		
        final Object[] vcopy = makeCopy ? arrayCopy( v ) : v ;
        v = null ;
        return new Enumeration()
        {
            private int pos = 0 ;
            
            public boolean hasMoreElements()
            {
                return pos < vcopy.length ;
            }
            
            public Object nextElement()
            {
                Object res = vcopy[ pos ] ;
                vcopy[ pos++ ] = null ;
                return res ;
            }
        } ;
    }
    
    public static Enumeration arrayEnumeration( Object[] v )
    {
        return arrayEnumeration( v, true ) ;
    }
    
    /*
    public static Enumeration nestedEnumeration( final Enumeration v )
    {
        return new Enumeration()
        {
            private int pos = 0 ;
            
            public boolean hasMoreElements()
            {
                return pos < v.length ;
            }
            
            public Object nextElement()
            {
                return v[ pos++ ] ;
            }
        } ;
    }
    */
    
    /*
    public static Enumeration stringEnumeration( final String v )
    {
        return new Enumeration()
        {
            private int pos = 0 ;
            
            public boolean hasMoreElements()
            {
                return pos < v.length() ;
            }
            
            public Object nextElement()
            {
                return Char.valueOf( v.charAt( pos++ ) ) ;
            }
        } ;
    }
    */
    
    public static Object[] arrayTake( int n, Object[] vn )
    {
        Object[] res ;
        if ( n < vn.length )
        {
            res = new Object[ n ] ;
            System.arraycopy( vn, 0, res, 0, n ) ;
        }
        else
        {
            res = vn ;
        }
        return res ;
    }
    
    public static Object[] arrayCopy( Object[] vn )
    {
        Object[] res = new Object[ vn.length ] ;
        System.arraycopy( vn, 0, res, 0, vn.length ) ;
        return res ;
    }
    
    public static Object[] arrayDrop( int n, Object[] vn )
    {
        Object[] res ;
        if ( n < vn.length )
        {
            int nRest = vn.length - n ;
            res = new Object[ nRest ] ;
            System.arraycopy( vn, n, res, 0, nRest ) ;
        }
        else
        {
            res = zeroArray ;
        }
        return res ;
    }
    
    public static Object[] arrayConcat( Object[] vn1, Object[] vn2 )
    {
        Object[] res = new Object[ vn1.length + vn2.length ] ;
        System.arraycopy( vn1, 0, res, 0, vn1.length ) ;
        System.arraycopy( vn2, 0, res, vn1.length, vn2.length ) ;
        return res ;
    }
    
    public static Object[] arrayMake( int sz, Object fill )
    {
        Object[] res = (Object[])java.lang.reflect.Array.newInstance( fill.getClass(), sz ) ;
        for ( int i = 0 ; i < sz ; i++ )
            res[ i ] = fill ;
        return res ;
    }
    
    public static Object[] arrayCons( Object v1, Object[] vn2 )
    {
        return arrayConcat( new Object[]{v1}, vn2 ) ;
    }
    
    public static Object[] toArray( Vector v )
    {
        Object[] res = new Object[ v.size() ] ;
        for ( int i = 0 ; i < res.length ; i++ )
            res[ i ] = v.elementAt( i ) ;
        return res ;
    }
    
    public static Vector<Object> vectorAdd( Vector<Object> v, Enumeration e )
    {
        while( e.hasMoreElements() )
            v.addElement( e.nextElement() ) ;
        return v ;
    }
    
    public static Vector vectorAddAsSet( Vector<Object> v, Object o )
    {
	   	if ( v.indexOf( o ) < 0 )
            v.addElement( o ) ;
        return v ;
    }
    
    public static Vector vectorAddAsSet( Vector<Object> v, Enumeration e )
    {
        while( e.hasMoreElements() )
        {
        	Object elt = e.nextElement() ;
        	if ( v.indexOf( elt ) < 0 )
	            v.addElement( elt ) ;
        }
        return v ;
    }
    
    public static Vector vectorConcat( Vector v1, Vector v2 )
    {
        Vector<Object> v = new Vector<Object>() ;
        return vectorAdd( vectorAdd( v, v1.elements() ), v2.elements() ) ;
    }
    
    public static String vectorToString( Vector v, char sep )
    {
        int sz = v.size() ;
        if ( sz == 0 )
            return "" ;
        String s = v.elementAt( 0 ).toString() ;
        for ( int i = 1 ; i < sz ; i++ )
            s = s + sep + v.elementAt( i ).toString() ;
        return s ;
    }
    
    public static Vector toVector( Object[] vs )
    {
    	Vector<Object> v = new Vector<Object>() ;
    	vectorAdd( v, arrayEnumeration( vs ) ) ;
    	return v ;
    }
    
    public static Vector toVector( Enumeration e )
    {
    	Vector<Object> v = new Vector<Object>() ;
    	vectorAdd( v, e ) ;
    	return v ;
    }
    
    private static void printSpacesOn( PrintStream o, int lev )
    {
        for ( int i = 0 ; i < lev ; i++ )
            o.print( ' ' ) ;
    }
    
    private static int idCounter ;
    
    private static void printCyclicOn( Object start, PrintStream o, int lev, Hashtable<Object,Object> alreadyDone, int expandLevel )
    {
        Object v = start ;
        //Object v = ( expandLevel > 0 ? eval( start ) : start ) ;
        boolean hasDone = true ;
        
        Object sid = null ;
        if ( v != null && ( sid = alreadyDone.get( v ) ) == null )
        {
        	sid = "#" + idCounter++ + "#" ;
        	alreadyDone.put( v, sid ) ;
        	hasDone = false ;
    	}
    	
    	//System.out.println( "Pr Cycl on val null? " + (v==null) + ", id " + sid ) ;
        if ( ( v instanceof Describable ) && ( ! hasDone ) && ( expandLevel > 0 ) )
        {
            Describable gr = (Describable)v ;
            printSpacesOn( o, lev ) ;
            o.print( "[" + sid + ":" + getClassName( gr ) + ": " + gr.getParentInfo() + " : " ) ;
            Enumeration successors = gr.getChildrenInfo() ;
            if ( successors.hasMoreElements() )
            {
                o.println() ;
                for( ; successors.hasMoreElements() ; )
                {
                    Object succ = successors.nextElement() ;
            		printCyclicOn( succ, o, lev+2, alreadyDone, expandLevel-1 ) ;
                }
                printSpacesOn( o, lev ) ;
            }
            o.println( "]" ) ;
        }
        else
        {
            printSpacesOn( o, lev ) ;
            if ( v == null )
                o.println( "" + v ) ;
            else                
                o.println( sid.toString() + ":" + getClassName( v ) + ": " + ((v instanceof Describable) ? (((Describable)v).getParentInfo()) : "??") ) ;
        }
    }

    public static void printCyclicOn( Object gr, PrintStream o, int lev, int expandLevel )
    {
        idCounter = 1000 ;
    	Hashtable<Object,Object> ht = new Hashtable<Object,Object>() ;
    	printCyclicOn( gr, o, lev, ht, expandLevel ) ;
    }
    
    public static void printCyclicOn( Object gr, PrintStream o, int expandLevel )
    {
        printCyclicOn( gr, o, 0, expandLevel ) ;
    }
    
    public static String getAfterLast( String s, char ch1, char ch2 )
    {
        int i = Math.max( s.lastIndexOf( ch1 ), s.lastIndexOf( ch2 ) ) ;
        return ( i < 0 ) ? s : s.substring( i+1 ) ;
    }
    
    public static String getAfterLastDot( String s )
    {
        int i = s.lastIndexOf( '.' ) ;
        return ( i < 0 ) ? s : s.substring( i+1 ) ;
    }
    
    public static String getBeforeLastDot( String s )
    {
        int i = s.lastIndexOf( '.' ) ;
        return ( i < 0 ) ? s : s.substring( 0, i ) ;
    }
    
    public static String getAfterBeforFirstDollar( String s )
    {
        int i = s.lastIndexOf( '$' ) ;
        return ( i < 0 ) ? s : s.substring( 0, i ) ;
    }
    
    public static Vector splitAt( String s, char sep )
    {
        Vector<Object> v = new Vector<Object>() ;
        for ( int pos = s.indexOf( sep ) ; pos >= 0 ; pos = s.indexOf( sep ) )
        {
            v.addElement( s.substring( 0, pos ) ) ;
            s = s.substring( pos+1 ) ;
        }
        v.addElement( s ) ;
        return v ;
    }
    
	public static String getClassName( Object o )
	{
		return getAfterLast( o.getClass().getName(), '.', '$' ) ;
	}

	public static String getClassAllName( Object o )
	{
		return getAfterLastDot( o.getClass().getName() ) ;
	}

	public static String getClassClassName( Object o )
	{
		return getAfterBeforFirstDollar( getClassAllName( o ) ) ;
	}

	public static String toString( Describable d )
	{
		return "[" + getAfterLastDot( d.getClass().getName() ) + ": " + d.getInternalInfo() + "]" ;
	}

    public static void printAsListOn( Enumeration vals, StringBuffer buf, String sOpen, String sSep, String sClose )
    {
    	buf.append( sOpen ) ;
    	while ( vals.hasMoreElements() )
    	{
    		buf.append( "" + vals.nextElement() ) ;
    		if ( vals.hasMoreElements() )
    			buf.append( sSep ) ;
    	}
    	buf.append( sClose ) ;
    }

    /*
    public static void showAsListOn( Enumeration vals, PrintWriter output, String sPre, String sOpen, String sSep, String sClose )
    {
        if ( vals.hasMoreElements() )
        {
            output.print( sPre ) ;
            showAsListOn( vals, output, sOpen, sSep, sClose ) ;
        }
    }
    */
    
    /*
    public static void showAsListOn( Enumeration vals, PrintWriter output, String sOpen, String sSep, String sClose )
    {
    	output.print( sOpen ) ;
    	while ( vals.hasMoreElements() )
    	{
    		Eval.show( output, vals.nextElement() ) ;
			output.flush() ;
    		if ( vals.hasMoreElements() )
    			output.print( sSep ) ;
    	}
    	output.print( sClose ) ;
    }
    */

	public static Describable makeDescribableWrapper( final Object x )
	{
		return new Describable()
		{
			public String getInternalInfo() { return getParentInfo() ; }

			public String getParentInfo() { return "" + x ; }

			public Enumeration getChildrenInfo() { return zeroEnumeration() ; }
		} ;
	}
}

