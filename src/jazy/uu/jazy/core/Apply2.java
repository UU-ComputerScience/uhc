package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

/**
 * An application of a Eval to 2 parameters.
 */
class Apply2 extends Apply
{
	//private static Stat statNew = Stat.newNewStat( "Apply2" ) ;
	
	protected Object p1, p2 ;
	
	public Apply2( Object f, Object p1, Object p2 )
	{
		super( f ) ;
		this.p1 = p1 ;
		this.p2 = p2 ;
		//statNew.nrEvents++ ;
	}
	
    protected void eraseRefs()
    {
    	//function = null ;
    	p1 = p2 = null ;
    }
    
    public Object[] getBoundParams()
    {
	    if ( p1 == null )
	        return Utils.zeroArray ;
	    return new Object[] {p1,p2} ;
    }

    public int getNrBoundParams()
    {
        return 2 ;
    }

}
