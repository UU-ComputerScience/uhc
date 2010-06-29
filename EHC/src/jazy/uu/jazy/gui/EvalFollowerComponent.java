/** 
 * EvalFollowerComponent.java
 *
 * Title:			Evaluation Follower
 * Description:		GUI to follow evaluation of graph reduction evaluation
 * @author			atze
 */

package uu.jazy.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import uu.jazy.core.* ;
import uu.jazy.ehc.* ;

import java.util.* ;

public class EvalFollowerComponent extends JPanel
	implements EvalTracer, MouseListener, Scrollable
{
	//
	// Required
	//
	static final long serialVersionUID = 1L;

	//
	// Colors
	//
	
	private static Color colorApply    = Color.yellow ;
	private static Color colorTuple    = new Color( 0xFF, 0x40, 0xFF ) ;
	private static Color colorFunction = Color.green ;
	private static Color colorData     = new Color( 0x40, 0xFF, 0xFF ) ;
	private static Color colorDefault  = Color.white ;
	
	private static Color describableColor( Object x )
	{
		if ( x instanceof Apply )
			return colorApply ;
		else if ( x instanceof Function )
			return colorFunction ;
		else if ( x instanceof Data )
			return colorData ;
		else if ( x instanceof Object[] )
			return colorTuple ;
		else
			return colorDefault ;
	}
	
	//
	// Fonts
	//
	
	private static Font normalFont = new Font("SansSerif", Font.PLAIN, 10) ;
	private static Font underEvalFont = new Font("SansSerif", Font.ITALIC, 10) ;
	private static Font lineLabelFont = new Font("SansSerif", Font.PLAIN, 9) ;
	
	//
	// DrawInfo
	//
	
	private static class DrawInfo
	{
		Rectangle parentRectangle ;
		int spentHeight ;
		
		DrawInfo( Rectangle pr, int sh )
		{
			parentRectangle = pr ;
			spentHeight = sh ;
		}
	}
	
	//
	// Model
	//
	
	private Describable model = null ;
	
	public void setModel( Describable x )
	{
		model = x ;
		repaint() ;
	}
	
	public void setModel( Object x )
	{
		setModel
			( x instanceof Describable
			? (Describable)x
			: uu.jazy.core.Utils.makeDescribableWrapper( x ) 
			) ;
	}
	
	//
	// Triggering the repaint and continuation
	//
	
	private Semaphore continueSema = new Semaphore() ;
	
	//
	// Stack
	//
	
	private static Rectangle stackEltRect = new Rectangle( 0, 0, 32, 16 ) ;
	private static Point stackPos = new Point( 2, 2 ) ;
	
	private Stack<Object> stack = new Stack<Object>() ;
	
	private int selectedStackElt = -1 ;
	
	private Rectangle getStackRect()
	{
		int h = stackEltRect.height * stack.size() ;
		return
			new Rectangle
				( stackPos.x
				, stackPos.y
				, stackEltRect.width
				, h
				) ;
	}
	
	public EvalFollowerComponent()
	{
		setOpaque( true ) ;
		setFont( normalFont ) ;
		
		addMouseListener( this ) ;
		
		setPreferredSize( new Dimension( 600, 500 ) ) ;
	}

	//
	// Drawing
	//
	
	private void drawLabeledLineBetweenRect( Graphics g, Rectangle r1, Rectangle r2, Color c, int nr )
	{
		int gap = 10 ;
		g.setColor( c ) ;
		Point bPos = null, ePos = null ;
		boolean doDraw = true ;
		
		if ( r2.x - gap > r1.x + r1.width )
		{
			bPos = new Point( r1.x + r1.width, r1.y + r1.height / 2 ) ;
			ePos = new Point( r2.x, r2.y + r2.height / 2 ) ;
		}
		else if ( r1.x - gap > r2.x + r2.width )
		{
			bPos = new Point( r1.x, r1.y + r1.height / 2 ) ;
			ePos = new Point( r2.x + r2.width, r2.y + r2.height / 2 ) ;
		}
		else if ( r2.y > r1.y + r1.height )
		{
			bPos = new Point( r1.x + r1.width / 2, r1.y + r1.height ) ;
			ePos = new Point( r2.x + r2.width / 2, r2.y ) ;
		}
		else if ( r1.y > r2.y + r2.height )
		{
			bPos = new Point( r1.x + r1.width / 2, r1.y ) ;
			ePos = new Point( r2.x + r2.width / 2, r2.y + r2.height ) ;
		}
		else
			doDraw = false ;

		if ( doDraw )
		{
			g.drawLine( bPos.x, bPos.y, ePos.x, ePos.y ) ;
			String nrText = "" + nr ;
			FontMetrics metr = getFontMetrics( lineLabelFont ) ;
	        int ascent = metr.getMaxAscent() ;
			int wid = metr.stringWidth( nrText ) ;
			g.setFont( lineLabelFont ) ;
			Point tPos = new Point( ePos.x - (ePos.x - bPos.x) / 5, ePos.y - (ePos.y - bPos.y) / 5 ) ;
			Point aPos = new Point( ePos.x - (ePos.x - bPos.x) / 4, ePos.y - (ePos.y - bPos.y) / 4 ) ;
	        g.drawString( nrText, tPos.x - wid/2, tPos.y ) ;
		}
	}
	
	private DrawInfo drawOne( Graphics g, Describable d, String text, int x, int y, Hashtable positions, Color rcol, Dimension maxSize )
	{
		boolean isUnderEval = ( selectedStackElt < stack.size() - 1 && stack.elementAt( selectedStackElt+1 ) == d ) ;
		Font font = isUnderEval ? underEvalFont : normalFont ;
		
		FontMetrics metr = getFontMetrics( font ) ;
		int wid = metr.stringWidth( text ) ;
		int descent = metr.getDescent() ;
        int ascent = metr.getMaxAscent() ;
        int inset = 3 ;
        
        int w = wid + 2 * inset ;
        int h = descent + ascent + 2 * inset ;
        
        g.setColor( rcol ) ;
        g.fillRect( x, y, w, h ) ;
        g.setColor( Color.black ) ;
        g.drawRect( x, y, w, h ) ;
        if ( isUnderEval )
	        g.drawRect( x+1, y+1, w-2, h-2 ) ;
        g.setFont( font ) ;
        g.drawString( text, x + inset, y + inset + ascent ) ;
        
        maxSize.width = Math.max( maxSize.width, x + w + inset ) ;
        maxSize.height = Math.max( maxSize.height, y + h + inset ) ;
        
        return new DrawInfo( new Rectangle( x, y, w, h ), h ) ;
	}
	
	private DrawInfo drawMany( Graphics g, Describable d, FontMetrics metr, int x, int y, Hashtable<Describable,Rectangle> positions, Dimension maxSize )
	{
		int topY = y ;
		
		DrawInfo parentDI = drawOne( g, d, d.getParentInfo(), x, y, positions, describableColor(d), maxSize ) ;
		Rectangle parentR = parentDI.parentRectangle ;
		positions.put( d, parentR ) ;
		
		int rectHGap = 30 ;
		int rectVGap = 10 ;
		x += parentR.width + rectHGap ;
		
		int childNr = 0 ;
		for ( Enumeration children = d.getChildrenInfo() ; children.hasMoreElements() ; childNr++ )
		{
			Object child = children.nextElement() ;
			DrawInfo childDI ;
			if ( child instanceof Describable )
			{
				Object c = positions.get( child ) ;
				if ( c != null )
				{
					Rectangle childAlreadyDrawnRect = (Rectangle)c ;
					childDI = new DrawInfo( childAlreadyDrawnRect, 0 ) ;
				}
				else
				{
					childDI = drawMany( g, (Describable)child, metr, x, y, positions, maxSize ) ;
				}
			}
			else
			{
				childDI = drawOne( g, null, "" + child, x, y, positions, describableColor(child), maxSize ) ;
			}
			Rectangle childR = childDI.parentRectangle ;
			
			drawLabeledLineBetweenRect( g, parentR, childR, Color.black, childNr ) ;
			y += rectVGap + childDI.spentHeight ;
		}
		
		return new DrawInfo( parentR, Math.max( (y - topY) - rectVGap, parentDI.spentHeight ) ) ;
	}
	
	private void drawStack( Graphics g, Color c )
	{
		for ( int i = 0 ; i < stack.size() ; i++ )
		{
			g.setColor( i == selectedStackElt ? c.darker() : c ) ;
			g.fillRect( stackPos.x + stackEltRect.x, stackPos.y + stackEltRect.y + i * stackEltRect.height, stackEltRect.width, stackEltRect.height ) ;
			g.setColor( Color.black ) ;
			g.drawRect( stackPos.x + stackEltRect.x, stackPos.y + stackEltRect.y + i * stackEltRect.height, stackEltRect.width, stackEltRect.height ) ;
		}
	}
	
	public void paintComponent(Graphics g)
	{
		super.paintComponent(g); //paint background

		Insets insets = getInsets() ;
		Dimension maxSize = getSize() ;
		int currentWidth = getWidth() - insets.left - insets.right;
		int currentHeight = getHeight() - insets.top - insets.bottom;
		
		Font font = getFont() ;
		FontMetrics fontMetrics = getFontMetrics( font ) ;
		
		Hashtable<Describable,Rectangle> positions = new Hashtable<Describable,Rectangle>() ;

		drawStack( g, Color.gray.brighter() ) ;
		Rectangle stackRect = getStackRect() ;
		
		if ( model != null )
			drawMany( g, model, fontMetrics, stackRect.x + stackRect.width + stackPos.x, stackRect.y, positions, maxSize ) ;
		
		if ( maxSize.width > getWidth() || maxSize.height > getHeight() )
		{
			setPreferredSize( maxSize ) ;
			revalidate() ;
		}
	}
	
	//
	// Interface from tracer
	//
	
	public void pushOnStack( Object x )
	{
		stack.push( x ) ;
		selectedStackElt = stack.size() - 1 ;
		setModel( x ) ;
		continueSema.acquire() ;
	}

	public void popFromStack()
	{
		if ( ! stack.empty() )
		{
			stack.pop() ;
			selectedStackElt = stack.size() - 1 ;
			if ( ! stack.empty() )
				setModel( stack.peek() ) ;
			else
				setModel( "empty" ) ;
			continueSema.acquire() ;
		}
	}
	
	public void replaceTopOfStack( Object x )
	{
		stack.setElementAt( x, stack.size()-1 ) ;
		selectedStackElt = stack.size() - 1 ;
		setModel( x ) ;
		continueSema.acquire() ;
	}
	
	//
	// Event handling
	//
	
	public void mouseClicked(java.awt.event.MouseEvent e)
	{
		Rectangle s = getStackRect() ;
		Point p ;
		if ( s.contains( p = e.getPoint() ) )
		{
			selectedStackElt = ( p.y - s.y ) / stackEltRect.height ;
			setModel( stack.elementAt( selectedStackElt ) ) ;
		}
		else
		{
			continueSema.release() ;
		}
	}
	
	public void mouseEntered(java.awt.event.MouseEvent e)
	{
	}
	
	public void mouseExited(java.awt.event.MouseEvent e)
	{
	}
	
	public void mousePressed(java.awt.event.MouseEvent e)
	{
	}
	
	public void mouseReleased(java.awt.event.MouseEvent e)
	{
	}
	
	//
	// Scrollable stuff
	//

	public int getScrollableUnitIncrement(Rectangle visibleRect,
                                                    int orientation,
                                                    int direction)	
	{
		return 50 ;
	}
	
	public int getScrollableBlockIncrement(Rectangle visibleRect,
                                                     int orientation,
                                                     int direction)
	{
		return 200 ;
	}
	
	public boolean getScrollableTracksViewportWidth()
	{
		return false ;
	}
	
	public boolean getScrollableTracksViewportHeight()
	{
		return false ;
	}
	
	public Dimension getPreferredScrollableViewportSize()
	{
		return getPreferredSize() ;
	}
	
}
