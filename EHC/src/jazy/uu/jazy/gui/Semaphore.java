package uu.jazy.gui ;

import java.util.* ;
import java.io.* ;

/**
 * Semaphore
 */
public class Semaphore
{
	private int counter;

	public Semaphore()
	{
		this(0);
	}

	public Semaphore(int i)
	{
		if (i < 0) throw new IllegalArgumentException(i + " < 0");
		counter = i;
	}

	/**
	 * Increments internal counter, possibly awakening a thread
	 * wait()ing in acquire().
	 */
	public synchronized void release()
	{
		if (counter == 0) {
			this.notify();
		}
		counter++;
	}

	/**
	 * Decrements internal counter, blocking if the counter is already
	 * zero.
	 */
	public synchronized void acquire()
	{
		try
		{
			while (counter == 0) {
				this.wait();
			}
		}
		catch ( InterruptedException ex )
		{
		}
		counter--;
	}
}
