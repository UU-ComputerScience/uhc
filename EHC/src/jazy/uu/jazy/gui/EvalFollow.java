/** 
 * EvalFollow.java
 *
 * Title:			Evaluation Follower
 * Description:		GUI to follow evaluation of graph reduction evaluation
 * @author			atze
 */

package uu.jazy.gui;
import javax.swing.*;

public class EvalFollow {
	public EvalFollow() {
		try {
			// For native Look and Feel, uncomment the following code.
			/*
			try {
				UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			} 
			catch (Exception e) { 
			}
			*/
			EvalFollower frame = new EvalFollower();
			frame.initComponents();
			frame.setVisible(true);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}

	// Main entry point
	static public void main(String[] args) {
		new EvalFollow();
	}
	
}
