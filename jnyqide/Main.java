package jnyqide;


import javax.swing.UIManager;
import java.awt.*;
import java.util.Locale;

/**
 * @author unascribed
 * @version 1.01
 * 
 * Revised by Zeyu Jan-3-2013
 */

public class Main {
	boolean packFrame = false;

	// Construct the application
	public Main() {
		String osName = System.getProperty("os.name");
		System.out.println(osName);
		Locale loc = Locale.getDefault();
		System.out.println("lLocale is " + loc.toString());
		if (osName.startsWith("Linux")) {
			// motif style has some extra buttons to iconify internal frames
			// but this obscures windows, and metal looks better anyway
			try {
				UIManager
						.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
			} catch (Exception e) {
				System.out.println(e);
			}
		}
		MainFrame frame = new MainFrame();
		// Validate frames that have preset sizes
		// Pack frames that have useful preferred size info, e.g. from their
		// layout
		if (packFrame) {
			frame.pack();
		} else {
			frame.validate();
		}
		// Center the window
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = frame.getSize();
		// subtract 20 here because otherwise Mac OS X frames will be too tall
		// to access resize nbutton
		//System.out.print("initial frame height ");
		//System.out.println(frameSize.height);
		//System.out.print("screen height ");
		//System.out.println(screenSize.height);
		if (frameSize.height > screenSize.height) {
			frameSize.height = screenSize.height - 40;
		}
		if (frameSize.width > screenSize.width) {
			frameSize.width = screenSize.width;
		}
		//System.out.print("finall frame height ");
		//System.out.println(frameSize.height);
		frame.setSize(frameSize);
		frame.setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		frame.setVisible(true);
		frame.tileCompletion();
	}

	// Main method
	public static void main(String[] args) {
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			e.printStackTrace();
		}
		new Main();
	}
}
