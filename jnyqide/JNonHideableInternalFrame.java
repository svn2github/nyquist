package jnyqide;


import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.JInternalFrame;

/**
 * 
 * An internal frame that cannot be moved above the DesktopPanel.
 * 
 * @author zeyu
 * @version 1.0 (Jan-3-2013)
 */
public class JNonHideableInternalFrame extends JInternalFrame {

	JNonHideableInternalFrame thisFrame;
	
	private void _initialize() {
		thisFrame = this;
		thisFrame.addComponentListener(new ComponentAdapter() {
			@Override
			public void componentMoved(ComponentEvent arg0) {
				if (thisFrame.getLocation().y < 1) {
					thisFrame.setLocation(thisFrame.getLocation().x, 1);
				}
			}
		});
	}

	public JNonHideableInternalFrame() {
		super();
		_initialize();
	}
	
	public JNonHideableInternalFrame(String name) {
		super(name);
		_initialize();
	}
	
	public JNonHideableInternalFrame(String name, boolean someMeans) {
		super(name, someMeans);
		_initialize();
	}
	


}
