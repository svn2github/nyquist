package jnyqide;


import java.awt.*;
import javax.swing.*;
import javax.swing.text.html.*;
import javax.swing.event.*;
import java.net.URL;

/**
 * 
 * Internal mini Browser
 * 
 * Changes: Replace JInternal frame by new version
 * 
 * @author original_author Revised by Zeyu Jin (Jan-03-2013)
 * 
 */
public class MiniBrowser extends JNonHiddenableInternalFrame {
	JEditorPane urlPane;

	class Hyperactive implements HyperlinkListener {
		public void hyperlinkUpdate(HyperlinkEvent e) {
			if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
				JEditorPane pane = (JEditorPane) e.getSource();
				if (e instanceof HTMLFrameHyperlinkEvent) {
					HTMLFrameHyperlinkEvent evt = (HTMLFrameHyperlinkEvent) e;
					// System.out.println("Target = " + evt.getTarget());
					HTMLDocument doc = (HTMLDocument) pane.getDocument();
					doc.processHTMLFrameHyperlinkEvent(evt);
				} else {
					try {
						urlPane.setPage(e.getURL());
					} catch (Throwable t) {
						t.printStackTrace();
					}
				}
			}
		}
	}

	public MiniBrowser(String title) {
		super(title);
		urlPane = new JEditorPane();
		urlPane.setEditable(false);
		urlPane.addHyperlinkListener(new Hyperactive());
		JScrollPane sp = new JScrollPane(urlPane);
		sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		setVisible(false);
		setClosable(true);
		getContentPane().add(sp);
		setResizable(true);
		setPreferredSize(new Dimension(400, 400));
		setLocation(50, 100);
		setSize(700, 400);
		setDefaultCloseOperation(HIDE_ON_CLOSE);
	}

	public void setPage(String url) {
		try {
			// hard-wired URL for testing only!:
			// System.out.println("setPage actual input: " + url);
			// url = "file://doc/nyquist/doc/part8.html";
			URL u = new URL(url);
			System.out.println("MiniBrowser.setPage: " + u);
			urlPane.setPage(u);
		} catch (Exception ex) {
			System.out.println("Exception from urlPane.setPage: " + ex);
		}
	}
}
