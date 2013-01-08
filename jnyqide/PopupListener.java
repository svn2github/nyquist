package jnyqide;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.event.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.undo.*;
import jnyqide.*;

class PopupListener extends MouseAdapter {
	JPopupMenu popup;
	JTextPane pane;

	PopupListener(JPopupMenu popupMenu, JTextPane textPane)
	{
		popup = popupMenu;
		pane = textPane;
	}

	public void mousePressed(MouseEvent e)
	{
		maybeShowPopup(e);
	}

	public void mouseReleased(MouseEvent e)
	{
		maybeShowPopup(e);
	}

	private void maybeShowPopup(MouseEvent e)
	{
		if (e.isPopupTrigger()) {
			MenuElement[] items = popup.getSubElements();
            for (int i = 0; i < items.length; i++) {
                String name = items[i].getComponent().getName();
                if (name.equals("context copy") || name.equals("context cut")) {
                    ((JMenuItem) items[i]).setEnabled(pane.getSelectedText() !=
                                                      null);
                }
            }
			popup.show(e.getComponent(), e.getX(), e.getY());
		}
	}
}