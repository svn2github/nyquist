package jnyqide;

import javax.swing.text.*;
import javax.swing.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class SyntaxThread extends Thread {

    boolean running = true;
    boolean evenParens = true;
    public boolean needsUpdate = false;
    int updatePos = 0, updatePos2 = 0;
    DefaultStyledDocument doc;
    JScrollPane scroll;
    JTextPane pane;
    public int blinkx = -1;
    public int blinkCount = 0;

    public SyntaxThread(JScrollPane s,
			JTextPane p,
			DefaultStyledDocument d) {
	super();
	doc = d;
	scroll = s;
	pane = p;
    }
    
    public void blink(int loc)
    {
	blinkx = loc;
	blinkCount = 30;
    }

    public void run() {
	int height = 0; 
	int width = 0;
	int x = 0;
	int y = 0;
	while (running) {
	    // System.out.println("Thread runs " + getName());
	    int h = scroll.getViewport().getExtentSize().height;
	    int w = scroll.getViewport().getExtentSize().width;
	    int newx = scroll.getViewport().getViewPosition().x;
	    int newy = scroll.getViewport().getViewPosition().y;
	    if (h != height || w != width || newx != x || newy != y) {
		needsUpdate = true;
		height = h; width = w; x = newx; y = newy;
		update();
	    }
	    if (needsUpdate) {
		needsUpdate = false;
		evenParens = TextColor.format(pane, doc, this, 
					      x, y , width, height);
	    }
            try {
		sleep((int)(100));
	    } catch (InterruptedException e) {}
	    int n = blinkCount;
	    if (n >= 0) blinkCount = n - 1;
	    if (n == 0) needsUpdate = true;
	}
	System.out.println("DONE! " + getName());
    }
    
    public void update() {
	needsUpdate = true;
    }
    
    public boolean isParensEven()
    {
	return evenParens;
    }
}
