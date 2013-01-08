package jnyqide;


import javax.swing.event.MouseInputAdapter;
import java.awt.event.MouseEvent;
import javax.swing.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class PlotMouseAdapter extends MouseInputAdapter {

    int dx, dy;
    int startx, starty;
    int shiftx, shifty;
    boolean shifting;
    PlotFrame frame;

    public PlotMouseAdapter(PlotFrame f)
    {
        dx = 0;
        dy = 0;
        shiftx = 0;
        shifty = 0;
        shifting = false;
        frame = f;
    }

    public void mouseDragged(MouseEvent e)
    {
        System.out.println("mouseDragged");
        if (!shifting) {
            shifting = true;
            startx = e.getX();
            starty = e.getY();
        }
        dx = e.getX() - startx;
        dy = e.getY() - starty;
        frame.repaint();
    }

    public void mouseClicked( MouseEvent e )
    {
    }

    public void mouseReleased( MouseEvent e )
    {
        shiftx += dx;
        shifty += dy;
        shifting = false;
    }

    public int getShiftX()
    {
        return shiftx+dx;
    }

    public int getShiftY()
    {
        return shifty+dy;
    }
}