package jnyqide;

import javax.swing.*;
import java.awt.*;
import java.util.*;
/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

// Note: originally this code put plots in JInternalFrames. The code to
// support this is commented out and marked with "JPANEL:"

public class PlotFrame extends JPanel { //JPANEL: JInternalFrame {

    double minX, minY, height, width; // range of data values to be plotted
    double minXlabel, minYlabel, xSpacing, ySpacing;
    double minLabel, spacing; // computed by AxisLayout
    Vector data;
    PlotMouseAdapter mouse;
    Graphics g; // the whole thing, including labels
    Graphics gGraph; // the graph image without labels
    Image buffer;
    Image graphBuffer;
    int bufferHeight; // size of plot with labels
    int bufferWidth;  // size of plot with labels
    //JPANEL: int offsetWindowSide = 3; // To clear the side bar
    //JPANEL: int offsetWindowTop = 21; // To clear the title bar
    int offsetX = 40; // how wide is label space to left of gGraph
    int offsetRight = 10; // how wide is space to right of gGraph
    int offsetY = 5; // how high is space above of gGraph
    int dotSize = 3;  // diameter of data points on the plot image
    int textHeight = 20;
    // width of plot area (without labels)
    int graphBufferWidth;
    // height of plot area (without labels)
    int graphBufferHeight;
    int axisMinSpacing = 30;
    //JPANEL: int gadgetHeight = 10;    
    
    public void CalcBufferSize()
    {
        graphBufferWidth = bufferWidth - offsetX - offsetRight;
        graphBufferWidth = Math.max(10, graphBufferWidth);
        graphBufferHeight = bufferHeight - textHeight; //JPANEL: - gadgetHeight;
        graphBufferHeight = Math.max(10, graphBufferHeight);
    }
    
    
    public PlotFrame(JPanel jPanel) {
	minX = 0;
        minY = -1;
	width = 1;
	height = 2;
	data = new Vector();
	data.add(new Pair(0.0, 0.0));
        data.add(new Pair(1.0, 0.0));
	mouse = new PlotMouseAdapter(this);
	setPreferredSize(new Dimension(400, 100));
	// setResizable(true);
	addMouseMotionListener(mouse);
	addMouseListener(mouse);
	setVisible(true);
	jPanel.add(this, BorderLayout.CENTER);
	CalcBufferSize();

	renderGraph();
    }

    public void PlotData(Vector d, double minx, double miny, 
		    double maxx, double maxy) {
	//System.out.println("PlotData called.");
	minX = minx;
        minY = miny;
        height = maxy - miny;
        width = maxx - minx;
        data = d;
	//System.out.println("PlotData calling repaint");
	renderGraph();
	repaint();
    }

	

    //JPANEL: This is old code to put a plot in a JInternalFrame
    //    public PlotFrame(JDesktopPane jDesktop, Vector d, double minx, double miny, 
    //                     double maxx, double maxy) {
    //        minX = minx;
    //        minY = miny;
    //        height = maxy - miny;
    //        width = maxx - minx;
    //        data = d;
    //        mouse = new PlotMouseAdapter(this);
    //
    //        setSize(new Dimension(720, 520));
    //        //JPANEL: setResizable(true);
    //        //JPANEL: this.setClosable(true);
    //        //JPANEL: this.setIconifiable(true);
    //
    //        addMouseMotionListener(mouse);
    //        addMouseListener(mouse);
    //
    //        //JPANEL: setTitle("Plot");
    //        setVisible(true);
    //        jDesktop.add(this);
    //        CalcBufferSize();
    //        
    //        renderGraph();
    //    }
    //
    //    private int degree(double n)
    //    {
    //        int deg = 0;
    //        while (n < 10) {
    //            n *= 10.0;
    //            deg++;
    //        }
    //        return deg;
    //    }
    // (END JPANEL:)

    public void AxisLayout(int buffersize, int axisMinSpacing, 
                           double minval, double range)
    {
        if (range < 0.000001) range = 1.0; // convert range of zero
        // compute axis layout
        // first, figure out about how many x labels there will be
        int nlabels = buffersize / axisMinSpacing;
        if (nlabels <= 0) nlabels = 1; // avoid divide by zero
        // now figure out what that is in terms of the data
        spacing = range / nlabels;
        // now increase xSpacing to nearest multiple of 0.1, 0.2, 0.5, ...
        // first, get xSpacing between 0.1 and 1.0
        double tens = 1.0;
        //System.out.print("spacing "); System.out.println(spacing);
        while (spacing < 0.1) {
            spacing = spacing * 10.0;
            tens = tens * 0.1;
        }
        while (spacing > 1.0) {
            spacing = spacing * 0.1;
            tens = tens * 10.0;
        }
        // now 0.1 <= xSpacing <= 1.0, and xSpacing * tens is original value
        // adjust xSpacing up to 0.2, 0.5, or 1.0
        if (spacing < 0.2) spacing = 0.2;
        else if (spacing < 0.5) spacing = 0.5;
        else spacing = 1.0;
        // now translate back to original power of ten
        spacing = spacing * tens;
        // figure out minXlabel, the first multiple of xSpacing gtr than minX
        minLabel = ((int) (minval / spacing)) * spacing;
        // if minXlabel is close to minX, ok, otherwise increment
        if (minLabel < minval - 0.5 * width / graphBufferWidth) {
            minLabel = minLabel + spacing;
        // similar correction if minXlabel is negative
        } else if (minLabel - spacing >= minval - 0.5 * width / 
                                                  graphBufferWidth) {
            minLabel = minLabel - spacing;
        }
        //System.out.print("minLabel "); System.out.print(minLabel);
        //System.out.print(" spacing "); System.out.println(spacing);
    }

    public void renderGraph()
    {
        //System.out.println("renderGraph");
        if (gGraph == null) {
	    //System.out.print("creating graphbuffer"); 
	    //System.out.print(graphBufferWidth); System.out.print(", ");
	    //System.out.println(graphBufferHeight);
            graphBuffer = createImage(graphBufferWidth, graphBufferHeight);
	    //System.out.println(graphBuffer);
	    if (graphBuffer == null) return; // Why does this fail?
            gGraph = graphBuffer.getGraphics();
        }

        gGraph.setColor(new Color(220, 220, 220));
        gGraph.fillRect(0, 0, graphBufferWidth, graphBufferHeight);

	// draw graph as lines first, then overlay circles
        ListIterator iter = data.listIterator();
	Pair prev = (Pair) iter.next();
        int x1 = (int) ((prev.getTime() - minX) / width * graphBufferWidth);
        int y1 = (int) ((1.0 - ((prev.getValue() - minY) / height)) *
			   graphBufferHeight);
        Color pointColor = new Color(220, 220, 0);
        while (iter.hasNext()) {
            Pair p = (Pair) iter.next();
            gGraph.setColor(Color.black);
	    int x2 = (int) ((p.getTime() - minX) / width * graphBufferWidth);
            int y2 = (int) ((1.0 - ((p.getValue() - minY) / height)) *
			   graphBufferHeight);
	    gGraph.drawLine(x1, y1, x2, y2);
	    gGraph.setColor(pointColor);
	    x1 = x1 - dotSize / 2;
	    y1 = y1 - dotSize / 2;
            gGraph.fillOval(x1, y1, dotSize, dotSize);
            gGraph.setColor(Color.black);
            gGraph.drawOval(x1, y1, dotSize, dotSize);
	    x1 = x2;
	    y1 = y2;
        }
	// draw the last point
	    gGraph.setColor(pointColor);
	    x1 = x1 - dotSize / 2;
	    y1 = y1 - dotSize / 2;
            gGraph.fillOval(x1, y1, dotSize, dotSize);
            gGraph.setColor(Color.black);
            gGraph.drawOval(x1, y1, dotSize, dotSize);

        //System.out.println("calling AxisLayout");
        AxisLayout(graphBufferWidth, axisMinSpacing, minX, width);
        //System.out.print("return from AxisLayout");
        minXlabel = minLabel;
        xSpacing = spacing;
        
        for (double x = minXlabel; x < minX + width; x += xSpacing) {
            //System.out.println(x);
            int axisX = (int) (((x - minX) / width) * graphBufferWidth + 0.5);
            gGraph.setColor(new Color(170, 170, 170));
            gGraph.drawLine(axisX, 0, axisX, graphBufferHeight );
        }

        AxisLayout(graphBufferHeight, axisMinSpacing, minY, height);
        minYlabel = minLabel;
        ySpacing = spacing;
        
        for (double y = minYlabel; y < minY + height; y += ySpacing) {
            int axisY = (int) ((1.0 - ((y - minY) / height)) * 
                               graphBufferHeight + 0.5);
            gGraph.setColor(new Color(170, 170, 170));
            gGraph.drawLine(0, axisY, graphBufferWidth, axisY );
        }
    }

    public void paint(Graphics g2) {
        //    Graphics g = getContentPane().getGraphics();
        //System.out.println("Painting...");
        // because the window can be resized, check if we have the right width and height
	//System.out.print("graph dim "); System.out.print(getWidth());
	//System.out.print(", "); System.out.println(getHeight());
 
        if ((getHeight() != bufferHeight) ||
            (getWidth() != bufferWidth)) {
            g = null; // delete any existing double buffer to force an update
            gGraph = null; // the plot image is here, update this too
            //System.out.println("recalculate width and height");
        }

        if (g == null) { // create matching image buffer for double buffer update
            bufferHeight = getHeight();
            bufferWidth = getWidth();
            buffer = createImage(bufferWidth, bufferHeight);
            g = buffer.getGraphics();
            CalcBufferSize();
        }
        
        if (gGraph == null) {
            renderGraph();
        }

        if (g2 != null) {
            g.setColor(new Color(150, 150, 150));
            g.fillRect(0, 0, bufferWidth, bufferHeight); //JPANEL: - gadgetHeight);

            if (gGraph != null) {
                g.drawImage(graphBuffer, offsetX, offsetY, offsetX + graphBufferWidth,
                            offsetY + graphBufferHeight, 0, 0, 
                            graphBufferWidth, graphBufferHeight, this);
            }
    
            for (double x = minXlabel; x < minX + width; x += xSpacing) {
                int axisX = (int) (((x - minX) / width) * graphBufferWidth + 
                                   0.5);
                g.setColor(Color.black);
                String s = Double.toString(Math.round(x * 1000) / 1000.0);
                g.drawString(s, offsetX + axisX - s.length() * 2, 
                             offsetY + graphBufferHeight + textHeight - 8);
            }

            for (double y = minYlabel; y < minY + height; y += ySpacing) {
                int axisY = (int) (((y - minY) / height) * 
                                   graphBufferHeight + 0.5);
                g.setColor(Color.black);
                String s = Double.toString(Math.round(y * 1000) / 1000.0);
                g.drawString(s, offsetX - s.length() * 5 - 5, 
                             offsetY + graphBufferHeight - axisY);
           }
        }
        super.paint(g2);
        //JPANEL: g2.drawImage(buffer, offsetWindowSide, offsetWindowTop, this );
	g2.drawImage(buffer, 0, 0, this );
    }
}
