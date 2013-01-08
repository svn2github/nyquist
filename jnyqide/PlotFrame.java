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
	int numXlines, numYlines; // number of lines to be plotted
    double minLabel, spacing; // computed by AxisLayout
	int numLines; // computed by AxisLayout
	double xExtent, yExtent; // value range of plot area
    Vector<Pair> data;
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
		xExtent = 1;
		yExtent = 2;
        data = new Vector<Pair>();
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

    public void PlotData(Vector<Pair> d, double minx, double miny, 
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

	// do label layouts for x and y axes. Returns values by setting
	// spacing, minLabel, and numLines
	//
    public void AxisLayout(int buffersize, int axisMinSpacing, 
                           double minval, double range)
    {
        if (range < 0.000001) range = 1.0; // convert range of zero
        // compute axis layout
        // first, figure out about how many label divisions there can be
        int nlabels = buffersize / axisMinSpacing;
        if (nlabels <= 0) nlabels = 1; // avoid divide by zero
        // now figure out what that is in terms of the data
        // spacing is a proposed spacing for labels in terms of actual data
        spacing = range / nlabels;
        // now increase spacing to nearest multiple of 0.1, 0.2, 0.5, ...
        // first, get spacing between 0.1 and 1.0
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
        // now 0.1 <= spacing <= 1.0, and spacing * tens is original value
        // adjust spacing up to 0.2, 0.5, or 1.0
		// this will result in greater than minimum spacing
        if (spacing < 0.2) spacing = 0.2;
        else if (spacing < 0.5) spacing = 0.5;
        else spacing = 1.0;
        // now translate back to original power of ten
        spacing = spacing * tens;
        // figure out minLabel, the first multiple of spacing less or equal to minval
        // the extra 0.1% added by multiplying by 1.001 is there so that if
        // minval is already very close to a multiple of spacing, it is used
        // Sometimes this happens due to floating point rounding even when
        // minval is an exact multiple of spacing.
        minLabel = ((int) ((minval / spacing) * 1.001)) * spacing;
        // (int) rounds toward zero, so if minval is negative, subtract one
        if (minLabel > minval + spacing * 0.0001) minLabel -= spacing;
        // since spacing may have increased, we may not need so many divisions to
        // span the range of the data
        numLines = (int) ((minval + range - minLabel) / spacing);
        // increase by one in case we rounded down
        if (numLines < (minval + range - minLabel) / spacing) numLines += 1;
        // increase by one again. E.g. if the graph is divided in two, there are
        // three lines. (Always one more line than division.)
        numLines += 1;
        System.out.print("minLabel "); System.out.print(minLabel);
        System.out.print(" spacing "); System.out.println(spacing);
        System.out.print(" numLines " + numLines);
    }

    public void renderGraph()
    {
        System.out.println("renderGraph");
        if (gGraph == null) {
            System.out.print("creating graphbuffer"); 
            System.out.print(graphBufferWidth); System.out.print(", ");
            System.out.println(graphBufferHeight);
            graphBuffer = createImage(graphBufferWidth, graphBufferHeight);
            //System.out.println(graphBuffer);
            if (graphBuffer == null) return; // Why does this fail?
            gGraph = graphBuffer.getGraphics();
        }

        gGraph.setColor(new Color(220, 220, 220));
        gGraph.fillRect(0, 0, graphBufferWidth, graphBufferHeight);

        System.out.println("calling AxisLayout");
        AxisLayout(graphBufferWidth, axisMinSpacing, minX, width);
        //System.out.print("return from AxisLayout");
		xExtent = spacing * (numLines - 1);
        minXlabel = minLabel;
        xSpacing = spacing;
		numXlines = numLines;
		
        AxisLayout(graphBufferHeight, axisMinSpacing, minY, height);
		yExtent = spacing * (numLines - 1);
        minYlabel = minLabel;
        ySpacing = spacing;
        numYlines = numLines;
		
        // draw graph as lines first, then overlay circles
        ListIterator iter = data.listIterator();
        Pair prev = (Pair) iter.next();
        int x1 = (int) ((prev.getTime() - minXlabel) / xExtent * graphBufferWidth);
        int y1 = (int) ((1.0 - ((prev.getValue() - minYlabel) / yExtent)) *
                        graphBufferHeight);
        Color pointColor = new Color(220, 220, 0);
        while (iter.hasNext()) {
            Pair p = (Pair) iter.next();
            gGraph.setColor(Color.black);
            int x2 = (int) ((p.getTime() - minXlabel) / xExtent * graphBufferWidth);
            int y2 = (int) ((1.0 - ((p.getValue() - minYlabel) / yExtent)) *
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

        double x = minXlabel;
        for (int i = 0; i < numXlines; i++) {
            //System.out.println(x);
            int axisX = (int) (((x - minXlabel) / ((numXlines - 1) * xSpacing)) * 
			                   graphBufferWidth + 0.5);
            gGraph.setColor(new Color(170, 170, 170));
            gGraph.drawLine(axisX, 0, axisX, graphBufferHeight );
			x += xSpacing;
        }

        double y = minYlabel;
        for (int i = 0; i < numYlines; i++) {
		//double y = minYlabel; y < minYlabel + height; y += ySpacing) {
            int axisY = (int) ((1.0 - ((y - minYlabel) / ((numYlines - 1) * ySpacing))) * 
                               graphBufferHeight + 0.5);
            gGraph.setColor(new Color(170, 170, 170));
            gGraph.drawLine(0, axisY, graphBufferWidth, axisY );
			y += ySpacing;
        }
    }

    public void paint(Graphics g2) {
        //    Graphics g = getContentPane().getGraphics();
        System.out.println("Painting...");
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
    
			double x = minXlabel;
            for (int i = 0; i < numXlines; i++) {
                int axisX = (int) (((x - minXlabel) / (xSpacing * (numXlines - 1))) * 
				                   graphBufferWidth + 0.5);
                g.setColor(Color.black);
                String s = Double.toString(Math.round(x * 1000) / 1000.0);
                g.drawString(s, offsetX + axisX - s.length() * 2 - 
				                (i == numXlines - 1 ? s.length() * 2 : 0), 
                             offsetY + graphBufferHeight + textHeight - 8);
				x += xSpacing;
            }

		    double y = minYlabel;
            for (int i = 0; i < numYlines; i++) {
                int axisY = (int) (((y - minYlabel) / (ySpacing * (numYlines - 1))) * 
                                   graphBufferHeight + 0.5);
                g.setColor(Color.black);
                String s = Double.toString(Math.round(y * 1000) / 1000.0);
				// note: offset top string to fit within graphics area
                g.drawString(s, offsetX - s.length() * 5 - 5, 
                                offsetY + graphBufferHeight - axisY +
								(i == numYlines - 1 ? textHeight - 8 : 0));
				y += ySpacing;
           }
        }
        super.paint(g2);
        //JPANEL: g2.drawImage(buffer, offsetWindowSide, offsetWindowTop, this );
        g2.drawImage(buffer, 0, 0, this );
    }
}
