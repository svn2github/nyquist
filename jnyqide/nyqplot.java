package jnyqide;

import java.io.*;
import java.util.*;
import java.awt.*;
import javax.swing.*;

// Note: Modifications to move plots from a JInternalFrame to a fixed JPanel
// are marked with //JPANEL:

public class NyqPlot
{
    /**
     *  main method takes 1 parameter from command line
     *  @param args name of input file
     */
    public static void plot(String plotFilename, PlotFrame plotFrame) {
	if (plotFrame == null) {
	    System.out.println("null plotFrame in plot");
	    return;
	}
	BufferedReader input = null;

	try {
	    input = new BufferedReader(new FileReader(plotFilename));
	}  catch( FileNotFoundException fnfx ) {
	    System.out.println("File not found: " + plotFilename);
	    return;
	}  // End try/catch block


	/////////////////////////////////////////////////
	// Read File
	/////////////////////////////////////////////////

	Vector dataPoints = new Vector();
	String line;
	StringTokenizer st;
	boolean firsttime = true;

	Double num1 = new Double(0);
	
	double innum1 = 0;
	double innum2 = 0;

	double minx = 0;
	double miny = 0;
	double maxx = 0;
	double maxy = 0;

	System.out.println("plot reading file");
	try {
	    while ((line = input.readLine()) != null) {	 
		//Read another line from the file
		st = new StringTokenizer(line);
		if (st.countTokens() != 2) {
		    System.out.println("input must have two numbers per line");
		    return;
		}
		
		String word1 = st.nextToken();
		String word2 = st.nextToken();
		
		innum1 = num1.valueOf(word1).doubleValue();
		innum2 = num1.valueOf(word2).doubleValue();

		if (firsttime == true) {
		    // initializes mins and maxes to real values
		    minx = innum1;
		    maxx = innum1;
		    miny = innum2;
		    maxy = innum2;
		    firsttime = false;
		}

		if (innum1 < minx){minx = innum1;}
		if (innum1 > maxx){maxx = innum1;}
		if (innum2 < miny){miny = innum2;}
		if (innum2 > maxy){maxy = innum2;}

		Pair currentPair = new Pair(innum1, innum2);
		dataPoints.add(currentPair);
	    } //while ((line = input.readLine()) != null)

	    input.close();  //Close the file

	}  catch( IOException iox ) {
	    System.out.println(iox );
	}  // End try/catch block

	// printData(dataPoints);

	//System.out.println("Min X = " + minx + " Max X = " + maxx);
	//System.out.println("Min Y = " + miny + " Max Y = " + maxy);

	// Plotting stuff
/*
		Color	[] colors = new Color [4];

		colors[0] = Color.red;
		colors[1] = Color.yellow;
		colors[2] = Color.green;
		colors[3] = Color.blue;


		double [][][] graphstuff = new double [1][2][dataPoints.size()];

		for(int i = 0; i < dataPoints.size(); i++){
			graphstuff[0][0][i] = ((Pair) dataPoints.get(i)).getNum1();
			graphstuff[0][1][i] = ((Pair) dataPoints.get(i)).getNum2();
		}


		GRAph myGraph = new GRAph(colors);
		myGraph.graphit(minx,maxx,miny,maxy,graphstuff);
*/

	plotFrame.PlotData(dataPoints, minx, miny, maxx, maxy);
	//JPANEL: JInternalFrame plotFrame = new PlotFrame(jDesktop, dataPoints, minx, miny, maxx, maxy);
	//JPANEL: JPanel plotFrame = new PlotFrame(jDesktop, dataPoints, minx, miny, maxx, maxy);
	//JPANEL: plotFrame.setVisible(true);
    }

    public static void printData(Vector input){

	for(int i = 0; i < input.size(); i++){
	    System.out.println( ((Pair) input.get(i)).getTime() + " " + 
				((Pair) input.get(i)).getValue());
	}
    }
}
