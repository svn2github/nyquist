package jnyqide;

import java.io.*;
import javax.swing.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class NyquistThread extends Thread {

    Process myProcess;
    InputStream stdout;
    InputStreamReader isrStdout;
    BufferedReader brStdout;
    OutputStream stdin;
    byte b[];
//  JTextPane textIn;
    JTextArea textOut;
    Runnable update;
    String plotFilename;
    int plotNow;
    PlotFrame plotFrame;
    MainFrame mainFrame; // used to find current directory

    public NyquistThread() {
    }

/*  public void setInputArea( JTextPane in )
  {
    textIn = in;
  }*/

    public void start(JTextArea out, Runnable u, 
              PlotFrame plot, MainFrame main) {
        textOut = out;
        update = u;
        plotFrame = plot;
        mainFrame = main;

        try {
            String[] envp = {"XLISPPATH=./lib:./runtime", "USER=nyquist"};
            myProcess = Runtime.getRuntime().exec( "./ny", envp );
            stdout = myProcess.getInputStream();
            isrStdout = new InputStreamReader(stdout);
            brStdout = new BufferedReader(isrStdout);
            stdin = myProcess.getOutputStream();
        }
        catch( Exception e2 ) { System.out.println(e2); }

        b = new byte[1000];
        super.start();
    }

    public void run() {
        StringBuffer buffer = new StringBuffer(256);
        int buffer_index = 0;
        // this is tricky -- we want to accumulate lines to test
        // for plot commands, but we want to append chars to 
        // textOut even if there is no end-of-line.
        // 
        // We're going to flush the buffer after every line.
        // That will be expensive if lines are very short, but
        // that's pretty rare. So we'll loop until we either
        // accumulate a line or there are no characters ready.
        // To avoid re-appending characters to textOut, use
        // buffer_index to remember how many characters were
        // output from the current line.
        //
        // Algorithm:
        //   accumulate chars while input is ready and newline not found
        //   if we have a newline, test for plot command
        //   append characters to text box
        //   if we have a newline, reset the buffer

        String line = null;
        while (true) {
            try {
                // block until you read at least one character
                // System.out.println("block on char");
                char c = (char) isrStdout.read();
                // System.out.println("got char");
                buffer.append(c);
                while (isrStdout.ready() && c != '\n') {
                    // System.out.println("block on char in while");
                    c = (char) isrStdout.read();
                    // System.out.println("got it");
                    buffer.append(c);
                }
                if (c == '\n') {
                    line = new String(buffer);
                    testForPlot(line);
                }
                textOut.append(buffer.substring(buffer_index));
                buffer_index = buffer.length();
                if (line != null) { // end of line, reset the buffer
                    line = null;
                    buffer.setLength(0);
                    buffer_index = 0;
                    // limit number of lines in the textOut box
                    int lines = textOut.getLineCount();
                    if (lines > 5000) {
                        // take out about 100 lines at a time:
                        textOut.replaceRange("", 0, 
                            textOut.getLineStartOffset(lines - 4900));
                    }
                }
                // cause the scroll to go to the end of the new text
                SwingUtilities.invokeLater(update);
            } catch (Throwable t) {
                t.printStackTrace();
            }
    /*      
            try {

            while( stdout.available() > 0 ) {
                int howMany = stdout.read(b, 0, stdout.available());
                String output = new String(b, 0, howMany);
                testForPlot(output);
                textOut.append(output);

                // cause the scroll to go to the end of new text
                // System.out.println(output);
                SwingUtilities.invokeLater(update);
            }
        
            try {
                Thread.sleep((int)( Math.random()*300 ));
            } catch ( InterruptedException e ) {
                e.printStackTrace();
            }
            }
            catch( Exception e ) { System.out.println(e); }
     */
        }
    }

    public void testForPlot(String output) {
        int iNameStart;
        int iNameStop;
        //System.out.print("plot's text output:");
        //System.out.println(output);
        if (plotNow == 0) {
            iNameStart = output.indexOf("s-plot: writing ");
            if (iNameStart >= 0) {
            iNameStop = output.indexOf(" ...");
            plotFilename = output.substring(iNameStart + 16, iNameStop);
            if (!mainFrame.currentDir.equals("")) {
                plotFilename = mainFrame.currentDir + plotFilename;
            }
            // System.out.println("file:" + plotFilename + ":");
            plotNow = 1;
            }
        }
        // this is not ...else if... because output can have many lines
            if (plotNow == 1) {
            iNameStart = output.indexOf(" points from");
            // now, the file is written and closed so we can open it
            if (iNameStart >= 0) {
            // System.out.println("Calling NyqPlot.plot");
            NyqPlot.plot(plotFilename, plotFrame);
            plotNow = 0;
            }
        }
    }

    public OutputStream getStdin() {
    return stdin;
    }

    public void sendInput( String s )
    {
    stdin = myProcess.getOutputStream();
    // System.out.println(s);
//    if( s.equals("(exit)\n") )
 //     System.exit(0);
 //   s.concat("\n"); needed???
    try{
        stdin.write( s.getBytes() );
        stdin.flush();
    }
    catch( Exception e ) { System.out.println(e); }
    }
}









