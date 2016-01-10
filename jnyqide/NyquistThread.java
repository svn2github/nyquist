package jnyqide;


import java.io.*;
import javax.swing.*;

public class NyquistThread extends Thread {
    public Process myProcess;
    public boolean nyquist_is_running;
    InputStream stdout;
    InputStreamReader isrStdout;
    BufferedReader brStdout;
    OutputStream stdin;
    byte b[];
    JTextArea textOut;
    Runnable update;
    String plotFilename;
    boolean plotNow = false;
    boolean envelopeNow = false;
    boolean editNow = false;
    boolean eqNow = false;
    boolean preferenceDataSent = false;
    boolean initialLoad = false;
    String envData;
    String eqData;
    PlotFrame plotFrame;
    MainFrame mainFrame; // used to find current directory
    String soundBrowser; // path for sound browser
    String scoreEditFileName;

    public NyquistThread() {
    }

    public String StringFromFile(String filename, String default_string) {
    try {
        BufferedReader br = new BufferedReader(new FileReader(filename));
        default_string = br.readLine();
        br.close();
    } catch (Exception e) {
        // do nothing
    }
        return default_string;
    }

    public void start(JTextArea out, Runnable u, 
              PlotFrame plot, MainFrame main) {
        textOut = out;
        update = u;
        plotFrame = plot;
        mainFrame = main;

        try {
            // Get the current working directory where we can find lib and runtime
            // get the "." file present in all directories
            //java.io.File f = new java.io.File(".");
            // get the absolute path to the "." file
            //String cwd2 = f.getAbsolutePath();
            // return the absolute path minus the "."
            //cwd2 = cwd2.substring(0, cwd2.length() - 2);
            // the code above works, but the following is simpler:

            // find full path to instruments.txt
            String cwd = main.currentDir;
            if (cwd.equals("")) {
                cwd = System.getProperty("user.dir");
            }
            soundBrowser = main.nyquistDir + "lib/instruments.txt";
            System.out.println("soundBrowser file is " + soundBrowser);

            // build XLISPPATH environment specification
            String path = System.getenv("XLISPPATH"); // use getenv
            if (path == null) { // getenv failed, use a default setting
                path = cwd + "/lib/;" + cwd + "/runtime/;" + cwd + "/demos/;";
            }
            // if xlisppath file exists, use it instead
            path = "XLISPPATH=" + StringFromFile("xlisppath", path);
            System.out.println("XLISPPATH will be: " + path);
            
            // build TEMP environment specification
            String temp = System.getenv("TEMP"); // use getenv
            if (temp == null) { // getenv failed, use a default setting
                temp = "./";
            }
            // if temp file exists, use it instead
            temp = "TEMP=" + StringFromFile("temp", temp);

            // construct SystemRoot for environment from file
            String systemroot = StringFromFile("systemroot", "SystemRoot=C:/windows");
            
            // See if we can get the USER
            String user = System.getenv("USER");
            if (user == null) user = System.getenv("USERNAME");
            if (user == null) user = "";
            user = StringFromFile("user", user); // file value overrides all
            if (user.equals("")) user = "IGNORE="; // default value
            else user = "USER=" + user;
            
            // Construct the environment for nyquist subprocess
            System.out.println(path);
            System.out.println(user);
            System.out.println(temp);
            System.out.println(systemroot);

            // make environment from 3 strings
            String[] envp = {path, user, systemroot, temp};
            
            File cwdFile = new File(main.nyquistDir);
            try {
                myProcess = Runtime.getRuntime().exec(main.currentDir + "ny",
                                                      envp, cwdFile);
            } catch (Exception e3) {
                System.out.println("no " + main.currentDir + "ny found" +
                                   ", trying nyquist");
                try {
                    myProcess = Runtime.getRuntime().exec(
                            main.currentDir + "nyquist", envp, cwdFile);
                } catch (Exception e4) {
                    System.out.println("no " + main.currentDir +
                                       "nyquist found, trying ny");
                    // try using PATH to find ny (for linux systems where
                    // ny is installed)
                    myProcess = Runtime.getRuntime().exec("ny", envp, cwdFile);
                    // if this one fails, we'll take the exception below
                }
            }
            System.out.print("myProcess: " + myProcess);
            
            stdout = myProcess.getInputStream();
            isrStdout = new InputStreamReader(stdout);
            brStdout = new BufferedReader(isrStdout);
            stdin = myProcess.getOutputStream();
            stdin.write(6); // control-f turns off console echo for Mac and Linux
            stdin.flush();
        }
        catch( Exception e2 ) { System.out.println(e2); }
        
        b = new byte[1000];
        if (brStdout != null) {
            super.start();
        } else {
            textOut.append("Nyquist (ny or nyquist) not found or could not be started.");
        }
    }
    
    public void run() {
        final int max_buf_len = 256;
        StringBuffer buffer = new StringBuffer(max_buf_len);
        int buffer_index = 0;
        nyquist_is_running = true;
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
        //String debug_strings[] = new String[100];
        //int debug_strings_x = 0;
        while (nyquist_is_running) {
            try {
                // initialLoad is true when we see text that indicates
                // Nyquist has just started. But we want to wait for the
                // first prompt. We do this by delaying this process a bit
                // hoping that Nyquist will finish output and issue the
                // prompt. Then we process the input until we are about to
                // block, meaning that (maybe) Nyquist has finished output
                // and is waiting at the prompt.
                if (initialLoad && !brStdout.ready()) {
                    mainFrame.sendPreferenceData();
                    initialLoad = false;
                }
                // block until you read at least one character
                int ic = brStdout.read();
                //System.out.println("got char");
                if (ic != -1)  buffer.append((char) ic);
                // read many characters to avoid the overhead of single
                // character output. On the other hand, it's expensive
                // to build huge buffers if Nyquist outputs a giant
                // list, so limit the buffer length to max_buf_len.
                while (brStdout.ready() && ic != '\n' && ic != -1 &&
                       buffer.length() < max_buf_len) {
                    //System.out.println("block on char in while");
                    ic = brStdout.read();
                    //System.out.println("got it");
                    if (ic != -1) buffer.append((char) ic);
                }
                if ((char) ic == '\n') {
                    line = new String(buffer);
                    testForCmd(line);
                } else if (buffer.length() >= max_buf_len) {
                    // pretend we found a line in order to flush 
                    // characters building up in buffer
                    line = " ";
                } else if (ic == -1) {
                    buffer.append("The Nyquist process has terminated. Please save your files and exit.\n");
                    nyquist_is_running = false;
                }
                textOut.append(buffer.substring(buffer_index));
                buffer_index = buffer.length();
                if (line != null) { // end of line, reset the buffer
                    line = null;
                    buffer.setLength(0);
                    buffer_index = 0;
                    // limit number of lines and characters in the textOut box
                    int lines = textOut.getLineCount();
                    if (lines > 5000 || 
                        textOut.getLineEndOffset(lines - 1) > 50000) {
                        // take out about 100 lines at a time:
                        textOut.replaceRange("", 0, 
                            textOut.getLineStartOffset(
                                        Math.max(1, lines - 4900)));
                    }
                }
                // cause the scroll to go to the end of the new text
                SwingUtilities.invokeLater(update);
            } catch (Throwable t) {
                t.printStackTrace();
            }
        }
    }
    
    private NyquistFile findFile(String filename) {
        JInternalFrame[] frames = mainFrame.jDesktop.getAllFrames();
        int i;
        for (i = 0; i < frames.length; i++) {
            if (frames[i] instanceof NyquistFile) {
                NyquistFile nyquistFile = (NyquistFile) frames[i];
                System.out.println("findFile search: " + 
                        nyquistFile.file.getName() + 
                        " file " + nyquistFile.file + " getAbsolutePath " +
                        nyquistFile.file.getAbsolutePath());
                if (nyquistFile.file.getAbsolutePath().equals(filename))
                    return nyquistFile;
            }
        }
        return null;
    }


    class StringParser { // used to parse input strings; see below
        String s;
        int pos;

        public StringParser(String textToParse, int initialPos) {
            s = textToParse;
            pos = initialPos;
        }

        public void skipSpace() {
            while (pos < s.length() && s.charAt(pos) == ' ') pos++;
        }

        public String getQuotedString() {
            // get what's between " and " -- quotes are required
            // note: this does not handle embedded quotes
            skipSpace();
            if (pos >= s.length()) return null;
            if (s.charAt(pos) != '"') return null;
            pos++;
            int start = pos;
            pos = s.indexOf('"', pos);
            if (pos < 0) {
                pos = start;
                return null;
            }
            pos = pos + 1;
            return s.substring(start, pos - 1);
        }

        public Integer getInteger() {
            skipSpace();
            int start = pos;
            while (pos < s.length() && Character.isDigit(s.charAt(pos))) {
                pos++;
            }
            if (start == pos) return null;
            return Integer.parseInt(s.substring(start, pos));
        }

        public Double getDouble() {
            skipSpace();
            int start = pos;
            while (pos < s.length() &&
                   (Character.isDigit(s.charAt(pos)) || s.charAt(pos) == '.')) {
                pos++;
            }
            if (start == pos) return null;
            return Double.parseDouble(s.substring(start, pos));
        }
    }

    // testForCmd looks for Nyquist output that the jNyqIDE wants to know
    // about -- originally just plot command output, but now there are many
    // Lisp functions that communicate and synchronize with the jNyqIDE
    public void testForCmd(String output) {
        int iNameStart;
        int iNameStop;
        //System.out.print("plot's text output:");
        //System.out.println(output);
        if (!plotNow) {
            iNameStart = output.indexOf("s-plot: writing ");
            if (iNameStart >= 0) {
                iNameStop = output.indexOf(" ...");
                plotFilename = output.substring(iNameStart + 16, iNameStop);
                // Nyquist file should be absolute or current directories
                // should be synchronized. Appending the current directory
                // causes problems when Nyquist already prints a full path
                //if (!mainFrame.currentDir.equals("")) {
                //    plotFilename = mainFrame.currentDir + plotFilename;
                //}
                System.out.println("file:" + plotFilename + ":");
                plotNow = true;
            }
        }
        // this is not ...else if... because output can have many lines
        if (plotNow) {
            iNameStart = output.indexOf(" points from");
            // now, the file is written and closed so we can open it
            if (iNameStart >= 0) {
                System.out.println("Calling NyqPlot.plot");
                NyqPlot.plot(plotFilename, plotFrame);
                plotNow = false;
            }
        }
        // test for loading workspace and saving workspace
        if (output.indexOf("workspace loaded") >= 0) {
            mainFrame.workspaceLoaded = true;
        } else if (output.indexOf("workspace saved") >= 0) { 
            mainFrame.workspaceSaved = true;
        } else if ((iNameStart = output.indexOf("slider-panel-create: \"")) >= 0) {
            StringParser sp = new StringParser(output, iNameStart + 21);
            String panelName = sp.getQuotedString();
            Integer color = sp.getInteger();
            System.out.println("panel create " + panelName + " " + color);
            if (panelName != null && color != null) {
                mainFrame.createSliderPanel(panelName, color);
            }
        } else if ((iNameStart = output.indexOf("slider-panel-close: \"")) >= 0) {
            StringParser sp = new StringParser(output, iNameStart + 21);
            String panelName = sp.getQuotedString();
            System.out.println("panel delete " + panelName);
            if (panelName != null) {
                mainFrame.deleteSliderPanel(panelName);
            }
        } else if ((iNameStart = output.indexOf("slider-create: \"")) >= 0) {
            StringParser sp = new StringParser(output, iNameStart + 15);
            String sliderName = sp.getQuotedString();
            Integer num = sp.getInteger();
            Double init = sp.getDouble();
            Double low = sp.getDouble();
            Double high = sp.getDouble();
            System.out.println("slider create " + sliderName + " " + num +
                               " " + init + " " + low + " " + high);
            if (sliderName != null && num != null && init != null && 
                low != null && high != null) {
                mainFrame.createSlider(sliderName, num, init, low, high);
            }
        } else if ((iNameStart = output.indexOf("button-create: \"")) >= 0) {
            StringParser sp = new StringParser(output, iNameStart + 15);
            String buttonName = sp.getQuotedString();
            Integer num = sp.getInteger();
            Integer normal = sp.getInteger();
            System.out.println("button create " + buttonName + " " + num +
                               " " + normal);
            if (buttonName != null && num != null && normal != null) {
                mainFrame.createButton(buttonName, num, normal);
            }
        } else if (!envelopeNow) { // test for envelope data
            int index = output.indexOf("get-env-data: begin");
            if (index >= 0) {
                output = output.substring(index);
                envData = "";
                envelopeNow = true;
            }
        }
        if (envelopeNow) {
            envData += output;
            int index = envData.indexOf("get-env-data: end");
            // remove first line
            if (index >= 0) {
                int begin = envData.indexOf("\n");
                envData = envData.substring(begin + 1, index);
                mainFrame.loadEnvData(envData);
                envelopeNow = false;
            }
        }
        if (!eqNow) {
            int index = output.indexOf("get-eq-data: begin");
            if (index >= 0) {
                output = output.substring(index);
                eqData = "";
                eqNow = true;
            }
        }
        if (eqNow) {
            eqData += output;
            int index = eqData.indexOf("get-eq-data: end");
            // remove first line
            if (index >= 0) {
                int begin = eqData.indexOf("\n");
                eqData = eqData.substring(begin + 1, index);
                mainFrame.loadEqData(eqData);
                eqNow = false;
            }
        }
        if (!editNow) {
            iNameStart = output.indexOf("score-edit: writing ");
            if (iNameStart >= 0) {
                iNameStop = output.indexOf(" ...");
                scoreEditFileName = output.substring(iNameStart + 20, iNameStop);
                if (!mainFrame.currentDir.equals("")) {
                    scoreEditFileName = mainFrame.currentDir + scoreEditFileName;
                }
                System.out.println("file:" + scoreEditFileName + ":");
                editNow = true;
            }
        }
        if (editNow) {
            iNameStart = output.indexOf("score-edit: wrote ");
            if (iNameStart >= 0) {
                Piano_Roll.scoreEdit(scoreEditFileName);
                editNow = false;
            }
        }
        if (!preferenceDataSent) {
            iNameStart = output.indexOf("by Roger B. Dannenberg");
            if (iNameStart >= 0) {
                try { Thread.sleep(200); }
                catch (InterruptedException ie) { }
                initialLoad = true;
                preferenceDataSent = true;
            }
        }
        // System.out.println("OUTPUT FOR PROMPT SEARCH: " + output);
        // determine command input mode from prompt by Nyquist:
        int promptLoc = output.indexOf("> ");
        if (output.indexOf("Entering SAL mode ...") >= 0) {
            mainFrame.setSalMode(true);
        } else if (output.indexOf("Returning to Lisp ...") >= 0) {
            mainFrame.setSalMode(false);
        } else if (output.indexOf("[ back to top level ]") >= 0) {
            mainFrame.setSalMode(false);
        } else if (output.indexOf("if continued: return from BREAK") >= 0) {
            mainFrame.setSalMode(false);
        } else if (promptLoc == 2 && output.indexOf(">>> in ") == 0) {
            System.out.println("... detected >>> in (error line location) ");
            int lineLoc = output.indexOf(", line ");
            int colLoc = output.indexOf(", col ");
            if (lineLoc > 0 && colLoc > 0) {
                String filename = output.substring(7, lineLoc);
                String lineString = output.substring(lineLoc + 7, colLoc);
                String colString = output.substring(colLoc + 6, 
                                                    output.length() - 2);
                NyquistFile nf = findFile(filename);
                System.out.println("nf.setCursor line " + lineString + 
                    " col " + colString + " file " + filename + " nf " + nf);
                if (isDigits(lineString) && isDigits(colString) && nf != null) {
                    nf.setCursor(Integer.parseInt(lineString), 
                                 Integer.parseInt(colString));
                }
            }
        } else if (promptLoc >= 0) {
            if (output.indexOf("SAL> ") == 0) {
                mainFrame.setSalMode(true);
                // System.out.println("in SAL mode");
            } else if (isDigits(output.substring(0, promptLoc))) {
                mainFrame.setSalMode(false);
                // System.out.println("in LISP mode");
            }
        }
    }
    
    private boolean isDigits(String s) {
        // returns true if all chars in s are digits
        for (int i = 0; i < s.length(); i++)
            if (!Character.isDigit(s.charAt(i))) return false;
        return true;
    }
    

    public OutputStream getStdin() {
        return stdin;
    }

    public void sendInput(String s) { 
        sendInput(s, false); 
    }
    
    public void sendInput(String s, boolean hide) {
        try {
            //System.out.println("Sending:" + hide + ": " + s);
            if (!hide) textOut.append(s);
            stdin.write( s.getBytes() );
            stdin.flush();
        } catch (Exception e) { 
            System.out.println(e); 
        }
    }
}









