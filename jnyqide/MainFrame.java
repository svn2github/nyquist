package jnyqide;

import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.*;
import java.beans.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.event.*;
import javax.swing.undo.*;
import javax.swing.text.html.*;
// import javax.swing.JOptionPane.*;
import javax.swing.filechooser.FileFilter;
import java.io.*;
import jnyqide.*;
// this won't work on non-Mac, so use reflection tricks below
// import com.apple.mrj.*; // import Mac menu support
import java.lang.reflect.*; // import Constructor class
import java.util.prefs.*;
import java.util.Collection;
import java.net.URL;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

class ScrollUpdate implements Runnable {
    MainFrame frame;
    
    ScrollUpdate(MainFrame mainframe) {
        frame = mainframe;
    }

    public void run() {
        frame.ScrollToEnd();
    }
}


public class MainFrame extends JFrame {
    JPanel contentPane;
    JMenuBar jMenuBar1 = new JMenuBar();
    
    JMenu jMenuFile = new JMenu();
    JMenu jMenuEdit = new JMenu();
    JMenu jMenuProcess = new JMenu();
    JMenu jMenuWindow = new JMenu();
    JMenu jMenuHelp = new JMenu();

    JToolBar jToolBar = new JToolBar();
    float test_value = 0.0F;
    ImageIcon image1;
    ImageIcon image2;
    ImageIcon image3;
    public JLabel statusBar = new JLabel();
    JButton salLispButton;
    BorderLayout borderLayout1 = new BorderLayout();
    JDesktopPane jDesktop;
    public CodePane jScrollPane;
    // accessed by CodePane sometimes:
    public JTextArea jOutputArea;
    JTextArea jListOutputArea;
    JScrollPane jOutputPane;
    JScrollPane jListOutputPane;
    JInternalFrame jOutputFrame;
    JInternalFrame jListOutputFrame;
    MiniBrowser miniBrowser;
    
    NyquistThread nyquistThread;
    JInternalFrame jFrame;
    // used by TextColor to communicate result back to MainFrame
    // public static boolean evenParens;
    String currentDir = "";
    Runnable update = new ScrollUpdate(this);
    PlotFrame plotFrame;
    File homeDir = new File(".");
    public String findPattern = "";
    public String replacePattern = "";
    boolean packFrame = false;
    EnvelopeFrame envelopeEditor;
    /* BEGIN UPIC */
    UPICFrame upicEditor;
    /* END UPIC */
    Jslide eqEditor;
    public Preferences prefs;
    public static final boolean prefStartInSalModeDefault = true;
    public static final boolean prefSalShowLispDefault = false;
    public static final boolean prefParenAutoInsertDefault = false;
    public static final boolean prefEnableSoundDefault = true;
    public static final boolean prefAutoNormDefault = true;

    public static final boolean prefSalTraceBackDefault = true;
    public static final boolean prefSalBreakDefault = false;
    public static final boolean prefXlispBreakDefault = true;
    public static final boolean prefXlispTraceBackDefault = false;

    public static final boolean prefPrintGCDefault = false;
    public static final boolean prefFullSearchDefault = true;
    public static final boolean prefInternalBrowserDefault = false;
    public static final boolean prefOnlineManualDefault = false;
    public static final double prefCompletionListPercentDefault = 60.0;
    public static final String prefAudioRateDefault = "44100";
    public static final String prefControlRateDefault = "2205";
    public static final String prefFontSizeDefault = "12";

    public static boolean prefStartInSalMode = prefStartInSalModeDefault;
    public static boolean prefSalShowLisp = prefSalShowLispDefault;
    public static boolean prefParenAutoInsert = prefParenAutoInsertDefault;
    public static boolean prefEnableSound = prefEnableSoundDefault;
    public static boolean prefAutoNorm = prefAutoNormDefault;
    public static boolean prefSalTraceBack = prefSalTraceBackDefault;
    public static boolean prefSalBreak = prefSalBreakDefault;
    public static boolean prefXlispBreak = prefXlispBreakDefault;
    public static boolean prefXlispTraceBack = prefXlispTraceBackDefault;
    public static boolean prefPrintGC = prefPrintGCDefault;
    public static boolean prefFullSearch = prefFullSearchDefault;
    public static boolean prefInternalBrowser = prefInternalBrowserDefault;
    public static boolean prefOnlineManual = prefOnlineManualDefault;
    public static double prefCompletionListPercent = 
                                 prefCompletionListPercentDefault;
    public static String prefAudioRate = prefAudioRateDefault;
    public static String prefControlRate = prefControlRateDefault;
    public static String prefFontSize = prefFontSizeDefault;
    public static String prefDirectory = "";
    public static String prefSFDirectory = "";

    public static boolean prefsHaveBeenSet = false;
        
    public boolean workspaceLoaded = false;
    public boolean workspaceSaved = false;
    
    public static final String onlineManualURL = 
            "http://www.cs.cmu.edu/~rbd/doc/nyquist/";


    // inputStrings allows user to type ^P to get previous entry, 
    // or ^D for next entry. This is tricky. The desired behavior is:
    // There is a history list of everything executed in order.
    // Typing ^P moves a cursor back in history, ^D forward. When you type
    // Enter, a new string is placed at the front of history, and the
    // cursor is set to the front of history as well.
    // To implement this behavior, inputStringsX is the front of history.
    // It wraps around when it reaches inputStringsLen. To be precise,
    // inputStringsX is the indeX of the location where the next input
    // string will go. inputStringsCursor is the position controlled
    // by ^P and ^D. inputSringsCursor is set
    // to inputStringsX when the user types Enter.
    
    // Sending input is tricky because you could be in Lisp or SAL mode.
    // Either way, you want to indent multi-line input by the prompt size
    // (2 for lisp, 4 for Sal) and you want an extra return after Sal
    // commands. You DO NOT want the last 2 returns to be followed by
    // spaces since SAL looks at the end of input to determine when a
    // command is complete.
    //
    // Use cases:
    //     calling a function like replay (R):
    //          use callFunction()
    //     user types return after code in input box:
    //          use SendInputLn
    //
    // Support functions:
    //     callFunction -- build expression and call SendInputLn
    //     setVariable -- build expression and call SendInputLn
    //     SendInputLn -- fix up indentation, append newline or 2, SendInput
    //     SendInput -- just send text to Nyquist
    
    int inputStringsX = 0;
    int inputStringsCursor = 0;
    int inputStringsLen = 20;
    String inputStrings[] = new String[inputStringsLen];
        
    // some "features" for system dependent code
    public boolean hasRightMouseButton = true;

    //Construct the frame
    public MainFrame() {
        enableEvents(AWTEvent.WINDOW_EVENT_MASK);
        try {
            mainFrameInit();
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }
    
    
    public boolean isMac() {
        // System.out.println("mrj.version" + System.getProperty("mrj.version"));
        // return System.getProperty("mrj.version") != null;

        // The code above seems not to work on Leopard; the following 
        // suggested by Raymond Martin:

        final String strOS;

        try { strOS = System.getProperty("os.name"); }
        catch(final SecurityException e) {
            System.out.println("In isMac: error " + e);
            return(false); 
        }
        System.out.println("strOS " + strOS);
        return(strOS.indexOf("Mac OS") >= 0);
    }

    PreferencesDialog preferencesDialog;
    
    public void disconnectPreferences() {
        preferencesDialog = null;
    }
    
    
    protected void setVariable(String var, String val) {
        String input;
        if (jScrollPane.isSal) {
            input = "set " + var + " = " + val;
        } else {
            input = "(setf " + var + " " + val + ")";
        }
        sendInputLn(input);
    }
    
    
    String tOrNil(boolean val) {
        return (val ? "t" : "nil");
    }


    protected void setBoolean(String var, boolean val) {
        setVariable(var, tOrNil(val));
    }

    
    // invoke a function call in Nyquist with 0 or 1 parameters
    // (pass "" for 0 parameters)
    protected void callFunction(String fn, String parameter) {
        String input;
        if (jScrollPane.isSal) {
            input = "exec " + fn + "(" + parameter + ")";
        } else {
            input = "(" + fn + (parameter.length() > 0 ? " " : "") +
                    parameter + ")";
        }
        sendInputLn(input);
    }
    
    
    public void Prefs() {
        // ignore if preferences is already open
        if (preferencesDialog != null) return;
        preferencesDialog = new PreferencesDialog(this);
        jDesktop.add(preferencesDialog);
        jDesktop.getDesktopManager().activateFrame(preferencesDialog);
    }

    // create a button
    private JButton buttonInit(String name, 
                               String tip, ActionListener listener) {
        JButton button = new JButton();
        button.setText(name);
        button.setActionCommand(name);
        button.setToolTipText(tip);
        button.addActionListener(listener);
        jToolBar.add(button);
        return button;
    }

    private void menuAddItem(JMenu menu, String name, char mnemonic,
                 KeyStroke accelerator, ActionListener listener) {
        JMenuItem item = new JMenuItem();
        item.setText(name);
        item.setActionCommand(name);
        if (mnemonic != '\000') item.setMnemonic(mnemonic);
        if (accelerator != null) item.setAccelerator(accelerator);
        item.addActionListener(listener);
        menu.add(item);
    }
    
    public void handlePrefs() {
        System.out.println("handlePrefs called");
    }
    
    //Component initialization
    private void mainFrameInit()  throws Exception  {
        // if this is a Mac, we want some menu items on the application menu, 
        // which is accessed via some special apple code, but the apple code 
        // is not going to be installed if you are running windows or linux,
        // so we use java reflection here to load the special code ONLY if 
        // we're on a Mac
        //
        // The special code ultimately calls back to the methods About(), 
        // Prefs(), and Quit() in this class. The "normal" Windows/Linux 
        // code should do the same.
        if (isMac()) {
            hasRightMouseButton = false;
            try {
                Object[] args = { this };
                Class[] arglist = { MainFrame.class };

                Class mac_class = Class.forName("jnyqide.SpecialMacHandler");

                /* 
                Thread t = Thread.currentThread();
                ClassLoader cl = t.getContextClassLoader();
                Class mac_class = cl.loadClass("SpecialMacHandler");
                 */

                System.out.println("got the class\n");
                Constructor new_one = mac_class.getConstructor(arglist);
                System.out.println("got the constructor\n");
                new_one.newInstance(args);

                System.out.println("isMac, so created instance of SpecialMacHandler");
            } catch(Exception e) {
                System.out.println(e);
            }
        }

        prefs = Preferences.userNodeForPackage(Main.class);
        prefStartInSalMode = prefs.getBoolean("start-with-sal", 
                                              prefStartInSalMode);
        prefSalShowLisp = prefs.getBoolean("sal-show-lisp", prefSalShowLisp);
        prefParenAutoInsert = prefs.getBoolean("paren-auto-insert", 
                                               prefParenAutoInsert);
        prefEnableSound = prefs.getBoolean("sound-enable", prefEnableSound);
        prefAutoNorm = prefs.getBoolean("auto-norm", prefAutoNorm);
        prefSalTraceBack = prefs.getBoolean("sal-traceback", prefSalTraceBack);
        prefSalBreak = prefs.getBoolean("sal-break", prefSalBreak);
        prefXlispBreak = prefs.getBoolean("xlisp-break", prefXlispBreak);
        prefXlispTraceBack = prefs.getBoolean("xlisp-traceback", 
                                               prefXlispTraceBack);
        // if XlispTracBack, then we need to set XlispBreak:
        prefXlispBreak = prefXlispBreak || prefXlispTraceBack;
        prefPrintGC = prefs.getBoolean("print-gc", prefPrintGC);
        prefFullSearch = prefs.getBoolean("completion-list-full-search", 
                                          prefFullSearch);
        prefInternalBrowser = prefs.getBoolean("internal-browser", 
                                               prefInternalBrowser);
        prefOnlineManual = prefs.getBoolean("online-manual", prefOnlineManual);
        prefCompletionListPercent = prefs.getDouble("completion-list-percent", 
                                                    prefCompletionListPercent);
        prefAudioRate = prefs.get("audio-rate", prefAudioRate);
        prefControlRate = prefs.get("control-rate", prefControlRate);
        prefFontSize = prefs.get("font-size", prefFontSize);
        prefDirectory = prefs.get("initial-directory", prefDirectory);
        prefSFDirectory = prefs.get("default-sf-directory", prefSFDirectory);
        prefsHaveBeenSet = false;

        image1 = new ImageIcon("openFile.gif");
        image2 = new ImageIcon("closeFile.gif");
        image3 = new ImageIcon("help.gif");
        //setIconImage(Toolkit.getDefaultToolkit().createImage(MainFrame.class.getResource("[Your Icon]")));
        contentPane = (JPanel) this.getContentPane();
        contentPane.setLayout(borderLayout1);
        this.setSize(new Dimension(400, 300));
        this.setTitle("Nyquist IDE");
        statusBar.setText(" ");
        
        ActionListener menuButtonListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                menuButtonHandler(e);
            }
        };
        
        // Menu Bar
        jMenuFile.setText("File");
        menuAddItem(jMenuFile, "New", 'n', 
            KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N,
                       Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()),
            menuButtonListener);
        menuAddItem(jMenuFile, "Open", 'o', 
            KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O,
                       Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()),
            menuButtonListener);

        if (!isMac()) {  // if isMac(), Quit (not Exit) and Prefs are on the 
                         // application menu
            menuAddItem(jMenuFile, "Preferences...", '\000', null, 
                        menuButtonListener);
            menuAddItem(jMenuFile, "Exit", '\000', null, menuButtonListener);
        }

        jMenuEdit.setText("Edit");

        menuAddItem(jMenuEdit, "Previous", 'p',
            KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,
                       Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()),
            menuButtonListener);

        menuAddItem(jMenuEdit, "Next", 'n', 
            KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D,
                       Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()),
            menuButtonListener);

        menuAddItem(jMenuEdit, "Select Expression", 'e',
                    KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_E,
                                           Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()),
                    menuButtonListener);

        jMenuHelp.setText("Help");
        if (!isMac()) {  // if isMac(), About is on the application menu
            menuAddItem(jMenuHelp, "About", 'h', null, menuButtonListener);
        }
        menuAddItem(jMenuHelp, "Manual", 'm', null, menuButtonListener);

        jMenuProcess.setText("Process");
        menuAddItem(jMenuProcess, "Replay", 'r', null, menuButtonListener);
        menuAddItem(jMenuProcess, "Plot", 'p', null, menuButtonListener);
        menuAddItem(jMenuProcess, "Copy to Lisp", 'u', 
            KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_U,
                       Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()),
            menuButtonListener);
        menuAddItem(jMenuProcess, "Mark", 'a', null, menuButtonListener);
        
        jMenuWindow.setText("Window");
        menuAddItem(jMenuWindow, "Tile", 't', null, menuButtonListener);
        menuAddItem(jMenuWindow, "Browse", 'b', null, menuButtonListener);
        menuAddItem(jMenuWindow, "EQ", 'q', null, menuButtonListener);
        menuAddItem(jMenuWindow, "Envelope Edit", 'e', null, menuButtonListener);
        // menuAddItem(jMenuWindow, "Nyquistlator", 'y', null, menuButtonListener);
        /* BEGIN UPIC */
        menuAddItem(jMenuWindow, "UPIC Edit", 'u', null, menuButtonListener);
        /* END UPIC */
        buttonInit("Info", "Print Lisp memory status", menuButtonListener);
        buttonInit("Break", "Break/interrupt Lisp interpreter",
           menuButtonListener);
        // removed "Up" and "Cont" just to make some real-estate
        // buttonInit("Up", "Return from this break level", menuButtonListener);
        // buttonInit("Cont", "Continue Lisp execution", menuButtonListener);
        salLispButton = buttonInit("Sal", "Change Mode", menuButtonListener);
        buttonInit("Top", "Exit to Lisp top-level", menuButtonListener);
        buttonInit("Replay", "Replay the last sound", menuButtonListener);
        int i;
        for (i = 0; i < 11; i++) {
            String name = "F" + (i + 2);
            String tip = "Evaluate (" + name + ")";
            buttonInit(name, tip, menuButtonListener);
        }

        buttonInit("Browse", "Browse Sound/Instrument/Effect Library",
           menuButtonListener);
        buttonInit("EQ", "Equalizer Control Panel", menuButtonListener);
        buttonInit("EnvEdit", "Open Graphical Envelope Editor", 
                   menuButtonListener);
        // buttonInit("Nyquistlator", "Nyquistlator", menuButtonListener);
        // buttonNew.setIcon(image1);
        buttonInit("New File", "New File", menuButtonListener);
        // buttonOpen.setIcon(image1);
        buttonInit("Open File", "Open File", menuButtonListener);
        // buttonSave.setIcon(image3);
        buttonInit("Save File", "Save File", menuButtonListener);
        buttonInit("Load", "Load File into Nyquist", menuButtonListener);
        buttonInit("Mark", "Get elapsed audio play time", menuButtonListener);
        
        jMenuBar1.add(jMenuFile);
        jMenuBar1.add(jMenuEdit);
        jMenuBar1.add(jMenuProcess);
        jMenuBar1.add(jMenuWindow);
        jMenuBar1.add(jMenuHelp);
        this.setJMenuBar(jMenuBar1);
        
        //MRJApplicationUtils.registerPrefsHandler(this);
        
        jOutputArea = new JTextArea();
        jOutputArea.setFont(new Font("Courier", Font.PLAIN, 12));
        jOutputArea.setLineWrap(true);
        jOutputArea.setEditable(false);
        jOutputPane = new JScrollPane( jOutputArea );
        jOutputPane.setHorizontalScrollBarPolicy(
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        jOutputPane.setVerticalScrollBarPolicy( 
                                               JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        
        jListOutputArea = new JTextArea();
        jListOutputArea.setLineWrap(true);
        jListOutputArea.setEditable(false);
        // add mouse listener
        jListOutputArea.addMouseListener(new MouseListener () {
            public void mouseExited(MouseEvent e) { };
            public void mouseEntered(MouseEvent e) { };
            public void mousePressed(MouseEvent e) { };
            public void mouseReleased(MouseEvent e) { };
            
            public void mouseClicked(MouseEvent e) {
                // System.out.println(e.paramString());
                int pos = jListOutputArea.viewToModel(e.getPoint());
                int start = 0, end = 0;
                String line = "";
                // System.out.println("text posn = " + pos);
                try {
                    int lineno = jListOutputArea.getLineOfOffset(pos);
                    // System.out.println("line no = " + lineno);
                    start = jListOutputArea.getLineStartOffset(lineno);
                    end = jListOutputArea.getLineEndOffset(lineno);
                    // System.out.println("start  = " + start + " end = " + end);
                    // skip newline by subtracting one from length
                    if (end > start + 1) {
                        line = jListOutputArea.getText(start, end - start - 1);
                        // WordList.replaceWithTemplate(line);
                    } // otherwise nothing selected
                } catch (Exception ex) {
                    ex.printStackTrace(System.err);
                }
                
                // System.out.println("event: " + e);
                if (SwingUtilities.isRightMouseButton(e) ||
                    // for Mac, allow option click (which java sees as alt key)
                    (e.getModifiers() & InputEvent.ALT_MASK) != 0) {
                    String ext = WordList.getlink(line);
                    System.out.println(line + " : " + ext);
                    String url, urlbase;
                    
                    if (prefOnlineManual) urlbase = onlineManualURL;
                    else urlbase = findManualURL("");
                    url = urlbase + ext;
                    
                    if (prefInternalBrowser) {
                        miniBrowser.setVisible(true);
                        System.out.println("URL is: " + url);
                        miniBrowser.setPage(url);
                    } else BareBonesBrowserLaunch.openURL(url);
            	} else {
                    // System.out.println(e.paramString());
                    WordList.replaceWithTemplate(line);
                }
            }
        });
        jListOutputPane = new JScrollPane( jListOutputArea );
        jListOutputPane.setHorizontalScrollBarPolicy(
                JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        jListOutputPane.setVerticalScrollBarPolicy(
                JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    
        jScrollPane = new CodePane(new Dimension(400, 200), this, statusBar,
                                   Integer.parseInt(prefFontSize));
        
        // Top panel for command entry, plot, and toolbar
        JPanel jCommands = new JPanel( new BorderLayout() );
        JPanel jInputAndPlot = new JPanel(new BorderLayout(3, 0));
        jInputAndPlot.add(jScrollPane, BorderLayout.WEST);
        System.out.println("\n\n\n\n\n\n\nadded jScrollPane " + jScrollPane + " to panel\n\n\n\n\n\n");
        jCommands.add(jToolBar, BorderLayout.SOUTH);
        jCommands.add(jInputAndPlot, BorderLayout.CENTER);
        jCommands.setPreferredSize(new Dimension(300, 150));
        
        // Main desktop
        jDesktop = new JDesktopPane();
        jDesktop.setPreferredSize( new Dimension(300, 300) );
        
        jOutputFrame = new JInternalFrame("Output");
        // make this wide enough so XLISP GC messages do not wrap 
        //   (it's annoying)
        //jOutputFrame.setSize(new Dimension(500, 530 / 3));
        jOutputFrame.setVisible(true);
        jOutputFrame.getContentPane().add(jOutputPane);
        jOutputFrame.setResizable( true );
        //jOutputFrame.setLocation(0, (530 * 2) / 3);
        jDesktop.add( jOutputFrame );
        
        
        String clTitle = "Completion List" + (hasRightMouseButton ?
                                              " - Right Click for Help" :
                                              " - Option Click for Help");
        jListOutputFrame = new JInternalFrame(clTitle);
        //jListOutputFrame.setSize( new Dimension (500, (530 * 2) / 3));
        jListOutputFrame.setVisible(true);
        jListOutputFrame.getContentPane().add(jListOutputPane);
        jListOutputFrame.setResizable(true);
        //jListOutputFrame.setLocation(0,0);
        jDesktop.add(jListOutputFrame);
        
        contentPane.add( jCommands, BorderLayout.NORTH);
        contentPane.add( jDesktop, BorderLayout.CENTER );
        contentPane.add(statusBar, BorderLayout.SOUTH);
        setSize( new Dimension(800, 800) );

        miniBrowser = new MiniBrowser("Nyquist Manual");
        jDesktop.add(miniBrowser);
        
        TextColor.init();
        WordList.init(jListOutputArea);
        SalWordList.init();
        
        plotFrame = new PlotFrame(jInputAndPlot);
        
        nyquistThread = new NyquistThread();
        nyquistThread.start(jOutputArea, update, plotFrame, this);
        
        System.out.print("jDesktop size: ");
        System.out.println(jDesktop.getSize().toString());

        contentPane.setTransferHandler(new TransferHandler() {
            public boolean canImport(JComponent comp, DataFlavor[] transferFlavors) {
                for (DataFlavor transferFlavor : transferFlavors) {
                    if (transferFlavor.isFlavorJavaFileListType())
                        return true;
                }
                return false;
            }

            public boolean importData(JComponent comp, Transferable t) {
                try {
                    Collection<File> files = (Collection<File>) 
                            t.getTransferData(DataFlavor.javaFileListFlavor);
                    for (File file : files) {
                        openFile(file);
                    }
                    return true;
                } catch (Exception e) {
                   System.out.println("Drop failed: "+e.getMessage());
                }
                return false;
            }
        });
        // set size and location for jOutputFrame and jListOutputFrame
        // now this is done in Main after making desktop visible (otherwise
        // you can't get dimensions of the desktop and layout is faulty)
        // tileCompletion(); 
    }
    
    
    public void sendPreferenceData() {
        // send Nyquist the preference values (assumes in Lisp mode)
        sendInputLn(";; transferring preference data from jNyqIDE to Nyquist");
        sendInputLn("(progn");
        setBoolean("*sal-compiler-debug*", prefSalShowLisp);
        callFunction(prefEnableSound ? "sound-on" : "sound-off", "");
        callFunction(prefAutoNorm ? "autonorm-on" : "autonorm-off", "");
        callFunction("sal-tracenable", tOrNil(prefSalTraceBack));
        callFunction("sal-breakenable", tOrNil(prefSalBreak));
        callFunction("xlisp-breakenable", tOrNil(prefXlispBreak));
        callFunction("xlisp-tracenable", tOrNil(prefXlispTraceBack));
        setBoolean("*gc-flag*", prefPrintGC);
        callFunction("set-sound-srate", prefAudioRate);
        callFunction("set-control-srate", prefControlRate);
        setFontSize(Integer.parseInt(prefFontSize));
        if (prefDirectory != null && prefDirectory.length() > 0) {
            changeDirectory(prefDirectory);
        }
        if (prefSFDirectory != null && prefSFDirectory.length() > 0) {
            String dirString = escape_backslashes(prefSFDirectory);
            setVariable("*default-sf-dir*", "\"" + dirString + "\"");
        } else { // no preference, suggest Java temp dir. The Java temp dir
            // will be used as *default-sf-dir* only if *default-sf-dir*
            // was set to "", meaning previous methods to identify a temp
            // directory failed to produce anything.
            String tempdir = System.getProperty("java.io.tmpdir");
            if (!(tempdir.endsWith("/") || tempdir.endsWith("\\")))
                tempdir = tempdir + "/";
            // flip backslash to slash to avoid quote problems
            tempdir = "\"" + tempdir.replaceAll("\\\\", "/") + "\"";
            callFunction("suggest-default-sf-dir", tempdir);
        }
        setBoolean("*sal-secondary-prompt*", false);
        sendInputLn(";; end preference data transfer");
        sendInputLn(")");

        if (prefStartInSalMode) {
            callFunction("sal", "");
        }
    }
    
    //File | Exit action performed
    public void menuButtonHandler(ActionEvent e) {
        String cmd = e.getActionCommand();
        if (cmd == "New" || cmd == "New File") doFileNew(e);
        else if (cmd == "Open" || cmd == "Open File") doFileOpen(e);
        else if (cmd == "Save" || cmd == "Save File") doFileSave(e);
        else if (cmd == "Save As...") doFileSaveAs(e);
        else if (cmd == "Load" || cmd == "Load...") doFileLoad(e);
        else if (cmd == "Mark") doProcessMark(e);
        else if (cmd == "Preferences...") Prefs();
        else if (cmd == "Exit") {
            SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        Quit();
                    }
                });
            // throw new IllegalStateException("Let the quit handler do it");
        } else if (cmd == "Find...") doEditFind(e);
        else if (cmd == "Replace...") doEditReplace(e);
        else if (cmd == "Previous") doEditPrevious(e);
        else if (cmd == "Next") doEditNext(e);
        else if (cmd == "Select Expression") doEditSelectExpression(e);
        else if (cmd == "About") About();
        else if (cmd == "Manual") doHelpManual(e);
        else if (cmd == "Replay") doProcessReplay(e);
        else if (cmd == "Plot") doProcessPlot(e);
        else if (cmd == "Tile") doWindowTile(e);
        else if (cmd == "Browse") doWindowBrowse(e);
        else if (cmd == "EQ") doWindowEQ(e);
        else if (cmd == "EnvEdit" || cmd == "Envelope Edit") doWindowEnvelope(e);
        // else if (cmd == "Nyquistlator") doNyquistlator(e);
        /* BEGIN UPIC */
        else if (cmd == "UPIC Edit") doWindowUPIC(e);
        /* END UPIC */
        else if (cmd == "Info") doProcessInfo(e);
        else if (cmd == "Break") doProcessBreak(e);
        else if (cmd == "Up") doProcessUp(e);
        else if (cmd == "Cont") doProcessCont(e);
        else if (cmd == "Top") doProcessTop(e);
        else if (cmd == "Lisp") doProcessLisp(e);
        else if (cmd == "Sal") doProcessSal(e);
        else if (cmd == "Copy to Lisp") doProcessCopyToLisp(e);
        // do this last so other commands starting with "F" get handled
        else if (cmd.charAt(0) == 'F') doProcessFn(e);
        else System.out.println("menu or button command not expected: " + cmd);
    }
    
    public void Quit() {
        System.out.println("Quit() called in MainFrame.java");
        // close prefs?
        int r = JOptionPane.OK_OPTION;
        if (preferencesDialog != null) {
            r = JOptionPane.showConfirmDialog(this,
                    "Really close without closing (saving) preferences?",
                    "alert", JOptionPane.OK_CANCEL_OPTION);
        }
        if (r != JOptionPane.OK_OPTION) return; // do not quit
        if (prefsHaveBeenSet) {
            prefs.putBoolean("start-with-sal", prefStartInSalMode);
            prefs.putBoolean("sal-show-lisp", prefSalShowLisp);
            prefs.putBoolean("paren-auto-insert", prefParenAutoInsert);
            prefs.putBoolean("sound-enable", prefEnableSound);
            prefs.putBoolean("auto-norm", prefAutoNorm);
            prefs.putBoolean("sal-traceback", prefSalTraceBack);
            prefs.putBoolean("sal-break", prefSalBreak);
            prefs.putBoolean("xlisp-break", prefXlispBreak);
            prefs.putBoolean("xlisp-traceback", prefXlispTraceBack);
            prefs.putBoolean("print-gc", prefPrintGC);
            prefs.putBoolean("completion-list-full-search", prefFullSearch);
            prefs.putBoolean("internal-browser", prefInternalBrowser);
            prefs.putBoolean("online-manual", prefOnlineManual);
            prefs.putDouble("completion-list-percent", 
                            prefCompletionListPercent);
            prefs.put("audio-rate", prefAudioRate);
            prefs.put("control-rate", prefControlRate);
            prefs.put("font-size", prefFontSize);
            prefs.put("initial-directory", prefDirectory);
            prefs.put("default-sf-directory", prefSFDirectory);
            prefsHaveBeenSet = false;
        }
        JInternalFrame[] frames = jDesktop.getAllFrames();
        boolean flag = false;
        int i;
        for (i = 0; i < frames.length; i++) {
            if (frames[i] instanceof NyquistFile) {
                NyquistFile nyquistFile = (NyquistFile) frames[i];
                if (nyquistFile.modified) flag = true;
            }
        }
        r = JOptionPane.OK_OPTION;
        if (flag) {
            r = JOptionPane.showConfirmDialog(this,
                    "Really close without saving?",
                    "alert", JOptionPane.OK_CANCEL_OPTION);
        }
        if (r != JOptionPane.OK_OPTION) return; // do not quit
        if (workspaceLoaded) {
            r = JOptionPane.showConfirmDialog(this,
                    "Save workspace to current directory before exiting?",
                    "alert", JOptionPane.YES_NO_CANCEL_OPTION);
            if (r == JOptionPane.YES_OPTION) {
                workspaceSaved = false; // interface with NyquistThread
                callFunction("save-workspace", "");
                i = 0;
                while (!workspaceSaved && i < 10000) { // allow 10s
                    try { Thread.sleep(200); }
                    catch (InterruptedException ie) { }
                    i += 200;
                }
                if (!workspaceSaved) {
                    r = JOptionPane.showConfirmDialog(this,
                            "Timed out waiting for workspace save.\n" +
                            "Your workspace data may not be saved.\n" +
                            "Exit anyway?",
                            "alert", JOptionPane.OK_CANCEL_OPTION);
                }
            }
            if (r == JOptionPane.CANCEL_OPTION) return; // do not quit
        }
        System.out.println("Sending (exit) to Nyquist...");
        // try to shut down Nyquist before it is orphaned
        // Sal need special syntax to exit the Nyquist process:
        if (jScrollPane.isSal) sendInputLn("exit nyquist");
        else                  callFunction("exit", ""); 
        try {
            Thread.sleep(200); // does it help Nyquist's exit to stall? 
        } catch (InterruptedException ie) {
        }
        System.out.println("Exiting from NyquistIDE");
        System.exit(0);
    }
    
    
    public void prepareNewNyquistFile(final NyquistFile file) {
        jDesktop.add(file);
        jDesktop.getDesktopManager().activateFrame(file);
        jDesktop.setSelectedFrame(file);
        file.addInternalFrameListener(
            new InternalFrameListener() {
                public void internalFrameClosing(InternalFrameEvent e) {
                    //System.out.println("FrameClosing");
                    int r = JOptionPane.OK_OPTION;
                    if (file.modified) {
                        r = JOptionPane.showConfirmDialog(file,
                                "Really close without saving?",
                                "alert", JOptionPane.OK_CANCEL_OPTION);
                    }
                    if (r == JOptionPane.OK_OPTION) {
                        file.dispose();
                    }
                }
                public void internalFrameOpened(InternalFrameEvent e) {
                }
                public void internalFrameClosed(InternalFrameEvent e) {
                    //System.out.println("FrameClosed");
                }
                public void internalFrameIconified(InternalFrameEvent e) {
                }
                public void internalFrameDeiconified(InternalFrameEvent e) {
                }
                public void internalFrameActivated(InternalFrameEvent e) {
                }
                public void internalFrameDeactivated(InternalFrameEvent e) {
                }
            }
        );
    }


    public void doFileNew(ActionEvent e) {

        final NyquistFile file = 
                new NyquistFile(this, Integer.parseInt(prefFontSize));
        prepareNewNyquistFile(file);
    }
    
    public String fileDirectory(File file) {
        String path = file.getAbsolutePath();
        String name = file.getName();
        return path.substring(0, path.length() - name.length());
    }
    
    
    //File | Exit action performed
    public void doFileOpen(ActionEvent e) {
        JFileChooser chooser = new JFileChooser();
        LispFileFilter lispFilter = new LispFileFilter();
        SalFileFilter salFilter = new SalFileFilter();
        // last one seems to be the default setting for user, so set according
        // to current mode
        if (jScrollPane.isSal) {
            chooser.setFileFilter(lispFilter);
            chooser.addChoosableFileFilter(salFilter);
        } else {
            chooser.setFileFilter(salFilter);
            chooser.addChoosableFileFilter(lispFilter);
        }
        // note if current directory setting fails on some platform,
        // consider this code using getCanonicalPath():
        //      File f = new File(new File(".").getCanonicalPath());
        File curdir = new File(currentDir);
        chooser.setCurrentDirectory(curdir);
        int returnVal = chooser.showOpenDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            System.out.println("You chose to open this file: " +
                               chooser.getSelectedFile().getAbsoluteFile());
            openFile(chooser.getSelectedFile());
        }
    }

    private void openFile(File fileToOpen) {
        // see if file is already open
        JInternalFrame[] frames = jDesktop.getAllFrames();
        int i;
        for (i = 0; i < frames.length; i++) {
            if (frames[i] instanceof NyquistFile) {
                NyquistFile file = (NyquistFile) frames[i];

                if (file.getFile() != null &&
                    file.getFile().getAbsolutePath().equals(
                            fileToOpen.getAbsolutePath())) {
                    jDesktop.setSelectedFrame(file);
                    try {
                        file.setSelected(true);
                    }
                    catch(PropertyVetoException ve) {
                        //System.out.println("setSelected was vetoed");
                    }

                    JInternalFrame jInternalFrame =
                        jDesktop.getSelectedFrame();
                    if (jInternalFrame instanceof NyquistFile) {
                        NyquistFile nf = (NyquistFile) jInternalFrame;
                        //System.out.println("selected is " +
                        //                   nf.getAbsolutePath());
                    } else {
                        //System.out.println("selected not a NyquistFile");
                    }
                    return;
                }
            }
        }
        // Didn't find it. Open it in a new frame.
        final NyquistFile file =
            new NyquistFile(fileToOpen, this, Integer.parseInt(prefFontSize));
        changeDirectory(fileDirectory(fileToOpen));
        prepareNewNyquistFile(file);
    }

    public void doFileSave(ActionEvent e) {
        if (jDesktop.getSelectedFrame() instanceof NyquistFile) {
            NyquistFile file = (NyquistFile)jDesktop.getSelectedFrame();
            if (file.save(currentDir)) {
                changeDirectory(fileDirectory(file.getFile()));
            }
        }
    }
    
    public void doFileSaveAs(ActionEvent e) {
        if (jDesktop.getSelectedFrame() instanceof NyquistFile) {
            NyquistFile file = (NyquistFile)jDesktop.getSelectedFrame();
            if (file.saveAs(currentDir)) {
                changeDirectory(fileDirectory(file.getFile()));
            }
        }
    }
    
    public void doFileLoad(ActionEvent e) {
        JInternalFrame frame = jDesktop.getSelectedFrame();
        if (frame instanceof NyquistFile) {
            NyquistFile file = (NyquistFile) frame;
            if (file.save(currentDir)) {
                loadFile(file.getFile());
            }
        }
    }
    
    //Edit | Find action performed
    public void doEditFind(ActionEvent e) {
        JInternalFrame frame = jDesktop.getSelectedFrame();
        if (frame instanceof NyquistFile) {
            NyquistFile file = (NyquistFile) frame;
            FindDialog findDialog = new FindDialog(file, this);
        }
    }
    
    //Edit | Replace action performed
    public void doEditReplace(ActionEvent e) {
        JInternalFrame frame = jDesktop.getSelectedFrame();
        if (frame instanceof NyquistFile) {
            NyquistFile file = (NyquistFile) frame;
            ReplaceDialog replaceDialog = new ReplaceDialog(file, this);
        }
    }
    
    public void filterCRLF(StringBuffer buf)
    {
        //int i = buf.toString().indexOf("\r\n"); // note: buf.indexOf() doesn't work on Mac
        //while (i >= 0) {
        //    buf.replace(i, i + 2, "\n");
        //    i = buf.toString().indexOf("\r\n", i);
        //}
        buf.replace(0, buf.length(), buf.toString().replaceAll("\r\n", "\n"));
    }
    
    public String trimNewline(String s) {
        int len = s.length();
        while (len > 0 &&
               (s.charAt(len - 1) == '\n' || s.charAt(len - 1) == '\r')) {
            len = len - 1;
        }
        return s.substring(0, len);
    }
    
    //Edit | Previous action performed
    public void doEditPrevious(ActionEvent e) {
        inputStringsCursor = inputStringsCursor - 1;
        if (inputStringsCursor < 0) inputStringsCursor = inputStringsLen - 1;
        String text = inputStrings[inputStringsCursor];
        if (text != null) {
            // remove the newline at the end
            jScrollPane.pane.setText(trimNewline(text));
        }
    }
    
    //Edit | Next action performed
    public void doEditNext(ActionEvent e) {
        inputStringsCursor = inputStringsCursor + 1;
        if (inputStringsCursor >= inputStringsLen) inputStringsCursor = 0;
        String text = inputStrings[inputStringsCursor];
        if (text != null) {
            jScrollPane.pane.setText(trimNewline(text));
        }
    }
    
    //Edit | Select Expression
    public void doEditSelectExpression(ActionEvent e) {
        JInternalFrame frame = jDesktop.getSelectedFrame();
        if (frame instanceof NyquistFile) {
            NyquistFile file = (NyquistFile) frame;
            file.selectExpression();
        }
    }
    
    //Help | About action performed
    public void About() {
        MainFrame_AboutBox dlg = new MainFrame_AboutBox(this);
        Dimension dlgSize = dlg.getPreferredSize();
        Dimension frmSize = getSize();
        Point loc = getLocation();
        dlg.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
        dlg.setModal(true);
        dlg.setVisible(true);
        
        Graphics g = jFrame.getContentPane().getGraphics();
        g.setColor(Color.cyan);
        g.fillRect(50, 50, 100, 100);
        
    }
    
    public String findManualURL(String ext) {
        String url = "";
        try { 
            url = homeDir.getCanonicalPath();
        } catch (Exception e) { 
            System.out.println("findManualURL exception: " + e);
        }
        String osName = System.getProperty("os.name");
        if (osName.startsWith("Mac OS")) {
            // from ./NyquistIDE.app/Contents/Resources/Java to .
            // note: I tried this using homeDir.getParentFile(), but
            // it always returns null, so I'm string parsing instead
            int i;
            for (i = 0;  i <  4; i++) {
                int pos = url.lastIndexOf('/');
                if (pos >= 0) {
                    url = url.substring(0, pos);
                }
                System.out.println(url);
            }
            url += "/nyquist";
        } else if (osName.startsWith("Windows")) {
	    // file://C:/ does not work, file:///C:/ does work 
	    url = "/" + url;
	}
        url = "file://" + url + "/doc/" + ext;
        return url;
    }
    
    public void doHelpManual(ActionEvent e) {
        if (prefInternalBrowser) {
            miniBrowser.setVisible(true);
            miniBrowser.setPage(findManualURL("title.html"));
        } else {
            // separate browser gets to use frames (with index) by 
            // opening home.html
            String url = findManualURL("home.html");
            System.out.println("Try to open: " + url);
            BareBonesBrowserLaunch.openURL(url);
        }
    }
    
    public void doProcessReplay(ActionEvent e)
    {
        callFunction("r", "");
    }
    
    public void disconnectEnv() { // no more envelopeFrame, so kill pointer
        envelopeEditor = null;
    }
    
    public void disconnectEq() { // no more equalizer panel, so kill pointer
        eqEditor = null;
    }

    /* BEGIN UPIC */
    public void disconnectUPIC() { // no more upicFrame, so kill pointer
        upicEditor = null;
    }
    /* END UPIC */
    
    public boolean workspaceWarning(String description) { // return true if OK to proceed
        if (workspaceLoaded) return true;
        Object[] options = { "CANCEL", "LOAD", "SKIP" };
        int i = JOptionPane.showOptionDialog(this,
                "No workspace has been loaded. If you save " + description +
                ",\nany existing workspace will be overwritten without\n" +
                "further notice. You should probably click CANCEL,\n" +
                "open workspace.lsp, load it into Nyquist, and then\n" +
                "revisit this editor.\n\n" +
                "Click SKIP to proceed at your own risk.\n" +
                "Click LOAD to load workspace.lsp from the current directory.\n" +
                "Click CANCEL to resume without opening a new editor window.\n\n" +
                "(If you have no workspace to load, select SKIP.)\n",
                "Warning",
                JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE,
                null, options, options[0]);
            System.out.println("dialog returns " + i);
        if (i == 2) { // skip (proceed) option
            workspaceLoaded = true;
        } else if (i == 1) { // load workspace
            callFunction("load", "\"workspace\"");
            i = 0;
            while (!workspaceLoaded && i < 10000) { // alow 10s
                try { Thread.sleep(200); }
                catch (InterruptedException ie) { }
                i += 200;
            }
            if (!workspaceLoaded) {
                JOptionPane.showMessageDialog(this,
                        "Timed out waiting for workspace load.\n" +
                        "Maybe it does not exist in this directory?",
                        "alert", JOptionPane.INFORMATION_MESSAGE);
            }
        }
        // otherwise OK_OPTION selected, return false meaning do not proceed
        return workspaceLoaded;
    }

    public void doWindowEnvelope(ActionEvent e)
    {
        // only one editor instance allowed
        if (envelopeEditor != null) {
            JOptionPane.showMessageDialog(this,
                        "Envelope editor is already open.",
                        "alert", JOptionPane.INFORMATION_MESSAGE);
            return;
        }
        // open an envelope window
        if (!workspaceWarning("envelopes")) return;
        final EnvelopeFrame envelopeFrame = 
                new EnvelopeFrame(this, jScrollPane.pane);
        final MainFrame mainFrame = this;
        envelopeEditor = envelopeFrame;
        envelopeFrame.validate();
        jDesktop.add(envelopeFrame);
        jDesktop.getDesktopManager().activateFrame(envelopeFrame);
        jDesktop.setSelectedFrame(envelopeFrame);
    }
    
    /*
    public void doNyquistlator(ActionEvent e)
    {
    	Nyquistlator nqltr = new Nyquistlator(this);
        //nqltr.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        nqltr.setVisible(true);
        nqltr.setClosable(true);
        nqltr.setMaximizable(true);
        nqltr.setIconifiable(true);
        
    	nqltr.validate();
    	jDesktop.add(nqltr);
        jDesktop.getDesktopManager().activateFrame(nqltr);
        jDesktop.setSelectedFrame(nqltr);
    }
    */

    /* BEGIN UPIC */
    public void doWindowUPIC(ActionEvent e)
    {
        // only one editor instance allowed
        if (upicEditor != null) {
            JOptionPane.showMessageDialog(this,
                        "UPIC editor is already open.",
                        "alert", JOptionPane.INFORMATION_MESSAGE);
            return;
        }
        // open a UPIC window
        final UPICFrame upicFrame = 
                new UPICFrame(this, jScrollPane.pane);
        upicEditor = upicFrame;
        upicFrame.validate();
        jDesktop.add(upicFrame);
        jDesktop.getDesktopManager().activateFrame(upicFrame);
        jDesktop.setSelectedFrame(upicFrame);
    }
    /* END UPIC */
    
    public void doWindowEQ(ActionEvent e) {
          /* Code added by Rivera Create a slider object that is the graphic equalizer
           * Then add that slider object to the desktop, It will display
           * directly to the right of the output, given the outputs frame
           * width of 500
          */
        if (eqEditor != null) {
            JOptionPane.showMessageDialog(this,
                        "Equalizer editor is already open.",
                        "alert", JOptionPane.INFORMATION_MESSAGE);
            return;
        }
        if (!workspaceWarning("equalizers")) return;
        final Jslide jslide = new Jslide(this);
        eqEditor = jslide;
        final MainFrame mainFrame = this;
        final JInternalFrame jEq = new JInternalFrame("Equalizer", true, true);
        jEq.setSize(new Dimension(350, 300));
        jEq.setLocation(500, 0);
        jEq.setVisible(true);
          
        jEq.getContentPane().add(jslide.getGraphEq());
        jDesktop.add(jEq);
        
        jEq.addInternalFrameListener(
            new InternalFrameListener() {
                public void internalFrameClosing(InternalFrameEvent e) {
                    //System.out.println("FrameClosing");
                    int r = JOptionPane.OK_OPTION;
                    if (jslide.modified) {
                        r = JOptionPane.showConfirmDialog(jEq,
                            "Really close without saving?",
                            "alert", JOptionPane.OK_CANCEL_OPTION);
                    }
                    if (r == JOptionPane.OK_OPTION) {
                        jEq.dispose();
                    }
                }
                public void internalFrameOpened(InternalFrameEvent e) {
                }
                public void internalFrameClosed(InternalFrameEvent e) {
                    mainFrame.disconnectEq();
                    //System.out.println("FrameClosed");
                }
                public void internalFrameIconified(InternalFrameEvent e) {
                }
                public void internalFrameDeiconified(InternalFrameEvent e) {
                }
                public void internalFrameActivated(InternalFrameEvent e) {
                }
                public void internalFrameDeactivated(InternalFrameEvent e) {
                }
            }
        );
        
        System.out.print("jOutputFrame size: ");
        System.out.println(jOutputFrame.getSize().toString());
        System.out.print("Available space in jDesktop: ");
        System.out.println(jDesktop.getInsets().toString());
        System.out.print("jDesktop size: ");
        System.out.println(jDesktop.getSize().toString());        
    }
        
    public void doProcessInfo(ActionEvent e)
    {
        callFunction("info", "");
    }
    
    public void doProcessBreak(ActionEvent e)
    {
        sendInput("\02\n");
    }
    
    public void doProcessCont(ActionEvent e)
    {
        callFunction("continue", "");
    }
    
    public void doProcessTop(ActionEvent e)
    {
        // don't use callFunction because isSal might be wrong
        // using (top) will work in both Lisp and Sal modes
        sendInputLn("(top)");
    }
    
    public void doProcessSal(ActionEvent e) {
        callFunction("sal", "");
    }
    
    public void doProcessLisp(ActionEvent e) {
        sendInputLn("exit");
    }
    
    public void doProcessUp(ActionEvent e)
    {
        callFunction("up", "");
    }
    
    public void doProcessMark(ActionEvent e)
    {
        sendInput(Character.toString('\001'), true); 
    }

    /* this is an old test function to simulate a slider change...
    public void doProcessTest(ActionEvent e)
    {
        sendInput(Character.toString('\016'), true); // begin message
        sendInput("S", true); // command is "slider change"
        test_value += 1.0;
        sendInput("5 ", true); // which slider
        sendInput(Float.toString(test_value), true); // new value
        sendInput(Character.toString('\021'), true); // end message
    }
    */
    
    public void doProcessFn(ActionEvent e)
    {
        callFunction(e.getActionCommand(), "");
    }
    
    // Plot command
    public void doProcessPlot(ActionEvent e) {
        NyqPlot.plot("points.dat", plotFrame);
    }
    
    
    // Process | Copy to Lisp Command
    public void doProcessCopyToLisp(ActionEvent e) {
        JInternalFrame frame = jDesktop.getSelectedFrame();
        if (frame instanceof NyquistFile) {
            NyquistFile file = (NyquistFile) frame;
            String selection = file.currentSelection();
            jScrollPane.pane.setText(selection);
            sendCommandToNyquist();
        }
    }
    
    // adjust completion window and output windows only
    public void tileCompletion() {
        // place output frame at left, full height
        Dimension dim = jDesktop.getSize();
        // something goes wrong at initialization, so hack in a reasonable value
        if (dim.width == 0 || dim.height == 0) {
            System.out.println("desktop size is zero, guessing 800 by 612");
            dim = new Dimension(800, 612);
        } else {
            System.out.println("desktop size is actually " + dim);
        }
        //System.out.print("jDesktop size: ");
        //System.out.println(dim.toString());
        int loc = (int) (dim.height * prefCompletionListPercent * 0.01);
        jOutputFrame.setLocation(0, loc);
        jListOutputFrame.setLocation(0, 0);
        // make output_width based on width of "desktop", which is the
        // area that contains the output frame and all the file (editor)
        // frames.
        int output_width = 530;
        if (dim.width < 600) output_width = dim.width - 100;
        if (output_width < 100) output_width = dim.width / 2;
        jOutputFrame.setSize(output_width, dim.height - loc);
        jListOutputFrame.setSize(output_width, loc);
        System.out.println("jListOutputFrame.setSize " + output_width + " " + loc + " " + dim);
    }    
    
    // Window Tile command -- organize window placement
    public void doWindowTile(ActionEvent e) {
        tileCompletion();
        // place output frame at left, full height
        Dimension dim = jDesktop.getSize();
        System.out.println("jDesktop.getSize(): " + dim);
        int output_width = 530; // for now this is constant, see tileCompletion
        
        // organize windows
        // if there are 3 or less or width is less than 1200,
        // use one column
        int cols = 1;
        JInternalFrame[] frames = jDesktop.getAllFrames();
        int num_frames = frames.length - 2; // don't count jOutput Frame or 
                                            // completion frame
        if (!miniBrowser.isVisible()) num_frames--; // don't count browser frame
        if (num_frames <= 0) return; // nothing to tile
        if (num_frames > 3 && dim.width >= 1200) {
            cols = 2;
        }
        int frames_per_col = (num_frames + cols - 1) / cols;
        int frame_spacing = dim.height / frames_per_col;
        // allow overlap if necessary
        int frame_height = Math.max(frame_spacing, 100); 
        int frame_width = (dim.width - output_width) / cols;
        int i;
        int col = 0;
        int row = 0;
        for (i = 0; i < frames.length; i++) {
            if (frames[i] != jOutputFrame && frames[i] != jListOutputFrame &&
                frames[i].isVisible()) {
                //NyquistFile nyquistFile = (NyquistFile) frames[i];
                JInternalFrame nyquistFile = (JInternalFrame) frames[i];
                nyquistFile.setLocation(output_width + col * frame_width, 
                                        row * frame_spacing);
                nyquistFile.setSize(frame_width, frame_height);
                row = row + 1;
                if (row >= frames_per_col) {
                    row = 0;
                    col = col + 1;
                }
            }
        }
    }
    
    // Window Browse command -- create a browse/demo window
    public void doWindowBrowse(ActionEvent e) {
        // place output frame at left, full height
        loadBrowserFrame();
    }
    
    //Overridden so we can exit (if confirmed) when window is closed
    protected void processWindowEvent(WindowEvent e) {
        // super.processWindowEvent(e);
        if (e.getID() == WindowEvent.WINDOW_CLOSING) {
            Quit();
        }
    }
    
    // convert backslash to escaped backslash in path for nyquist
    public String escape_backslashes(String path) {
        String escaped = "";
        int i = 0;
        while (i < path.length()) {
            char c = path.charAt(i);
            escaped = escaped + c;
            if (c == '\\') escaped = escaped + c;
            i++;
        }    
        return escaped;
    }
    
    // set current directory
    public void changeDirectory(String dir) {
        System.out.println("changeDirectory: currentDir " + currentDir +
                           " to " + dir);
        if (!currentDir.equals(dir)) {
            currentDir = dir;
            String escapedDir = escape_backslashes(dir);
            callFunction("setdir", "\"" + escapedDir + "\"");
        }
    }
    
    // tell nyquist to load a file
    public void loadFile(File file) {
        changeDirectory(fileDirectory(file));
        String path = escape_backslashes(file.getAbsolutePath());
        // if we're in lisp, pop out of any debug/break prompts before loading
        // don't do this is we're in sal because it will exit Sal - not good
        if (jScrollPane.isSal) {
            // callFunction would also work, but I prefer to use the "native"
            // Sal load command
            sendInputLn("load \"" + path + "\"");
        } else {
            callFunction("top", "");
            callFunction("sal-load", "\"" + path + "\"");
        }
    }

    
    // send data to Nyquist process after fixing indentation and appending 
    // a newline
    public void sendInputLn(String text) {
        // fix text with indentation if there are multiple lines
        String newlines = (jScrollPane.isSal ? "\n\n" : "\n");
        sendInput(
            text.replaceAll("\n", (jScrollPane.isSal ? "\n     " : "\n  ")) + 
            newlines, false);
    }
    
    public void setSalMode(final boolean sal) {
        jScrollPane.isSal = sal;
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                String name = (sal ? "Lisp" : "Sal");
                salLispButton.setText(name);
                salLispButton.setActionCommand(name);
                salLispButton.setToolTipText(sal ? "Switch to Lisp Mode" :
                                                   "Switch to SAL Mode");
            }
        });
    }
    
            
    // send data to Nyquist process
    public void sendInput(String text) {
        sendInput(text, false);
    }
    
    public void sendInput(String text, boolean hide) {
        SwingUtilities.invokeLater(update);
        System.out.println("sendInput: " + text + "(" + hide + ")");
        nyquistThread.sendInput(text, hide);
    }
    
    public void sendCommandToNyquist() {
        StringBuffer text = new StringBuffer(jScrollPane.pane.getText());
        inputStrings[inputStringsX] = new String(text);
        // for some reason, text sometimes ends up
        // with a CR LF at end. Make sure it's gone
        // before we output to Nyquist
        filterCRLF(text);
        // System.out.println("text |" + text +  "| pos " + pos);
        // SAL wants newline before multiline input to make input prettier
        //if (jScrollPane.isSal && 
        //    inputStrings[inputStringsX].indexOf("\n") >= 0)
        //    sendInput("\n");

        sendInputLn(inputStrings[inputStringsX]);
        // System.out.println("text sent to Nyquist");
        inputStringsX++;
        if (inputStringsX >= inputStringsLen) {
            inputStringsX = 0;
        }
        inputStringsCursor = inputStringsX;
        jScrollPane.pane.setText("");
    }
    
    public void ScrollToEnd() {
        JScrollBar scroll = jOutputPane.getVerticalScrollBar();
        scroll.setValue(scroll.getMaximum() - scroll.getVisibleAmount());
    }
    
    public void loadBrowserFrame() {
        Browser frame = new Browser(this, nyquistThread);
        
        // Validate frames that have preset sizes
        // Pack frames that have useful preferred size info, e.g. from their layout
        if (packFrame) {
            frame.pack();
        } else {
            frame.validate();
        }
        jDesktop.add(frame);
    }
    
    public void loadEnvData(String data) {
        if (envelopeEditor != null) {
            envelopeEditor.loadEnvData(data);
        }
        /* BEGIN UPIC */
        /* if (upicEditor != null) {
            upicEditor.loadEnvData(data);
        } */
        /* END UPIC */
    }

    public void loadEqData(String data) {
        if (eqEditor != null) {
            eqEditor.loadEqData(data);
        }
    }

    public void setFontSize(int s) {
        JInternalFrame[] frames = jDesktop.getAllFrames();
        int i;
        for (i = 0; i < frames.length; i++) {
            if (frames[i] instanceof NyquistFile) {
                NyquistFile nyquistFile = (NyquistFile) frames[i];
                CodePane pane = nyquistFile.filePane;
                pane.setFontSize(s);
            }
        }
        jScrollPane.setFontSize(s);
        jOutputArea.setFont(new Font("Courier", Font.PLAIN, s));
    }

}

