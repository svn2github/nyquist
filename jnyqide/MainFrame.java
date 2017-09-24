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
import javax.swing.filechooser.FileFilter;
import java.io.*;
import jnyqide.*;
// this won't work on non-Mac, so use reflection tricks below
// import com.apple.mrj.*; // import Mac menu support
import java.lang.reflect.*; // import Constructor class
import java.util.prefs.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.net.URL;
import java.net.URLDecoder;
import javax.swing.border.EmptyBorder;
import java.nio.file.Paths;
import java.nio.file.Files;
import java.lang.ProcessBuilder;
import javax.imageio.ImageIO;

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
    // ImageIcon image1;
    // ImageIcon image2;
    // ImageIcon image3;
    public JLabel statusBar = new JLabel();
    JButton salLispButton;
    BorderLayout borderLayout1 = new BorderLayout();
    JDesktopPane jDesktop;
    public CodePane codeInputPane;
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
    public static String currentDir = "";
    public static String nyquistDir = "";
    public static String docDir = ""; // the nyquist/doc directory
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
    public static final boolean prefLastDirectoryDefault = true;

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
    // if Nyquist should use the last-used directory on start-up, this
    //   is set to true
    public static boolean prefLastDirectory = prefLastDirectoryDefault;
    //   otherwise, the prefDirectory (if non-empty) is used as the
    //   start-up directory. The prefDirectory is retained even if
    //   lastDirectory is set, and we revert to it if lastDirectory is
    //   turned off.
    public static String prefDirectory = "";
    // directory when Nyquist was last closed: If starting directory is not
    //   given, we'll open the directory in use the last time NyquistIDE ran:
    public static String lastDirectory = "";
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
    public MainFrame(String[] args) {
        enableEvents(AWTEvent.WINDOW_EVENT_MASK);
        try {
            mainFrameInit(args);
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }
    
    
    public boolean isMac() {
        // the following suggested by Raymond Martin:
        final String strOS;

        try { strOS = System.getProperty("os.name"); }
        catch(final SecurityException e) {
            System.out.println("In isMac: error " + e);
            return(false); 
        }
        System.out.println("strOS " + strOS);
        return(strOS != null && strOS.indexOf("Mac OS") >= 0);
    }

    PreferencesDialog preferencesDialog;
    
    public void disconnectPreferences() {
        preferencesDialog = null;
    }
    
    
    protected void setVariable(String var, String val) {
        String input;
        if (codeInputPane.isSal) {
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
        if (codeInputPane.isSal) {
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

    // if docDir is set, we try to make symbolic links from docDir to
    // the Application. Assumes docDir is valid.
    private void setupLibAndDemosLinks() {
        try {
            // first, see if there is already a lib in docDir:
            File libFile = new File(docDir + "../lib");
            if (libFile.exists() && !Files.isSymbolicLink(libFile.toPath())) {
                System.out.println(docDir + "../lib already exists");
                return; // seems to be a copy of lib there already,
                        // don't mess with it
            }
            File demosFile = new File(docDir + "../demos");
            if (demosFile.exists() && !Files.isSymbolicLink(demosFile.toPath())) {
                System.out.println(docDir + "../demos already exists");
                return; // don't mess with demos either
            }
            // create missing links. If both lib and demos are symbolic links,
            // NyquistIDE.app might have moved, so overwrite the links to make
            // sure they are up-to-date
            File libTarget = new File(nyquistDir + "lib");
            if (libTarget.exists()) { 
                if (libFile.exists()) { // must be a link, we can update it
                    Files.delete(libFile.toPath());
                }
                Files.createSymbolicLink(libFile.toPath(),
                                         libTarget.toPath());
                System.out.println(docDir + "../lib linked to " +
                                   libTarget.getAbsolutePath());
            } else {
                System.out.println("not linked because " +
                        libTarget.getAbsolutePath() + " doesn't existt");
            }
            File demosTarget = new File(nyquistDir + "demos");
            if (demosTarget.exists()) {
                if (demosFile.exists()) { // must be a link, we can update it
                    Files.delete(demosFile.toPath());
                }
                Files.createSymbolicLink(demosFile.toPath(),
                                         demosTarget.toPath());
                System.out.println(docDir + "../demos linked to " +
                                   demosTarget.getAbsolutePath());
            } else {
                System.out.println("not linked because " +
                        demosTarget.getAbsolutePath() + " doesn't existt");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        
    }
	/* THIS CODE WRITTEN FOR JAVA SE 7 -- NOT TESTED, USING SHELL SCRIPT INSTEAD
	// create a symbolic link if it is not already there
	private static void createSymbolicLinkConditional(Path link, File target) {
		try {
			Path targetPath = Files.readSymbolicLink(link);
			if (targetPath.equals(target.toPath())) {
				System.out.println("link " + link + " points to " + 
								  target + " already");
				return;
			}
		} catch(Exception e) {
			System.out.println("Failed to read " + link);
		}
		Files.createSymbolicLink(link, target.toPath());
	}
	*/

    private boolean isTrue(String s) { return s != null && s.equals("true"); }

    // save the location of nyquist/doc in a file where we start
    // This is not in prefs because you might have another NyquistIDE
    // installation and we want each installation/version to have it's
    // own copy of the Nyquist library, documentation, etc.
    private void writeDocDirHint(String hint) {
        try {
            File file = new File(nyquistDir + "doc-dir-hint.txt");
			FileWriter fileWriter = new FileWriter(file);
			fileWriter.write(hint);
			fileWriter.flush();
			fileWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void setupDocDir() {
        // look in preferences for a hint
        String hint = "";
        try {
            hint = new String(Files.readAllBytes(
                    Paths.get(nyquistDir + "doc-dir-hint.txt")));
            System.out.println("Read doc-dir-hint.txt: " + hint);
        } catch (IOException e) { // not an error
            System.out.println("Could not find doc-dir-hint.txt - probably ok");
        }
        try {
            // guess that nyquist is next to NyquistIDE.app
            // because that's where it is in the download
            if (hint.equals("")) {
                hint = new File(currentDir + "../../../nyquist/doc").
                                getCanonicalPath() + "/";
                System.out.println("docDir guess: " + hint);
            }
        } catch (IOException e) {
                e.printStackTrace();
        }
        File hintedFile = null;
        File docDirFile = null;

        // validate the hint
        if (hint != null && !hint.equals("") && hint.endsWith("/doc/")) {
            // see if the directory exists
            hintedFile = new File(hint);
            if (hintedFile.isDirectory()) {
                docDir = hint;
                System.out.println("assuming docDir is: " + hint);
                return;
            }
        }
        // hint failed; ask the user for the nyquist directory
        String msg[] = {
            "In order to show you documentation, NyquistIDE needs",
            "to know where you installed the \"nyquist\" directory",
            "that came in the same folder as NyquistIDE.app. The",
            "next screen will be a file chooser so you can find",
            "the \"nyquist\" directory. Click OK to continue." };
        JOptionPane.showMessageDialog(this, msg,
                    "Notice", JOptionPane.INFORMATION_MESSAGE);
        JFileChooser fd = new JFileChooser(
                "Select the folder named nyquist that you installed");
        fd.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        // if user hits cancel on file chooser and ignores warnings,
        // after 3 tries, we give up
        for (int i = 0; i < 3; i++) {
            System.out.println("opening fd file dialog");
            fd.showOpenDialog(this);
            File file = fd.getSelectedFile();
            if (file != null) {
                // I tried to use the parent,child form of constructor
                // but couldn't get it to work, so just use path+"/doc"
                docDirFile = new File(file.getAbsolutePath() + "/doc");
                if (docDirFile.isDirectory()) {
                    docDir = docDirFile.getAbsolutePath() + "/";
                    writeDocDirHint(docDir);
                    System.out.println("docDir set to " + docDir);
                    return;
                }
            }
            String msg2[] = {"That directory does not contain \"doc\" as ",
                             "expected, so something is wrong. Try again",
                             "to find the \"nyquist\" folder?"};
            int r = JOptionPane.showConfirmDialog(this, msg2, 
                             "alert", JOptionPane.OK_CANCEL_OPTION);
            if (r != JOptionPane.OK_OPTION) {
                String msg3[] =
                          { "NyquistIDE did not find nyquist/doc folder.",
                            "Help:Manual and other documentation links",
                            "may not work. If you do not have nyquist/doc,",
                            "reinstall NyquistIDE." };
                JOptionPane.showMessageDialog(this, msg3,
                            "Notice", JOptionPane.INFORMATION_MESSAGE);
                        
                return; // docDir is empty!!! Help:Manual will not work
            }
        }
        return; // docDir is empty!!! Help:Manual will not work
    }

    
    //Component initialization
    private void mainFrameInit(String[] cmdlineArgs)  throws Exception  {
        // if this is a Mac, we want some menu items on the application menu, 
        // which is accessed via some special apple code, but the apple code 
        // is not going to be installed if you are running windows or linux,
        // so we use java reflection here to load the special code ONLY if 
        // we're on a Mac
        //
        // The special code ultimately calls back to the methods About(), 
        // Prefs(), and Quit() in this class. The "normal" Windows/Linux 
        // code should do the same.
		//
		// Also, if this is a Mac, the lib, demo, and runtime directories
		// are effectively hidden inside the application bundle. To make
		// them more accessible, look in the directory containing
		// the application bundle for a directory named nyquist. This is
		// normally created to hold documentation. Add links in nyquist
		// to lib and demo. If we do not find the directory, tell user to
        // set the directory in Preferences (because maybe we are in a sandbox).
		//
        System.out.println("mainFrameInit output test, args are:\n---");
        for (String arg : cmdlineArgs) {
            System.out.println(arg);
        }
        System.out.println("---\nisMac(): " + isMac() + "\n");

        // "h" in cmdlineArgs means clear the hints for testing
        if (cmdlineArgs.length > 0 && cmdlineArgs[0].contains("h")) {
            writeDocDirHint("");
        }
        // set current working directory if we are in an application bundle
        currentDir = Paths.get(MainFrame.class.getProtectionDomain().
                    getCodeSource().getLocation().toURI()).getParent().
                    toString() + "/";
        prefs = Preferences.userNodeForPackage(Main.class);
        if (isMac()) {
            hasRightMouseButton = false;
            if (isTrue(System.getProperty("isOSXbundle"))) {
                System.out.println("isOSXbundle: true\n");
                nyquistDir = currentDir;
                // docDir = findDocDir();
                //docDir = new File(currentDir + "../../../nyquist/doc").
                //         getCanonicalPath() + "/";
            }
            // Debugging:
            // System.out.println("currentDir: |" + currentDir + "|");
            // System.out.println("nyquistDir: |" + nyquistDir + "|");
            // System.out.println("docDir: |" + docDir + "|");
            //
            // try to load special mac-specific code that won't even compile
            // on windows or linux
            try {
                Object[] args = { this };
                Class[] arglist = { MainFrame.class };

                Class mac_class = Class.forName("jnyqide.SpecialMacHandler");

                System.out.println("got the class\n");
                Constructor new_one = mac_class.getConstructor(arglist);
                System.out.println("got the constructor\n");
                new_one.newInstance(args);

                System.out.println("isMac, so created instance of SpecialMacHandler");

                /*
				ProcessBuilder process = new ProcessBuilder(
                        "/bin/sh", "mac-os-x-link-script.sh");
                process.redirectErrorStream(true).inheritIO();
                process.directory(new File(currentDir));
                Process p = process.start();
				int result = p.waitFor();
				// if this fails, something is wrong, but it would be annoying to
				// pop up a dialog box every time the user starts NyquistIDE, so 
				// we'll just print the error to aid with debugging and ignore the
				// failure, which could happen if the user intentionally write
				// protected nyquist (not a bad idea, really), or removed it.
				System.out.println("Ran script to install lib and demos links,");
				System.out.println("   -> result is " + result + " (0 == success)");
                */
            } catch(Exception e) {
                System.out.println(e);
            }
        } else { // Linux and Windows
            String icon_path = currentDir + "nycon.png";
            Image im = null;
            try {
                im = ImageIO.read(new File(icon_path));
            } catch (IOException ex) {
                System.out.println(ex);
            }
            System.out.println("nycon" + im + " from " + icon_path);
            setIconImage(im);
        }
        // Linux, Windows, and Mac debugging from command line are handled here
		if (nyquistDir.equals("")) {
             nyquistDir = new File(currentDir + "..").getCanonicalPath() + "/";
             docDir = nyquistDir + "/doc/";
        }
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
        prefLastDirectory = prefs.getBoolean("use-last-directory",
                                             prefLastDirectory);
        lastDirectory = prefs.get("last-directory", lastDirectory);
        prefSFDirectory = prefs.get("default-sf-directory", prefSFDirectory);
        prefsHaveBeenSet = false;

        // image1 = new ImageIcon("openFile.gif");
        // image2 = new ImageIcon("closeFile.gif");
        /// image3 = new ImageIcon("help.gif");

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
        // buttonInit("Test", "This should not be in released version.", menuButtonListener);

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
        // jOutputArea.append("currentDir: |" + currentDir + "|\n");
        // jOutputArea.append("nyquistDir: |" + nyquistDir + "|\n");
        // jOutputArea.append("docDir: |" + docDir + "|\n");
        
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
                    openManual(ext);
            	} else {
                    // System.out.println(e.paramString());
            		if (line.length() > 0)
            			WordList.replaceWithTemplate(line);
                }
            }
        });
        jListOutputPane = new JScrollPane( jListOutputArea );
        jListOutputPane.setHorizontalScrollBarPolicy(
                JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        jListOutputPane.setVerticalScrollBarPolicy(
                JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    
        codeInputPane = new CodePane(new Dimension(400, 200), this, statusBar,
                                   Integer.parseInt(prefFontSize));
        
        // Top panel for command entry, plot, and toolbar
        JPanel jCommands = new JPanel( new BorderLayout() );
        JPanel jInputAndPlot = new JPanel(new BorderLayout(3, 0));
        jInputAndPlot.add(codeInputPane, BorderLayout.WEST);
        jCommands.add(jToolBar, BorderLayout.SOUTH);
        jCommands.add(jInputAndPlot, BorderLayout.CENTER);
        jCommands.setPreferredSize(new Dimension(300, 150));
        
        // Main desktop
        jDesktop = new JDesktopPane();
        jDesktop.setBorder(new EmptyBorder(80, 0, 0, 0));
        jDesktop.setPreferredSize( new Dimension(300, 300) );
        
        jOutputFrame = new JNonHideableInternalFrame("Output");
       
        // make this wide enough so XLISP GC messages do not wrap 
        //   (it's annoying)
        //jOutputFrame.setSize(new Dimension(500, 530 / 3));
        jOutputFrame.setVisible(true);
        jDesktop.setLayout(null);
        jOutputFrame.getContentPane().add(jOutputPane);
        jOutputFrame.setResizable( true );
        //jOutputFrame.setLocation(0, (530 * 2) / 3);
        jDesktop.add( jOutputFrame );
        
        
        String clTitle = "Completion List" + (hasRightMouseButton ?
                                              " - Right Click for Help" :
                                              " - Option Click for Help");
        jListOutputFrame = new JNonHideableInternalFrame(clTitle);
        jListOutputFrame.setBounds(0, 0, 0, 0);
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
        miniBrowser.setBounds(50, 100, 700, 400);
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
                   System.out.println("Drop failed: " + e.getMessage());
                }
                return false;
            }
        });
        // set size and location for jOutputFrame and jListOutputFrame
        // now this is done in Main after making desktop visible (otherwise
        // you can't get dimensions of the desktop and layout is faulty)
        // tileCompletion(); 

        // set directory according to preferences or user.dir:
        if (lastDirectory.length() == 0) {
            lastDirectory = System.getProperty("user.dir");
            System.out.println("lastDirectory was empty, set to " + lastDirectory);
        }

        // do not trust currentDir to be where user put
        // NyquistIDE.app -- OS X 10.12 relocates it to a
        // randomized location, so we'll ask the user to find it
        //
        // the following is here because when we directly open a file chooser
        // dialog box at this point, OS X apps hang. I don't know why, but
        // certainly dialog boxes work in an initialized, running program,
        // so that's when we'll try to complete the initialization.
        if (isMac()) {
            SwingUtilities.invokeLater(
                    new Runnable() { public void run() {
                        setupDocDir();
                        if (!docDir.equals("")) {
                            setupLibAndDemosLinks();
                        }
                    }});
        }
    }
    
    
    public void openManual(String ext) {
        String url = (prefOnlineManual ? onlineManualURL : 
                                         "file://" + docDir) +
                     ext;
        
        if (prefInternalBrowser) {
            miniBrowser.setVisible(true);
            System.out.println("Mini browser URL is: " + url);
            miniBrowser.setPage(url);
        } else {
            System.out.println("BareBonesBrowserLaunch URL is: " + 
                               url);
            BareBonesBrowserLaunch.openURL(url);
        }
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
        if (prefLastDirectory && lastDirectory.length() > 0) {
            changeDirectory(lastDirectory);
        } else if (prefDirectory.length() > 0) {
            changeDirectory(prefDirectory);
        }
        System.out.println("sendPreferenceData: prefDir " + prefDirectory + 
                           " lastDir " + lastDirectory);

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
        if (cmd.equals("New") || cmd.equals("New File")) doFileNew(e);
        else if (cmd.equals("Open") || cmd.equals("Open File")) doFileOpen(e);
        else if (cmd.equals("Save") || cmd.equals("Save File")) doFileSave(e);
        else if (cmd.equals("Save As...")) doFileSaveAs(e);
        else if (cmd.equals("Load") || cmd.equals("Load...")) doFileLoad(e);
        else if (cmd.equals("Mark")) doProcessMark(e);
        else if (cmd.equals("Test")) doProcessTest(e); // usually disabled
        else if (cmd.equals("Preferences...")) Prefs();
        else if (cmd.equals("Exit")) {
            SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        Quit();
                    }
                });
            // throw new IllegalStateException("Let the quit handler do it");
        } else if (cmd.equals("Find...")) doEditFind(e);
        else if (cmd.equals("Replace...")) doEditReplace(e);
        else if (cmd.equals("Previous")) doEditPrevious(e);
        else if (cmd.equals("Next")) doEditNext(e);
        else if (cmd.equals("Select Expression")) doEditSelectExpression(e);
        else if (cmd.equals("About")) About();
        else if (cmd.equals("Manual")) doHelpManual(e);
        else if (cmd.equals("Replay")) doProcessReplay(e);
        else if (cmd.equals("Plot")) doProcessPlot(e);
        else if (cmd.equals("Tile")) doWindowTile(e);
        else if (cmd.equals("Browse")) doWindowBrowse(e);
        else if (cmd.equals("EQ")) doWindowEQ(e);
        else if (cmd.equals("EnvEdit") || cmd.equals("Envelope Edit")) 
            doWindowEnvelope(e);
        // else if (cmd.equals("Nyquistlator")) doNyquistlator(e);
        /* BEGIN UPIC */
        else if (cmd.equals("UPIC Edit")) doWindowUPIC(e);
        /* END UPIC */
        else if (cmd.equals("Info")) doProcessInfo(e);
        else if (cmd.equals("Break")) doProcessBreak(e);
        else if (cmd.equals("Up")) doProcessUp(e);
        else if (cmd.equals("Cont")) doProcessCont(e);
        else if (cmd.equals("Top")) doProcessTop(e);
        else if (cmd.equals("Lisp")) doProcessLisp(e);
        else if (cmd.equals("Sal")) doProcessSal(e);
        else if (cmd.equals("Copy to Lisp")) doProcessCopyToLisp(e);
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
            prefs.putBoolean("use-last-directory", prefLastDirectory);
            prefs.put("last-directory", currentDir);
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
        if (codeInputPane.isSal) sendInputLn("exit nyquist");
        else                     callFunction("exit", ""); 

        System.out.println("Exiting from NyquistIDE");
		for (i = 0; i < 10; i++) {
            try {
                Thread.sleep(200); // does it help Nyquist's exit to stall? 
            } catch (InterruptedException ie) {
            }
			if (!nyquistThread.nyquist_is_running) break;
		}
		if (nyquistThread.nyquist_is_running) {
            nyquistThread.myProcess.destroy(); // make sure it dies
		}
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
        FileDialog fileDialog = new FileDialog(this, "Select a File to Open",
                                               FileDialog.LOAD);
        NyquistFileFilter nyquistFilter = new NyquistFileFilter();
        fileDialog.setFile("*.sal");
        fileDialog.setFilenameFilter(nyquistFilter);
        // note if current directory setting fails on some platform,
        // consider this code using getCanonicalPath():
        //      File f = new File(new File(".").getCanonicalPath());
        fileDialog.setDirectory(currentDir);
        fileDialog.setVisible(true);
        if (fileDialog.getDirectory() != null && fileDialog.getFile() != null) {
            String path = fileDialog.getDirectory() + 
                    System.getProperty("file.separator") + 
                    fileDialog.getFile();
            System.out.println("You chose to open this file: " + path);
            openFile(new File(path));
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
            codeInputPane.pane.setText(trimNewline(text));
        }
    }
    
    //Edit | Next action performed
    public void doEditNext(ActionEvent e) {
        inputStringsCursor = inputStringsCursor + 1;
        if (inputStringsCursor >= inputStringsLen) inputStringsCursor = 0;
        String text = inputStrings[inputStringsCursor];
        if (text != null) {
            codeInputPane.pane.setText(trimNewline(text));
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
        dlg.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x,
                        (frmSize.height - dlgSize.height) / 2 + loc.y);
        dlg.setModal(true);
        dlg.setVisible(true);
        
        Graphics g = jFrame.getContentPane().getGraphics();
        g.setColor(Color.cyan);
        g.fillRect(50, 50, 100, 100);
        
    }
    

    public void doHelpManual(ActionEvent e) {
        // separate browser gets to use frames (with index) by 
        // opening home.html
        String ext = (prefInternalBrowser ? "title.html" : "home.html");
        openManual(ext);
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
                new EnvelopeFrame(this, codeInputPane.pane);
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
                new UPICFrame(this, codeInputPane.pane);
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

    // Slider panel management
    HashMap<String, Nsliders> sliderPanels = new HashMap<String, Nsliders>();
    Nsliders activeSliderPanel;
    String activeSliderPanelName;

    /* this is a test function to simulate creating a slider panel */
    /* or you can put any other test in here - it should not be in release */
    public void doProcessTest(ActionEvent e) {
        /*        createSliderPanel("PanelName2", 3);
        createSlider("S1", 10, 0.3, 0.0, 2.0);
        createSlider("S2Long", 11, 0.3, 0.0, 1.0);
        */
        /*
        // System.setProperty("apple.awt.fileDialogForDirectories", "true");
        JFileChooser xxx = new JFileChooser();
        System.out.println("xxx open ...");
        xxx.showOpenDialog(this);
        System.out.println("xxx open returned");
        */
    }

    public void createSliderPanel(String name, int color) {
        Nsliders old = sliderPanels.get(name);
        if (old != null) { // already exists: get rid of it
            old.dispose();
        }
        activeSliderPanel = new Nsliders(name, color, this);
        sliderPanels.put(name, activeSliderPanel);
        jDesktop.add((JInternalFrame) activeSliderPanel);
    }

    public void deleteSliderPanel(String name) {
        Nsliders old = sliderPanels.get(name);
        if (old != null) { // already exists: get rid of it
            old.dispose();
        }
    }

    public void createSlider(String name, int num, double init, 
                             double low, double high)
    {
        if (activeSliderPanel == null) return;
        activeSliderPanel.addSlider(name, num, init, low, high);
    }        


    public void createButton(String name, int num, int normal)
    {
        if (activeSliderPanel == null) return;
        activeSliderPanel.addButton(name, num, normal);
    }        


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
            codeInputPane.pane.setText(selection);
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
        // currentDir must be "" or end in "/"
        if (dir.length() > 0 && dir.charAt(dir.length() - 1) != '/') {
            dir = dir + "/";
        }
        System.out.println("changeDirectory: currentDir " + currentDir +
                           " to " + dir);
        if (!currentDir.equals(dir)) {
            currentDir = dir;
            prefsHaveBeenSet = true;
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
        if (codeInputPane.isSal) {
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
        String newlines = (codeInputPane.isSal ? "\n\n" : "\n");
        sendInput(
            text.replaceAll("\n", (codeInputPane.isSal ? "\n     " : "\n  ")) + 
            newlines, false);
    }
    
    public void setSalMode(final boolean sal) {
        codeInputPane.isSal = sal;
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
        StringBuffer text = new StringBuffer(codeInputPane.pane.getText());
        inputStrings[inputStringsX] = new String(text);
        // for some reason, text sometimes ends up
        // with a CR LF at end. Make sure it's gone
        // before we output to Nyquist
        filterCRLF(text);
        // System.out.println("text |" + text +  "| pos " + pos);
        // SAL wants newline before multiline input to make input prettier
        //if (codeInputPane.isSal && 
        //    inputStrings[inputStringsX].indexOf("\n") >= 0)
        //    sendInput("\n");

        sendInputLn(inputStrings[inputStringsX]);
        // System.out.println("text sent to Nyquist");
        inputStringsX++;
        if (inputStringsX >= inputStringsLen) {
            inputStringsX = 0;
        }
        inputStringsCursor = inputStringsX;
        codeInputPane.pane.setText("");
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
        codeInputPane.setFontSize(s);
        jOutputArea.setFont(new Font("Courier", Font.PLAIN, s));
    }

}

