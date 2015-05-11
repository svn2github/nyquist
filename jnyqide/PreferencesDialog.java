package jnyqide;
/*
 * Preferences dialog based on ReplaceDialog.java
 */

// Current elements are:
//
//   Restore Defaults
//   [] Automatically insert close-parentheses
//   <-|--> Relative height of completion box
//


import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.File;

import com.sun.corba.se.spi.servicecontext.SendingContextServiceContext;

import jnyqide.*;

class PreferencesDialog extends JInternalFrame implements ActionListener {
    private MainFrame mainFrame;
    private JScrollPane scrollPane;
    private JPanel panel;
    private GridBagConstraints c = new GridBagConstraints();
    private JButton defaultPrefs;  // "Restore Defaults"
    private JCheckBox startInSalMode; // "Start in SAL mode (not Lisp)"
    private JCheckBox salShowLisp; // "Show translation of SAL to Lisp"
    private JCheckBox parenAutoInsert; 
                      // "Automatically insert close-parentheses"
    private JCheckBox enableSound; // "Enable sound output in PLAY command"
    private JCheckBox autoNorm; // "AutoNorm"
    private JCheckBox salTraceBack; // "Print SAL traceback on SAL error"
    private JCheckBox salBreak; // "Enable XLISP break on SAL error"
    private JCheckBox xlispBreak; // "Enable XLISP break on XLISP error"
    private JCheckBox xlispTraceBack; // "Print XLISP traceback on XLISP error"
    private JCheckBox printGC; // "Print info about garbage collection"
    private JCheckBox fullSearch;
                      // "Use full search for code completion"
    private JCheckBox internalBrowser;
                      // "Use window in jNyqIDE for help browser"
    private JCheckBox onlineManual; 
                      // "Use online manual instead of local copy"
    private JScrollBar completionListPercent; 
                       // "Relative height of completion box"
    private JComboBox audioRate; // "Audio Sample Rate"
    private JTextField controlRate; // "Control Sample Rate"
    private JComboBox fontSize; // "Font Size"
    private JButton sfDirectory; // "Set Default Sound File Directory"
    private JButton initialDirectory; // "Set Initial Directory"
    private JButton lastDirectory; // "Last Used Directory is Initial Directory"
    private JFileChooser startfd;
    private JFileChooser fd;
    private String[] audioRates = { "96000", "48000", "44100", "22050", "16000",
                                    "11025", "8000" };
    private String currentFontSize;
    private String[] fontSizes = { "6", "7", "8", "9", "10", "11", "12", "14", 
                                   "16", "18", "20", "24", "28", "32", "36" };
    protected JCheckBox makeCheckBox(String doc, boolean val) {
        JCheckBox cb = new JCheckBox(doc, val);
        c.gridy += 1;
        panel.add(cb, c);
        return cb;
    }
                                    
    public PreferencesDialog(MainFrame mainFrame_) {
        super();
        final PreferencesDialog preferencesDialog = this;
        //super(mainFrame_, "Preferences", true); // initialize Dialog
        setTitle("Preferences");

        mainFrame = mainFrame_;
                
        addInternalFrameListener(new InternalFrameListener() {
            public void internalFrameClosing(InternalFrameEvent e) {
                mainFrame.prefStartInSalMode = startInSalMode.isSelected();

                boolean enable = salShowLisp.isSelected();
                mainFrame.setBoolean("*sal-compiler-debug*", enable);
                mainFrame.prefSalShowLisp = enable;
                
                mainFrame.prefParenAutoInsert = parenAutoInsert.isSelected();

                enable = enableSound.isSelected();
                mainFrame.callFunction(
                        enable ? "sound-on" : "sound-off", "");
                mainFrame.prefEnableSound = enable;
                
                enable = autoNorm.isSelected();
                mainFrame.callFunction(
                        enable ? "autonorm-on" : "autonorm-off", "");
                mainFrame.prefAutoNorm = enable;

                enable = salTraceBack.isSelected();
                mainFrame.callFunction("sal-tracenable",
                                       mainFrame.tOrNil(enable));
                mainFrame.prefSalTraceBack = enable;
                
                enable = salBreak.isSelected();
                mainFrame.callFunction("sal-breakenable",
                                       mainFrame.tOrNil(enable));
                mainFrame.prefSalBreak = enable;

                enable = (xlispBreak.isSelected() || xlispTraceBack.isSelected());
                mainFrame.callFunction("xlisp-breakenable", 
                                       mainFrame.tOrNil(enable));
                mainFrame.prefXlispBreak = enable;

                enable = xlispTraceBack.isSelected();
                mainFrame.callFunction("xlisp-tracenable",
                                       mainFrame.tOrNil(enable));
                mainFrame.prefXlispTraceBack = enable;

                enable = printGC.isSelected();
                if (enable != mainFrame.prefPrintGC) {
                    mainFrame.setBoolean("*gc-flag*", enable);
                    mainFrame.prefPrintGC = enable;
                }
                
                mainFrame.prefFullSearch = fullSearch.isSelected();

                mainFrame.prefInternalBrowser = internalBrowser.isSelected();

                mainFrame.prefOnlineManual = onlineManual.isSelected();
                
                if ((int) mainFrame.prefCompletionListPercent != 
                    (int) (completionListPercent.getValue())) {
                    mainFrame.prefCompletionListPercent = 
                            completionListPercent.getValue();
                    mainFrame.tileCompletion();
                }
                
                String rateString = (String) audioRate.getSelectedItem();
                //audioRate.setSize(50, 20);
                int rate = validate(rateString);
                if (rate > 0 && !rateString.equals(mainFrame.prefAudioRate)) {
                    mainFrame.callFunction("set-sound-srate", rateString);
                    mainFrame.prefAudioRate = rateString;
                }

                rateString = controlRate.getText();
                rate = validate(rateString);
                if (rate > 0 && !rateString.equals(mainFrame.prefControlRate)) {
                    mainFrame.callFunction("set-control-srate ", rateString);
                    mainFrame.prefControlRate = rateString;
                }

                String fontString = (String) fontSize.getSelectedItem();
                int size = validate(fontString);
                if (size > 0 && !fontString.equals(mainFrame.prefFontSize)) {
                    mainFrame.prefFontSize = fontString;
                    mainFrame.setFontSize(size);
                }

                File file = startfd.getSelectedFile();

                System.out.println("startfd.getSelectedFile() -> " + file);

                if (file != null) {
                    String dir = file.toString().replaceAll("\\\\", "/");
                    System.out.println("startfd.getSelectedFile: " + dir);
                    if (dir != null && dir.length() > 0) {
                        mainFrame.prefDirectory = dir;
                        mainFrame.changeDirectory(dir);
                    }
                } else {
                    mainFrame.prefDirectory = "";
                }
                        
                file = fd.getSelectedFile();
                if (file != null) {
                    String dir = file.toString().replaceAll("\\\\", "/");
                    System.out.println("fd.getSelectedFile: " + dir);
                    if (dir != null && dir.length() > 0) {
                        mainFrame.prefSFDirectory = dir;
                        mainFrame.setVariable("*default-sf-dir*",
                                              "\"" + dir + "\"");
                    }
                } else {
                    mainFrame.prefSFDirectory = "";
                }

                mainFrame.prefsHaveBeenSet = true;
                dispose();
            }

            public void internalFrameOpened(InternalFrameEvent e) {
            }
            public void internalFrameClosed(InternalFrameEvent e) {
                mainFrame.disconnectPreferences();
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
        });

        panel = new JPanel();
        scrollPane = new JScrollPane(panel);
        JPanel contentPane = (JPanel) getContentPane();
        contentPane.add(scrollPane, BorderLayout.CENTER);
        //contentPane
        panel.setLayout(new GridBagLayout());
        c.fill = GridBagConstraints.VERTICAL;
        c.gridx = 0;
        c.gridy = 0;
        c.gridwidth = 2;
        c.anchor = GridBagConstraints.LINE_START;
        panel.add(new JLabel("Preferences are updated when you close this"), c);

        c.gridy += 1;
        panel.add(new JLabel("     Preferences Window"), c);

        c.gridy += 1;
        panel.add(Box.createRigidArea(new Dimension(0, 10)), c);

        // button to restore default preferences
        defaultPrefs = new JButton("Restore Defaults");
        defaultPrefs.addActionListener(this);
        // defaultPrefs.setAlignmentX(Component.LEFT_ALIGNMENT);
        c.gridy += 1;
        panel.add(defaultPrefs, c);

        c.gridy += 1;
        panel.add(Box.createRigidArea(new Dimension(0, 10)), c);
        
        // Start in Sal mode (not Lisp)"
        startInSalMode = makeCheckBox("Start in SAL mode (not Lisp)", 
                                    mainFrame.prefStartInSalMode);
        // Show translation of SAL to Lisp
        salShowLisp = makeCheckBox("Show translation of SAL to Lisp",
                                   mainFrame.prefSalShowLisp);
        // Automatically insert close-parenthesis (checkbox)
        parenAutoInsert = makeCheckBox(
                                "Automatically insert close-parentheses",
                                mainFrame.prefParenAutoInsert);
        // Enable sound output (checkbox)
        enableSound = makeCheckBox("Enable sound output in PLAY command",
                                   mainFrame.prefEnableSound);
        // AutoNorm (checkbox)
        autoNorm = makeCheckBox("AutoNorm", mainFrame.prefAutoNorm);
        
        // Enable SAL Stack Traceback on Error
        salTraceBack = makeCheckBox("Print SAL traceback on SAL error",
                                    mainFrame.prefSalTraceBack);
        // break into XLISP debugger on SAL error
        salBreak = makeCheckBox("Enable XLISP break on SAL error",
                                 mainFrame.prefSalBreak);
        // Enable XLISP Break when XLISP encounters error
        xlispBreak = makeCheckBox("Enable XLISP break on XLISP error",
                                  mainFrame.prefXlispBreak);
        // print XLISP TraceBack on XLISP error
        xlispTraceBack = makeCheckBox("Print XLISP traceback on XLISP error",
                                       mainFrame.prefXlispTraceBack);
        // printGC
        printGC = makeCheckBox("Print info about garbage collection",
                               mainFrame.prefPrintGC);

        // Use full search for code completion (checkbox)
        fullSearch = makeCheckBox("Use full search for code completion",
                                          mainFrame.prefFullSearch);
        // Use internal window for manual (checkbox)
        internalBrowser = makeCheckBox("Use window in jNyqIDE for help browser",
                                       mainFrame.prefInternalBrowser);
        // Use online manual (checkbox)
        onlineManual = makeCheckBox("Use online manual instead of local copy",
                                    mainFrame.prefOnlineManual);

        c.gridy += 1;
        panel.add(Box.createRigidArea(new Dimension(0, 10)), c);

        // Relative height of completion box (slider)
        c.gridy += 1;
        panel.add(new JLabel("Relative height of completion box"), c);
        completionListPercent = new JScrollBar(JScrollBar.HORIZONTAL,
                    (int) mainFrame.prefCompletionListPercent, 1, 0, 100);
        c.gridy += 1;
        c.fill = GridBagConstraints.HORIZONTAL;
        panel.add(completionListPercent, c);
        c.fill = GridBagConstraints.VERTICAL;

        c.gridy += 1;
        panel.add(Box.createRigidArea(new Dimension(0, 10)), c);

        // Audio Sample Rate (editable combobox)
        c.gridy += 1;
        panel.add(new JLabel("Audio Sample Rate"), c);
        audioRate = new JComboBox(audioRates);
        // Set correct selection
        for (int i = 0; i < audioRates.length; i++) {
            if (mainFrame.prefAudioRate.equals(audioRates[i])) {
                audioRate.setSelectedIndex(i);
                break;
            }
        }
        audioRate.setEditable(true);
        audioRate.setAlignmentX(Component.LEFT_ALIGNMENT);
        audioRate.setMaximumSize(
                new Dimension(100, audioRate.getPreferredSize().height));
        c.gridy += 1;
        panel.add(audioRate, c);
            
        c.gridy += 1;
        panel.add(Box.createRigidArea(new Dimension(0, 10)), c);

        // Control Rate (text field)
        c.gridy += 1;
        panel.add(new JLabel("Control Sample Rate"), c);
        controlRate = new JTextField(mainFrame.prefControlRate);
        controlRate.setAlignmentX(Component.LEFT_ALIGNMENT);
        controlRate.setMaximumSize(
                new Dimension(100, controlRate.getPreferredSize().height));
        c.gridy += 1;
        panel.add(controlRate, c);

        c.gridy += 1;
        panel.add(Box.createRigidArea(new Dimension(0, 10)), c);

        // Font Size (editable combobox)
        c.gridy += 1;
        panel.add(new JLabel("Font Size"), c);
        fontSize = new JComboBox(fontSizes);
        // Set correct selection
        for (int i = 0; i < fontSizes.length; i++) {
            if (mainFrame.prefFontSize.equals(fontSizes[i])) {
                fontSize.setSelectedIndex(i);
                break;
            }
        }
        fontSize.setEditable(true);
        fontSize.setAlignmentX(Component.LEFT_ALIGNMENT);
        fontSize.setMaximumSize(
                new Dimension(100, fontSize.getPreferredSize().height));
        c.gridy += 1;
        panel.add(fontSize, c);

        c.gridy += 1;
        panel.add(Box.createRigidArea(new Dimension(0, 10)), c);

        // Select Startup Directory (button)
        startfd = new JFileChooser("Select Initial Directory");
        startfd.setCurrentDirectory(new File(mainFrame.prefDirectory));
        startfd.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        initialDirectory = new JButton("Set Initial Directory");
        initialDirectory.addActionListener(this);
        initialDirectory.setAlignmentX(Component.LEFT_ALIGNMENT);
        c.gridy += 1;
        panel.add(initialDirectory, c);

        lastDirectory = new JButton("Last Used Directory is Initial Directory");
        lastDirectory.addActionListener(this);
        lastDirectory.setAlignmentX(Component.LEFT_ALIGNMENT);
        c.gridy += 1;
        panel.add(lastDirectory, c);

        c.gridy += 1;
        panel.add(Box.createRigidArea(new Dimension(0, 10)), c);

        // Select Sound File Output Directory (button)
        fd = new JFileChooser("Select Default Soundfile Directory");
        fd.setCurrentDirectory(new File(mainFrame.prefSFDirectory));
        fd.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        sfDirectory = new JButton("Set Default Soundfile Directory");
        sfDirectory.addActionListener(this);
        sfDirectory.setAlignmentX(Component.LEFT_ALIGNMENT);
        c.gridy += 1;
        panel.add(sfDirectory, c);
        
        c.gridy += 1;
        panel.add(Box.createVerticalGlue(), c);
                
        /*
        * The Nyquist IDE has a preferences dialog with a couple of things you
        * can change. It would be great to have a graphical way to set things
        * like the normalization style, default audio and control sample rates,
        * whether to play sounds or save audio to disk or both when calling the
        * play function, whether to apply reverb and/or EQ to the output signal
        * when using PLAY, a default sound file directory, whether to print a
        * stack trace when an error is encountered, etc. (All of these things
        * can be set in Nyquist, but most users do not know how.)
        */

        pack();
        Dimension size = new Dimension(400, 400);
        setSize(size);
        Point mfloc = mainFrame.getLocation();
        setLocation(mfloc.x + 25, mfloc.y + 25);
        setResizable(true);
        setVisible(true);
        setClosable(true);
        setMaximizable(true);
        setIconifiable(true);
        repaint();
    }

    private int validate(String s) {
        try {
            int temp = Integer.parseInt(s);
            if (temp > 0) return temp;
            } catch (Exception e) {
        }
        return -1;
    }
    

    /*
    // On Mac OS X, we can select directories using the native file open dialog
    void getDirectoryUsingFileDialog(String title) {
        boolean saveUseJFC = Prefs.useJFileChooser;
        Prefs.useJFileChooser = false;
        System.setProperty("apple.awt.fileDialogForDirectories", "true");
        OpenDialog od = new OpenDialog(title, defaultDir, null);
        if (od.getDirectory()==null)
            directory = null;
        else
            directory = od.getDirectory() + od.getFileName() + "/";
        defaultDir = directory;
        System.setProperty("apple.awt.fileDialogForDirectories", "false");
        Prefs.useJFileChooser = saveUseJFC;
    }
    */

    public void actionPerformed(ActionEvent evt) {
        if (evt.getSource() == sfDirectory) {
            fd.showOpenDialog(this);
        } else if (evt.getSource() == initialDirectory) {
            startfd.showOpenDialog(this);
        } else if (evt.getSource() == lastDirectory) {
            startfd.setSelectedFile(null);
        } else if (evt.getSource() == defaultPrefs) {
            startInSalMode.setSelected(mainFrame.prefStartInSalModeDefault);
            salShowLisp.setSelected(mainFrame.prefSalShowLispDefault);
            parenAutoInsert.setSelected(
                    mainFrame.prefParenAutoInsertDefault);
            enableSound.setSelected(mainFrame.prefEnableSoundDefault);
            autoNorm.setSelected(mainFrame.prefAutoNormDefault);
            salTraceBack.setSelected(mainFrame.prefSalTraceBackDefault);
            salBreak.setSelected(mainFrame.prefSalBreakDefault);
            xlispBreak.setSelected(mainFrame.prefXlispBreakDefault);
            xlispTraceBack.setSelected(mainFrame.prefXlispTraceBackDefault);
            printGC.setSelected(mainFrame.prefPrintGCDefault);
            fullSearch.setSelected(mainFrame.prefFullSearchDefault);
            internalBrowser.setSelected(mainFrame.prefInternalBrowserDefault);
            onlineManual.setSelected(mainFrame.prefOnlineManualDefault);
            completionListPercent.setValue(
                    (int) (mainFrame.prefCompletionListPercentDefault + 0.5));
            audioRate.setSelectedItem(mainFrame.prefAudioRateDefault);
            controlRate.setText(mainFrame.prefControlRateDefault);
            fontSize.setSelectedItem(mainFrame.prefFontSizeDefault);
            startfd.setSelectedFile(null);
            fd.setSelectedFile(null);
        }
    } 
}
