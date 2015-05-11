package jnyqide;
// Code: Dmitry Portnoy, 5/1/09
// Revised 2011 by Roger B. Dannenberg
/*
 Graphics organization:
 
 mainframe -- the MainFrame
    UPICFrame -- this editor, a JInternalFrame
        mainPanel -- a JPanel
            topPanel -- a JPanel
                dataPanel -- a JPanel, border "Current Envelope"
                    varNameLabel -- a JLabel ("Name:")
                    varName -- a JTextField
                    waveformNameLabel -- a JLabel ("Waveform:")
                    waveformName -- a JComboBox
                    envNameLabel -- a JLabel("Envelope:")
                    envName -- a JComboBox
                rangePanel - a JPanel, border "Range"
                    update - JButton("Update Range")
                    jMaxTL - a JLabel("Stop Time")
                    jMaxT - a JTextField
                    jMinAL - a JLabel("Min Freq")
                    jMinA - a JTextField
                    jMaxAL - a JLabel("Max Freq")
                    jMaxA - a JTextField
                undoPanel - a JPanel, border "Undo/Redo"
                    undo - a JButton("Undo")
                    redo - a JButton("Redo")
                canvas - a PiecewiseCanvas (a JPanel)
    
if endpoints are specified, e.g. if times[0] is 0 or last value in times
    matches the ending time, then use PWLV or PWEV version
    Be careful not to drop first/last point unless the time matches the
    begin/end time.


 */

/* Design notes by Dannenberg:

Too much data to treat this like envelopes. Instead, load/save data a a file.
Menu items: save, save as, revert, clear
Use variable name box to give a variable name for the data.
Use waveform name drop-down box to name waveforms.
Use envelope name drop-down box to name envelopes.
File format will be a waveform name, envelope name, and 
list of lists of alternating time value coordinates.
File will have the format:
------------
;; data saved from NyquistIDE UPIC editor
set data-name = {
{ waveform envelope first list }
{ waveform envelope second list }
...
}
------------
It's up to the user to interpret this data, but we'll add some 
functions to do this.

*/

/* Revised by Zeyu Jin (Jan-03-2013)
 * Replace JInternalFrame with a new one.
 */



import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.image.*;
import java.util.*;
import java.text.*;
import java.io.*;
import javax.imageio.ImageIO;

public class UPICFrame extends JNonHideableInternalFrame implements ActionListener {

    double EPSILON = 0.00000001; // small number -- allow for rounding error
    int LEFT_BORDER = 3; // inset everything by 3 pixels
    int TOP_BORDER = 3;
    int BOTTOM_BORDER = 5;
    int RIGHT_BORDER = 5;
    MainFrame mainframe;
    
    private JPanel mainPanel;
    
    private JTextPane jInputArea;
    private PiecewiseCanvas canvas;
    
    private JMenuItem showPictureItem;
    private JTextField jMaxT;
    private double maxT;
    private JTextField jMinHz;
    private double minHz;
    private JTextField jMaxHz;
    private double maxHz;
    private JCheckBox jLinear;
    private boolean isLinear = true;
    private JTextField varName;
    private String currentVarName;
    private JComboBox envName;
    private String currentEnvName = "upic-env";
    private JComboBox waveformName;
    private String currentWaveformName = "*sine-table*";
    private boolean showCs;
    private boolean showGrandStaff;
    private boolean showPicture;
    // when saving envelopes, we copy current envelope to the collection,
    // but this makes the combobox think that an envelope was selected, 
    // and we get a request to save the same envelope were saving. To 
    // avoid this, the "saving" flag is used to disable the check.
    private boolean saving = false;
    private File file; // the file backing the current data
    private String fileName; // name of file
            
    //hashtable for storing envelopes
    private Hashtable<String, String> envColl;
    
    static double initTime=0.0;
    static double finalTime=1.0;    
    static boolean displayCoord = false;
    static boolean valueType=false;
    static DecimalFormat form = new DecimalFormat("#.###");
    private boolean modified; // tells when any aspect of envelope 
    // has changed
    // envelope is modified by: entering a point, deleting a point, changing
    // the end-time (update), or clearing
    // modified is reset when envelope is loaded or saved
    
    private JButton MakeButton(String name, String cmd) {
        JButton b = new JButton(name);
        b.addActionListener(this);
            b.setActionCommand(cmd);
        return b;
    }

    private JMenuItem AddMenuItem(JMenu menu, String name, String cmd, int code) {
        return AddMenuItem(menu, name, cmd, code, false, false);
    }

    private JMenuItem AddMenuItem(JMenu menu, String name, String cmd, int code, 
                             boolean shift) {
        return AddMenuItem(menu, name, cmd, code, shift, false);
    }

    private JMenuItem AddMenuItem(JMenu menu, String name, String cmd, 
                             int code, boolean shift, boolean check) {
        int keyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        JMenuItem item;
        if (check) {
            item = new JCheckBoxMenuItem(name);
        } else {
            item = new JMenuItem(name);
        }
        item.setActionCommand(cmd);
        item.addActionListener(this);
        if (code != -1) {
            item.setAccelerator(KeyStroke.getKeyStroke(code, keyMask | 
                    (shift ? java.awt.event.InputEvent.SHIFT_MASK : 0)));
        }
        menu.add(item);
        return item;
    }

    private JComboBox MakeCombo(String cmd) {
        JComboBox b = new JComboBox();
        b.setPreferredSize(new Dimension(150, 20));
        b.setEditable(true);
        b.addActionListener(this);
        b.setActionCommand(cmd);
        return b;
    }

    // Constructor
    public UPICFrame(final MainFrame parent, JTextPane inputArea)        {
        super();
        mainframe = parent;
        jInputArea = inputArea;
        mainPanel = (JPanel) getContentPane();
                        
        envColl = new Hashtable<String, String>();
        String name = "UPIC Editor";
        setTitle(name);
        setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
        final UPICFrame upicFrame = this;
        modified = false;
        addInternalFrameListener(
            new InternalFrameListener() {
                public void internalFrameClosing(InternalFrameEvent e) {
                    //System.out.println("FrameClosing");
                    int r = JOptionPane.OK_OPTION;
                    if (upicFrame.modified) {
                        r = JOptionPane.showConfirmDialog(upicFrame,
                                "Really close without saving?",
                                "Alert", JOptionPane.OK_CANCEL_OPTION);
                    }
                    if (r == JOptionPane.OK_OPTION) {
                        upicFrame.dispose();
                    }
                }
                public void internalFrameOpened(InternalFrameEvent e) {
                }
                public void internalFrameClosed(InternalFrameEvent e) {
                    parent.disconnectUPIC();
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

        System.out.println("EnvelopeFrame constructor 1");                
              
        JMenuBar menuBar = new JMenuBar();
        // File Menu
        JMenu fileMenu = new JMenu("File");
        AddMenuItem(fileMenu, "Load", "loadUpicData", 
                              java.awt.event.KeyEvent.VK_K);
        AddMenuItem(fileMenu, "Open", "openUpicData", 
                              java.awt.event.KeyEvent.VK_O);
        AddMenuItem(fileMenu, "Save", "saveUpicData", 
                              java.awt.event.KeyEvent.VK_S);
        AddMenuItem(fileMenu, "Save As...", "saveUpicDataAs", 
                    java.awt.event.KeyEvent.VK_S, true);
        AddMenuItem(fileMenu, "Revert", "revertUpicData", -1);
        AddMenuItem(fileMenu, "Clear", "clearUpicData", -1);

        JMenu editMenu = new JMenu("Edit");
        AddMenuItem(editMenu, "Undo", "undo",
                    java.awt.event.KeyEvent.VK_Z);
        AddMenuItem(editMenu, "Redo", "redo",
                    java.awt.event.KeyEvent.VK_Z, true);
        AddMenuItem(editMenu, "Duplicate", "duplicate",
                    java.awt.event.KeyEvent.VK_C);
        AddMenuItem(editMenu, "Stretch", "stretch",
                    java.awt.event.KeyEvent.VK_T);
        AddMenuItem(editMenu, "Align Beginning", "align-beginning",
                    java.awt.event.KeyEvent.VK_B);
        AddMenuItem(editMenu, "Align Ending", "align-ending",
                    java.awt.event.KeyEvent.VK_E);
        AddMenuItem(editMenu, "Thin Data", "thin",
                    java.awt.event.KeyEvent.VK_H);

        JMenu backgroundMenu = new JMenu("Background");
        AddMenuItem(backgroundMenu, "C's", "showCs", 
                    java.awt.event.KeyEvent.VK_F, false, true);
        AddMenuItem(backgroundMenu, "Grand Staff", "showGrandStaff", 
                    java.awt.event.KeyEvent.VK_G, false, true);
        
        showPictureItem = 
                AddMenuItem(backgroundMenu, "Show Picture", "showPicture",
                    java.awt.event.KeyEvent.VK_P, false, true);
        AddMenuItem(backgroundMenu, "Load Picture...", "loadPicture", -1);

        menuBar.add(fileMenu);
        menuBar.add(editMenu);
        menuBar.add(backgroundMenu);
        setJMenuBar(menuBar);

        JLabel varNameLabel = new JLabel("Name: ");
        varName = new JTextField("upicdata", 10);
        currentVarName = "upicdata";
    
        JLabel waveformNameLabel = new JLabel("Waveform:");
        waveformName = MakeCombo("waveformNameSelected");

        JLabel envNameLabel = new JLabel("Envelope:");
        envName = MakeCombo("envNameSelected");

        JPanel dataPanel = new JPanel();
        GridBagLayout layout0 = new GridBagLayout();
        dataPanel.setLayout(layout0);
        GridBagConstraints cons0 = new GridBagConstraints();
    
        cons0.fill = GridBagConstraints.NONE;
        cons0.anchor = GridBagConstraints.EAST;
        cons0.insets = new Insets(2, 2, 2, 2); // TEST
        cons0.weightx = 0;
        cons0.weighty = 0;
        cons0.gridy = 0;
        cons0.gridx = 0;
        cons0.gridheight = 1;
        cons0.gridwidth = 1;
        dataPanel.add(varNameLabel, cons0);
        cons0.anchor = GridBagConstraints.WEST;
        cons0.gridx = 1;
        cons0.fill = GridBagConstraints.HORIZONTAL; //TEST
        dataPanel.add(varName, cons0);
        cons0.anchor = GridBagConstraints.EAST;
        cons0.fill = GridBagConstraints.NONE;
        cons0.gridy = 1;
        cons0.gridx = 0;
        dataPanel.add(waveformNameLabel, cons0);
        cons0.anchor = GridBagConstraints.WEST;
        cons0.gridx = 1;
        dataPanel.add(waveformName, cons0);
        cons0.gridy = 2;
        cons0.gridx = 0;
        dataPanel.add(envNameLabel, cons0);
        cons0.anchor = GridBagConstraints.WEST;
        cons0.gridx = 1;
        dataPanel.add(envName, cons0);
    
        // panel to contain time and amplitude parameters
        JPanel rangePanel = new JPanel();
        //JPanel paramPanel = new JPanel();
        //paramPanel.setBorder(BorderFactory.createTitledBorder("Range"));
        rangePanel.setLayout(new GridBagLayout());
        rangePanel.setBorder(BorderFactory.createTitledBorder("Range")); //TEST
        //paramPanel.setLayout(new GridBagLayout());
    
        jMaxT = new JTextField("20.0", 5);
        maxT = 20.0;
        jMinHz = new JTextField("20.0", 5);
        minHz = 20.0;
        jMaxHz = new JTextField("2000.0", 5);
        maxHz = 2000;
        jLinear = new JCheckBox("linear", true);
        jLinear.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {            
                setModified();
                isLinear = getLinear();
                canvas.history.save(canvas);
                canvas.repaint();
            }
            });
    
        showCs = false;
        showGrandStaff = false;
        showPicture = false;
            
        JLabel jMaxTL = new JLabel("Stop Time: ");
        JLabel jMinHzL = new JLabel("Min Freq: ");
        JLabel jMaxHzL = new JLabel("Max Freq: ");
            
        JButton update = new JButton("Update Range");
        cons0.gridx = 0;
        cons0.gridy = 0;
        cons0.gridwidth = 2;
        rangePanel.add(update, cons0);
        cons0.gridwidth = 1;
        cons0.gridx = 2;
        cons0.gridy = 0;
        rangePanel.add(jMaxTL, cons0);
        cons0.gridx = 3;
        cons0.gridy = 0;
        rangePanel.add(jMaxT, cons0);
        cons0.gridx = 0;
        cons0.gridy = 1;
        rangePanel.add(jMinHzL, cons0);    
        cons0.gridx = 2;
        rangePanel.add(jMaxHzL, cons0);
        cons0.gridx = 1;
        rangePanel.add(jMinHz, cons0);
        cons0.gridx = 3;
        rangePanel.add(jMaxHz, cons0);
        cons0.gridx = 4;
        rangePanel.add(jLinear, cons0);
        
        update.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {            
                if (getMaxT() <= 0) {
                    JOptionPane.showMessageDialog(mainPanel, 
                           "Stop Time cannot be negative or zero");
                } else if (getMinHz() > getMaxHz()) {
                    JOptionPane.showMessageDialog(mainPanel, 
                 "Minimum frequency cannot be greater than maximum frequency");
                } else if ((canvas.endTime() > getMaxT())) {
                    JOptionPane.showMessageDialog(mainPanel, 
              "Stop Time is less than the time of an existing envelope point");
                } else if (getMinHz() < 20.0) {
                    JOptionPane.showMessageDialog(mainPanel, 
                           "Minimum frequency cannot be less than 20 Hz");
                } else {
                    setModified();
                    maxT = getMaxT();
                    minHz = getMinHz();
                    maxHz = getMaxHz();
                    isLinear = getLinear();
                    canvas.history.save(canvas);
                    canvas.repaint();
                    return;
                }
                // an error occurred, reset the Range (using complete restore)
                canvas.restore();
            }
            });
    
        JPanel undoPanel = new JPanel();
        undoPanel.setLayout(new GridBagLayout());
        undoPanel.setBorder(BorderFactory.createTitledBorder("Undo/Redo"));
        JButton undo = new JButton("Undo");
        undo.setActionCommand("undo");
        undo.addActionListener(this);
        JButton redo = new JButton("Redo");
        redo.setActionCommand("redo");
        redo.addActionListener(this);
        cons0.gridx = 0;
        cons0.gridy = 0;
        undoPanel.add(undo, cons0);
        cons0.gridy = 1;
        undoPanel.add(redo, cons0);

        //insert components into the larger panels
        cons0.fill = GridBagConstraints.NONE;
        cons0.anchor = GridBagConstraints.WEST;
        cons0.weightx = 0;
        cons0.weighty = 0;
        cons0.gridx = 0;
        cons0.gridy = 0;
        cons0.gridheight = 1;
        cons0.gridwidth = 1;
        //cons0.insets = new Insets(5,0,0,5);
        //paramPanel.add(rangePanel, cons0);
        //        cons0.fill = GridBagConstraints.NONE;
        //        cons0.anchor = GridBagConstraints.CENTER;
        //        cons0.weightx = 0;
        //        cons0.weighty = 0;
        //        cons0.gridx = 0;
        //        cons0.gridy = 1;
        //        cons0.gridheight = 1;
        //        cons0.gridwidth = 1;
        //        //cons0.insets = new Insets(0,0,0,5);        
        //        paramPanel.add(update, cons0);
    
        JPanel topPanel = new JPanel();
        //topPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 30, 0));
        topPanel.setLayout(new GridBagLayout());
        cons0.anchor = GridBagConstraints.NORTHWEST;
        cons0.gridx = 0;
        cons0.gridy = 0;
        topPanel.add(dataPanel, cons0);
        cons0.gridx = 1;
        topPanel.add(rangePanel, cons0); //TEST
        cons0.gridx = 2;
        topPanel.add(undoPanel, cons0);
        
        canvas = new PiecewiseCanvas();
        //insert panels into main frame and display
        mainPanel.add(BorderLayout.NORTH, topPanel);
        mainPanel.add(BorderLayout.CENTER, canvas);
        pack();
        
        //resize and center the window
        //Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

        setLocation(100, 100);
        setSize(650, 580);
        // setBackground(Color.white);
        setResizable(true);
        setVisible(true);
        setClosable(true);
        setMaximizable(true);
        setIconifiable(true);
        clearUpicData(true);
        System.out.println("UPICFrame constructor 2 after setIconifiable");
        repaint();
    }

    public boolean waveformNameSelected() {
        String name = (String) waveformName.getSelectedItem();
        if (name == null || name.equals("")) return false;
        currentWaveformName = name;
        return true;
    }

    public void waveformNewName() {
        if (waveformNameSelected()) {
            waveformName.addItem(currentWaveformName);
        }
    }

    public boolean envNameSelected() {
        String name = (String) envName.getSelectedItem();
        if (name == null) return false;

        String originalName = currentEnvName;
        currentEnvName = name.trim();
        if (!originalName.equals(currentEnvName)) {
            modified = true;
        }
        
        // make name be the selected name
        envName.setSelectedItem(name);
        currentEnvName = name;
        return true;
    }


    public void envNewName() {
        if (envNameSelected()) {
            envName.addItem(currentEnvName);
        }
    }
        

    public void varNameSelected() {
        if (saving) return; // ignore selection generated by "save" button
        // If the name is different from the current envelope name, do
        // a "save". Then switch to editing the newly selected envelope.
        String name = (String) varName.getText();
        // when we load the JComboBox with new names, the contentsChanged
        // method of JComboBox invokes a selection action, even if nothing
        // is selected. I don't know why, but we have to handle the null
        // selection case.
        if (name == null) return;
        
        String originalName = currentVarName;
        currentVarName = varName.getText().trim();
        if (!originalName.equals(currentVarName)) {
            modified = true;
        }
        varName.setText(name);
        currentVarName = name;
    }
    
    
    //public double getMinT() { return Double.parseDouble(minT.getText().trim()); }
    public double getMaxT() { return Double.parseDouble(jMaxT.getText().trim()); }
    public double getMinHz() { return Double.parseDouble(jMinHz.getText().trim()); }
    public double getMaxHz() { return Double.parseDouble(jMaxHz.getText().trim()); }
    public boolean getLinear() { return jLinear.isSelected(); }

    public boolean within(double x, double y, double eps) {
        return Math.abs(x - y) < eps;
    }


    // prompt for a file name and write the data
    public boolean saveUpicDataAs() {
        // select a file and write to it. Return true if success.
        FileDialog fileDialog = new FileDialog(mainframe, "Save UPIC Data",
                                               FileDialog.SAVE);
        SalFileFilter salFilter = new SalFileFilter();
        fileDialog.setFilenameFilter(salFilter);
        String name;
        fileDialog.setDirectory(mainframe.currentDir);
        
        while (true) { // loop until file is chosen
            fileDialog.setVisible(true);
            if (fileDialog.getDirectory() != null && 
                fileDialog.getFile() != null) {
                name = fileDialog.getDirectory() + fileDialog.getFile();
                System.out.println("You chose to save this file: " + name);
                String lower = name.toLowerCase();
                if (lower.endsWith(".sal")) break;
                JOptionPane dialog = new JOptionPane();
                int result = dialog.showConfirmDialog(this, 
                         "Do you really want to save a file without a " +
                         ".sal extension?", "Warning", 
                         JOptionPane.YES_NO_OPTION);
                System.out.println("return from dialog " + result);
                if (result == JOptionPane.YES_OPTION) break;
            } else { // file chooser cancel, early return
                return false;
            }
        }
        fileName = name;
        file = new File(name);
        mainframe.changeDirectory(mainframe.fileDirectory(file));
        modified = true; // for saveUpicData, do not need to add "*"
        return saveUpicData();
    }


    private void setModified() {
        if (modified) return;
        setTitle(fileName + "*");
        modified = true;
    }

    public boolean saveUpicData()
    // saves the file if there is a file name, otherwise calls saveUpicDataAs.
    // returns false if the operation fails or is cancelled.
    // returns true if save succeeds or if file is unmodified.
    {
        if (modified) {
            if (file == null)
                return saveUpicDataAs();
            else {
                try {
                    FileWriter saveFileStream = new FileWriter(file);
                    BufferedWriter out = new BufferedWriter(saveFileStream);
                    String s = ";; data saved from NyquistIDE UPIC editor\n";
                    s += String.format(
                              ";!; maxT %g minHz %g maxHz %g linear %s\n",
                              maxT, minHz, maxHz, (isLinear ? "t" : "nil"));
                    s += String.format("set %s = {\n", 
                                       varName.getText().trim());
                    out.write(s);
                    for (Curve c : canvas.curves) {
                        if (c.times.size() > 0) {
                            out.write(String.format("{ %s %s\n",
                                                    c.waveform, c.envelope));
                            for (int i = 0; i < c.times.size(); i++) {
                                out.write(String.format(" %.4f %.1f", 
                                        c.times.get(i), c.freqs.get(i)));
                            }
                            out.write(" }\n\n");
                        } 
                    }
                    out.write("}\n");
                    out.close();
                }
                catch(Exception e) { 
                    System.out.println("exception in saveUpicData" + e);
                    return false; // did not save file
                }
                
                modified = false;
                setTitle(fileName);
            }
        }
        return true;
    }


    public void actionPerformed(ActionEvent e)
    {
        String cmd = e.getActionCommand();
    
        // File Menu options
        if (cmd.equals("saveUpicData")) saveUpicData();
        else if (cmd.equals("loadUpicData")) {
                saveUpicData();
                mainframe.loadFile(file);
        } else if (cmd.equals("saveUpicDataAs")) saveUpicDataAs();
        else if (cmd.equals("openUpicData")) openUpicData();
        else if (cmd.equals("clearUpicData")) clearUpicData(false);

        // Edit Menu options
        else if (cmd.equals("duplicate")) canvas.duplicateCmd();
        else if (cmd.equals("stretch")) canvas.stretchCmd();
        else if (cmd.equals("align-beginning")) canvas.alignBeginningCmd();
        else if (cmd.equals("align-ending")) canvas.alignEndingCmd();
        else if (cmd.equals("thin")) canvas.thinCmd();
        

        else if (cmd.equals("envNameSelected")) envNameSelected();
        else if (cmd.equals("waveformNameSelected")) waveformNameSelected();

        // Background Menu options
        else if (cmd.equals("showCs")) {
            showCs = ((JCheckBoxMenuItem) e.getSource()).getState();
            canvas.history.save(canvas);
            canvas.repaint();
        } else if (cmd.equals("showGrandStaff")) {
            showGrandStaff = ((JCheckBoxMenuItem) e.getSource()).getState();
            canvas.history.save(canvas);
            canvas.repaint();
        } else if (cmd.equals("showPicture")) {
            showPicture = ((JCheckBoxMenuItem) e.getSource()).getState();
            canvas.history.save(canvas);
            canvas.repaint();
        } else if (cmd.equals("loadPicture")) {
            canvas.loadPicture();
            // if you load a picture, show it automatically
            ((JCheckBoxMenuItem) showPictureItem).setState(true);
            showPicture = true;
            canvas.repaint();
            System.out.println("loadPicture command done, after repaint()");
        } else if (cmd.equals("undo")) {
            canvas.history.undo();
            canvas.restore();
        } else if (cmd.equals("redo")) {
            canvas.history.redo();
            canvas.restore();
        } else if (cmd.equals("comboBoxEdited")) {
            if (e.getSource() == envName) envNewName();
            else if (e.getSource() == waveformName) waveformNewName();
            else System.out.println("COMBOBOXEDITED not handled\n");
        } else System.out.println("ACTION NOT HANDLED: " + cmd);
    }


    public void clearUpicData(boolean init) {
        canvas.clearUpicData(init);
        waveformName.removeAllItems();
        waveformName.addItem("*sine-table*");
        waveformName.addItem("*tri-table*");
        waveformName.addItem("*saw-table*");
        waveformName.setSelectedItem("*sine-table*");
        
        envName.removeAllItems();
        envName.addItem("upic-env");
        envName.setSelectedItem("upic-env");
    }


    public void addToComboBox(JComboBox cb, String s) {
        int n = cb.getItemCount();
        for (int i = 0; i < n; i++) {
            if (s.equals(cb.getItemAt(i))) return;
        }
        cb.addItem(s);
    }


    public void readUpicData(File fileToOpen) {
        clearUpicData(false);
        fileName = fileToOpen.getName();
        setTitle(fileName);
        double maxt = 20, minhz = 0, maxhz = 2000;
        boolean islinear = true;
        // this disabled saving state for undo:
        canvas.mouseDown = true;
        try {
            Scanner sc = new Scanner(fileToOpen);
            String next = sc.next();
            while (next.charAt(0) == ';') {
                if (next.equals(";!;")) { // parse parameters
                    if (!sc.next().equals("maxT") )
                        throw new Exception("syntax - maxT");
                    maxt = Double.parseDouble(sc.next());
                    if (!sc.next().equals("minHz") )
                        throw new Exception("syntax - minHz");
                    minhz = Double.parseDouble(sc.next());
                    if (!sc.next().equals("maxHz") )
                        throw new Exception("syntax - maxHz");
                    maxhz = Double.parseDouble(sc.next());
                    if (!sc.next().equals("linear") )
                        throw new Exception("syntax - linear");
                    islinear = !sc.next().equals("nil");
                    if (maxt < 0 || minhz < 20.0 || maxhz < minhz) {
                        throw new Exception("bad maxt, minhz, maxhz");
                    }
                }
                sc.nextLine();
                next = sc.next();
            }
            // next tokens are (s)et variable = 
            if (!next.equals("set")) throw new Exception("syntax - set");
            currentVarName = sc.next();
            varName.setText(currentVarName);
            if (!sc.next().equals("=")) throw new Exception("syntax - =");
            // now read the first {
            if (!sc.next().equals("{")) throw new Exception("syntax - {");
            // read lists
            next = sc.next();
            while (next.equals("{")) {
                String waveform = sc.next();
                addToComboBox(waveformName, waveform);
                String envelope = sc.next();
                addToComboBox(envName, envelope);
                System.out.println("waveform " + waveform + " env " + envelope);
                canvas.startNewCurve(waveform, envelope);
                next = sc.next();
                while (!next.equals("}")) {
                    // System.out.println("time " + next);
                    double time = Double.parseDouble(next);
                    double freq = Double.parseDouble(sc.next());
                    if (time < 0) time = 0;
                    canvas.insertInOrder(time, freq);
                    next = sc.next();
                }
                next = sc.next();
            }
            if (!next.equals("}")) {
                throw new Exception("syntax");
            }
            // make sure whole thing fits on canvas
            maxT = Math.max(canvas.endTime(), maxt);
            minHz = Math.min(canvas.lowHz(), minhz);
            maxHz = Math.max(canvas.highHz(), maxhz);
            isLinear = islinear;

            jMaxT.setText(String.valueOf(maxT));
            jMinHz.setText(String.valueOf(minHz));
            jMaxHz.setText(String.valueOf(maxHz));
            jLinear.setSelected(isLinear);
        } catch (Exception e) {
            System.out.println("readUpicData " + e);
            JOptionPane.showMessageDialog(this, 
"A problem was encountered reading a upic data file, data may have been lost");
        }
        canvas.mouseDown = false;
    }
            

    public void openUpicData() {
        FileDialog fileDialog = new FileDialog(mainframe, 
                                               "Select a File to Open",
                                               FileDialog.LOAD);
        SalFileFilter salFilter = new SalFileFilter();
        fileDialog.setFile("*.sal");
        fileDialog.setFilenameFilter(salFilter);
        fileDialog.setDirectory(mainframe.currentDir);
        fileDialog.setVisible(true);
        if (fileDialog.getDirectory() != null && fileDialog.getFile() != null) {
            String path = fileDialog.getDirectory() + fileDialog.getFile();
            File file = new File(path);
            mainframe.changeDirectory(mainframe.fileDirectory(file));
            System.out.println("You chose to open this file: " + path);
            readUpicData(file);
            modified = false;
        }
    }


    private class State {
        public double maxT;
        public double minHz;
        public double maxHz;
        public boolean linear;
        public boolean cs;
        public boolean gs;
        public boolean picture;

        public State(double stop, double low, double hi, boolean lin,
                     boolean showcs, boolean showgs, boolean showpic) {
            maxT = stop; minHz = low; maxHz = hi; linear = lin;
            cs = showcs; gs = showgs; picture = showpic;
        }
    }
    
    private class History {
    /* consider a sequence of undo/redo to be a single edit operation -- thus
     * the end of the versions list is set to the latest undo/redo selection
     */
        private boolean undoRedo = false;
        private ArrayList<ArrayList<Curve>> history = 
                new ArrayList<ArrayList<Curve>>();
        private ArrayList<State> state_history = new ArrayList<State>();
        private int version = -1;
    
        @SuppressWarnings("unchecked")
        public void save(PiecewiseCanvas canvas) {
            history.add((ArrayList<Curve>) canvas.curves.clone());
            state_history.add(new State(maxT, minHz, maxHz, isLinear, 
                                        showCs, showGrandStaff, showPicture));
            version = history.size() - 1;
            System.out.println("Saved version " + version);
            undoRedo = false; /* clear flag for next undoRedo sequence */
        }

        public boolean canGet() { 
            boolean result = version >= 0 && version < history.size();
            System.out.println("canGet returns " + result + " version " +
                version);
            return result;
        }
        
        public ArrayList<Curve> getCurves() { return history.get(version); }

        public State getState() { return state_history.get(version); }
        
        @SuppressWarnings("unchecked")
        private void processUndoRedo() {
            if (!undoRedo) { /* extend with copy of the version */
                history.add((ArrayList<Curve>) (history.get(version).clone()));
                state_history.add(state_history.get(version));
            } else { /* replace with different version */
                history.set(history.size() - 1, 
                            (ArrayList<Curve>) (history.get(version).clone()));
                state_history.set(state_history.size() - 1, 
                                  state_history.get(version));
            }
            undoRedo = true;
        }
        
        public void undo() {
            if (version > 0) {
                version--; 
                processUndoRedo();
            }
        }
        public void redo() { 
            if (version < history.size() - 1) {
                version++; 
                processUndoRedo();
            }
        }
    }
    
    private class Curve {
        public String waveform;
        public String envelope;
        public ArrayList<Double> times;
        public ArrayList<Double> freqs;
        public boolean selected;

        public Curve(String wave, String env) {
            waveform = wave; 
            envelope = env;
            selected = false;
            times = new ArrayList<Double>();
            freqs = new ArrayList<Double>();
        }

        public Curve(Curve c, int displacement) {
            waveform = c.waveform;
            envelope = c.envelope;
            selected = c.selected;
            times = (ArrayList<Double>) (c.times.clone());
            freqs = (ArrayList<Double>) (c.freqs.clone());
            displace(displacement, displacement);
        }

        public void displace(int x, int y) {
            for (int i = 0; i < times.size(); i++) {
                times.set(i, canvas.x2time(canvas.time2x(times.get(i)) + x));
                freqs.set(i, canvas.y2hz(canvas.hz2y(freqs.get(i)) + y));
            }
        }


        // A general stretch operation. Create new curve between start
        // and end, or if end is zero, begin at start and stretch by amount
        public Curve stretch(double start, double end, double amount) {
            Curve c = new Curve(this, 0);
            if (times.size() < 1) return c;
            if (times.size() < 2) {
                times.set(0, start);
                return c;
            }
            double oldStart = times.get(0);
            if (amount == 0) {
                double oldEnd = times.get(times.size() - 1);
                if (oldEnd - oldStart < 0.0000001) return c;
                amount = (end - start) / (oldEnd - oldStart);
            }
            for (int i = 0; i < times.size(); i++) {
                c.times.set(i, start + (times.get(i) - oldStart) * amount);
            }
            return c;
        }


        public int distance(int x, int y) {
            int dist = 10000;
            for (int i = 0; i < times.size(); i++) {
                int d = Math.abs(canvas.time2x(times.get(i)) - x) +
                        Math.abs(canvas.hz2y(freqs.get(i)) - y);
                if (d < dist) dist = d;
            }
            return dist;
        }

        public void insertInOrder(double time, double amp) {
            int i = times.size();
            while (i > 0 && time < times.get(i - 1)) {
                i--;
            }
            addPoint(i, time, amp);
        }


        private void addPoint(int i, double absT, double absA) {
            times.add(i, absT);
            freqs.add(i, Math.max(absA, 0.0));
        }


        // remove any duplicate points that map to the same pixel
        public Curve thin() {
            Curve c = new Curve(this, 0);
            int j = 0;
            int x = -1000; // x,y coordinates of previous point
            int y = -1000;
            for (int i = 0; i < times.size(); i++) {
                int x2 = canvas.time2x(times.get(i));
                int y2 = canvas.hz2y(freqs.get(i));
                if ((x != x2) || (y != y2)) { // different, so keep it
                    c.times.set(j, times.get(i));
                    c.freqs.set(j, freqs.get(i));
                    x = x2;
                    y = y2;
                    j++;
                }
            }
            // clear the left-over points. removeRange is protected, so
            // remove items one-at-a-time
            while (c.times.size() > j) {
                c.times.remove(c.times.size() - 1);
                c.freqs.remove(c.freqs.size() - 1);
            }
            return c;
        }
    }
    
    // Class for the drawing area
    //private class PiecewiseCanvas extends Canvas implements MouseListener, 
    private class PiecewiseCanvas extends JPanel implements MouseListener, 
            MouseMotionListener, KeyListener {
        private int currentHz;
        private int currentTime;
        public BufferedImage image = null;
        public boolean selectCheck = false;
        public History history;

        public boolean mouseDown = false; // used to detect version for undo
        private boolean changed = false; // used to detect version for undo
    
        //  Vectors to store the absolute parameters of the points in the 
        // envelope.
        public ArrayList<Curve> curves = new ArrayList<Curve>();
        private int left, top, width, height;

        private BufferedImage background;

        private int dragx, dragy;
        private boolean is_dragging;

        // Constructor
        public PiecewiseCanvas() {
            setBackground(Color.white);
            addMouseListener(this);
            addMouseMotionListener(this);
            addKeyListener(this);
            history = new History();
            history.save(this);
            background = null;
        }
        
            
        public void loadPicture() {
            FileDialog fileDialog = new FileDialog(mainframe, 
                          "Select a Picture", FileDialog.LOAD);
            fileDialog.setDirectory(mainframe.currentDir);
            fileDialog.setVisible(true);
            File file = null; // in case open fails
            if (fileDialog.getDirectory() != null && 
                fileDialog.getFile() != null) {
                String path = fileDialog.getDirectory() + fileDialog.getFile();
                System.out.println("Opening image: " + path);
                file = new File(path);
                mainframe.changeDirectory(mainframe.fileDirectory(file));
                try {
                    System.out.println("Trying ImageIO.read");
                    background = ImageIO.read(file);
                    System.out.println("   returned " + background);
                } catch (Exception e) {
                    System.out.println("Image read failed " + e);
                    JOptionPane.showMessageDialog(this, 
                                                  "Could not open image file");
                    background = null;
                }
            }
            System.out.println("loadPicture returns");
        }
    

        public double endTime() {
            double m = 0;
            for (Curve c : curves) {
                for (double time : c.times) {
                    m = Math.max(m, time);
                }
            }
            return m;
        }

        public double lowHz() {
            double low = 20000;
            for (Curve c : curves) {
                for (double hz : c.freqs) {
                    low = Math.min(low, hz);
                }
            }
            return low;
        }

        public double highHz() {
            double high = 0;
            for (Curve c : curves) {
                for (double hz : c.freqs) {
                    high = Math.max(high, hz);
                }
            }
            return high;
        }


        // erase the canvas and clear the parameter vectors - 
        // completely reset the envelope
        public void clearUpicData(boolean init) {
            curves.clear();
            if (!init) setModified();
            repaint();
            history.save(this);
        }


        // Allow JPanel to accept keyboard events
        public boolean isFocusable() {
            return true;
        }
        
        //try to make the canvas the correct size
        public Dimension getMinimumSize() {
            return new Dimension(0, 0);
        }
        
        public Dimension getPreferredSize() {
            return new Dimension(575, 256);
        }
        
        public double impliedAmp() {
            return 0.0;
        }


        /*
        public Dimension getMaximumSize() {
            return getPreferredSize();
        }*/
        
        // draw the graphics inside the full canvas, so use these
        // functions to get the size and coordinates of the drawing
        // area that is usable
        private int graphLeft() { return LEFT_BORDER; }
        private int graphWidth() { 
            return getWidth() - (LEFT_BORDER + RIGHT_BORDER); 
        }
        private int graphTop() { return TOP_BORDER; }
        private int graphHeight() { 
            return getHeight() - (TOP_BORDER + BOTTOM_BORDER); 
        }
        private int clipX(int x) {
            return Math.max(LEFT_BORDER, Math.min(left + width, x)); }
        private int clipY(int y) {
            return Math.max(BOTTOM_BORDER, Math.min(top + height, y)); }
    
    
        //draw the canvas image
        public void paint(Graphics g) {
            super.paint(g); 
            isLinear = getLinear();
        
            Graphics2D drawArea = (Graphics2D) g;
            if (showCs) { // draw line for C in all octaves
                double hz = 16.3516;
                while (hz < maxHz) {
                    drawPitch(drawArea, hz);
                    hz *= 2;
                }
            }

            if (showPicture && (background != null)) {
                drawArea.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                               RenderingHints.VALUE_INTERPOLATION_BILINEAR);
                int x = time2x(0);
                int y = hz2y(maxHz);
                int w = time2x(maxT) - x;
                int h = hz2y(minHz) - y;
                drawArea.drawImage(background, x, y, w, h, null);
            }
            
            if (showGrandStaff) { // draw GrandStaff lines
                drawPitch(drawArea, 698.456); // top line F
                drawPitch(drawArea, 587.330); // D
                drawPitch(drawArea, 493.883); // B
                drawPitch(drawArea, 391.995); // G
                drawPitch(drawArea, 329.628); // E

                drawPitch(drawArea, 220.000); // A
                drawPitch(drawArea, 174.614); // F
                drawPitch(drawArea, 146.832); // D
                drawPitch(drawArea, 123.471); // B
                drawPitch(drawArea, 97.9989); // G
            }
            
            connectTheDots(drawArea);
            drawArea.dispose();
        }

        private void drawPitch(Graphics2D g, double hz) {
            g.setColor(Color.lightGray);
            int y = hz2y(hz);
            g.drawLine(time2x(0), y, time2x(maxT), y);
        }

        private void draw_connect(Graphics2D g, double t1, double a1, 
                                                double t2, double a2) {
            int x1 = time2x(t1);
            int x2 = time2x(t2);
            int y1 = hz2y(a1);
            int y2 = hz2y(a2);
            
            g.drawLine(x1, y1, x2, y2);
        }
        
        private void cacheSize() {
            left = graphLeft();
            top = graphTop();
            width = graphWidth() - 1;
            height = graphHeight() - 1;
        }

        //connect adjacent points in the envelope by lines
        private void connectTheDots(Graphics2D g) {
            cacheSize();
            for (Curve c : curves) {
                g.setColor(c.selected ? Color.red : Color.blue);
                if (c.times.size() > 0) {
                    //connect the points in the envelope
                    double t1 = c.times.get(0);
                    double a1 = c.freqs.get(0);
                    
                    for (int i = 1; i < c.times.size(); i++) {
                        double t2 = c.times.get(i);
                        double a2 = c.freqs.get(i);
                        draw_connect(g, t1, a1, t2, a2);
                        t1 = t2;
                        a1 = a2;
                    }
                } 
            }
        }
    
        
        @SuppressWarnings("unchecked")
        public void restore() {
            if (history.canGet()) {
                curves = (ArrayList<Curve>) (history.getCurves().clone());
                State state = history.getState();
                maxT = state.maxT;
                jMaxT.setText(String.valueOf(state.maxT));
                minHz = state.minHz;
                jMinHz.setText(String.valueOf(state.minHz));
                maxHz = state.maxHz;
                jMaxHz.setText(String.valueOf(state.maxHz));
                isLinear = state.linear;
                jLinear.setSelected(state.linear);
                showCs = state.cs;
                showGrandStaff = state.gs;
                showPicture = state.picture;
                repaint();
            }
        }
        
        //set time and amplitude on click by inserting the point into the vectors.
        public void mousePressed(MouseEvent e) {
            requestFocus();
            cacheSize();
            dragx = e.getX();
            dragy = e.getY();
            // check for curve selection
            // if (e.getButton() == MouseEvent.BUTTON3) { -- was button3
            if (e.isShiftDown()) { // selection is by using shift key
                int closest = 1000;
                Curve selection = null;
                for (Curve c : curves) {
                    int dist = c.distance(dragx, dragy);
                    if (dist < closest) {
                        closest = dist;
                        selection = c;
                    }
                }
                if (selection != null && closest < 8) {
                    selection.selected = !selection.selected;
                    repaint();
                    is_dragging = false;
                    return;
                }
                
                System.out.println("No curve selected");
                repaint();
                is_dragging = true;
                return;
            }

            // if we click on a selection, start dragging selections
            for (Curve c : curves) {
                if (c.selected && c.distance(dragx, dragy) < 8) {
                    is_dragging = true;
                    mouseDown = true;
                    break; // we are going to drag some selections
                }
            }
            if (is_dragging) { // continuing from the break just above...
                // make copies of selections so undo will work
                for (int i = 0; i < curves.size(); i++) {
                    Curve c = curves.get(i);
                    if (c.selected) {
                        curves.set(i, new Curve(c, 0));
                    }
                }
                return;
            }

            // start new curve
            startNewCurve();
            mouseDown = true;
            changed = true;
            System.out.println("mouseDown true\n");
            this.requestFocus();
            currentTime = e.getX();
            currentHz = e.getY();
            insertInOrder(x2time(currentTime), y2hz(currentHz));
            repaint();
        }
    

        public void mouseDragged(MouseEvent e) {
            // if this is not the left button drawing event, return
            if (!mouseDown) return;
            int x = e.getX();
            int y = e.getY();

            if (is_dragging && ((x != dragx) || (y != dragy))) {
                // move selections
                for (Curve c : curves) {
                    if (c.selected) {
                        c.displace(x - dragx, y - dragy);
                        changed = true;
                    }
                }
                dragx = x;
                dragy = y;
                if (changed) setModified();
                repaint();
                return;
            }

            currentTime = clipX(x);
            currentHz = clipY(y);
            if (currentTime <= left + width && currentTime >= left && 
                currentHz >= top && currentHz <= top + height) {

                insertInOrder(x2time(currentTime), y2hz(currentHz));
                repaint();
            }
        }

        
        public void mouseReleased(MouseEvent e) {
            requestFocus(); // otherwise, delete key may not work
            if (e.isShiftDown() && is_dragging) {
                int x = e.getX();
                int y = e.getY();
                // if no movement and curve was not selected, then deselect all
                if (dragx == x && dragy == y) {
                    for (Curve c : curves) {
                        c.selected = false;
                    }
                    is_dragging = false;
                    repaint();
                    return;
                }

                for (Curve c : curves) {
                    for (int j = 0; j < c.times.size(); j++) {
                        int px = time2x(c.times.get(j));
                        int py = hz2y(c.freqs.get(j));
                        if (dragx < px && px < x && dragy < py && py < y) {
                            c.selected = true;
                            break; // go to next curve
                        }
                    }
                }
                is_dragging = false;
                repaint();
                return;
            }
            if (e.getButton() != MouseEvent.BUTTON1)
                return;

            if (changed) {
                changed = false;
                // if curve has only one point, delete it. You must drag to
                // enter a new curve.
                int last = curves.size() - 1;
                if (curves.get(last).times.size() <= 1) {
                    curves.remove(last);
                } else {
                    history.save(this);
                }
            }
            mouseDown = false;
            is_dragging = false;
        }


        public void duplicateCmd() {
            int len = curves.size();
            System.out.println("Before, " + len + " curves");
            for (int i = 0; i < len; i++) {
                Curve c = curves.get(i);
                if (c.selected) { // copy the curve with offset
                    System.out.println("copying curve " + c);
                    curves.add(new Curve(c, 10));
                    c.selected = false;
                }
            }
            System.out.println("After, " + curves.size() + " curves");
            setModified();
            canvas.history.save(canvas);
            repaint();
        }


        public void stretchCmd() {
            String input = JOptionPane.showInputDialog("Enter stretch factor");
            double amt;
            try {
                amt = Double.parseDouble(input);
            } catch(NumberFormatException exception) {
                amt = 1.0;
            }
            // find earliest time:
            double start = 100000;
            for (Curve c : curves) {
                if (c.selected && c.times.size() > 0 && 
                    c.times.get(0) < start) {
                    start = c.times.get(0);
                }
            }
            // now start is the earliest time of any selected curve
            for (int i = 0; i < curves.size(); i++) {
                Curve c = curves.get(i);
                if (c.selected) { // copy the curve and stretch
                    curves.set(i, c.stretch(start, 0, amt));
                }
            }
            setModified();
            canvas.history.save(canvas);
            repaint();
        }


        // stretch each selected curve to start at the same time
        public void alignBeginningCmd() {
            // start time will be the median of all start times
            ArrayList<Double> times = new ArrayList<Double>();
            for (Curve c : curves) {
                if (c.selected && c.times.size() > 0) {
                    times.add(c.times.get(0));
                }
            }
            if (times.size() < 2) return; // nothing to align
            Collections.sort(times);
            // if even number of elements, choose the earlier of the middle 2
            double median = times.get((times.size() - 1) / 2);
            for (int i = 0; i < curves.size(); i++) {
                Curve c = curves.get(i);
                if (c.selected && c.times.size() > 0) {
                    double end = c.times.get(c.times.size() - 1);
                    curves.set(i, c.stretch(median, end, 0));
                }
            }
            setModified();
            canvas.history.save(canvas);
            repaint();
        }


        // stretch each selected curve to start at the same time
        public void alignEndingCmd() {
            // end time will be the median of all end times
            ArrayList<Double> times = new ArrayList<Double>();
            for (Curve c : curves) {
                if (c.selected && c.times.size() > 0) {
                    times.add(c.times.get(c.times.size() - 1));
                }
            }
            if (times.size() < 2) return; // nothing to align
            Collections.sort(times);
            // if even number of elements, choose the earlier of the middle 2
            double median = times.get((times.size() - 1) / 2);
            for (int i = 0; i < curves.size(); i++) {
                Curve c = curves.get(i);
                if (c.selected && c.times.size() > 0) {
                    double begin = c.times.get(0);
                    curves.set(i, c.stretch(begin, median, 0));
                }
            }
            setModified();
            canvas.history.save(canvas);
            repaint();
        }


        public void thinCmd() {
            int j = 0;
            for (int i = 0; i < curves.size(); i++) {
                Curve c = curves.get(i);
                c.thin();
                if (c.times.size() >= 2) {
                    curves.set(j++, c);
                }
            }
            while (curves.size() > j) {
                curves.remove(curves.size() - 1);
            }
            setModified();
            canvas.history.save(canvas);
            repaint();
        }
            

        public void startNewCurve(String waveform, String env) {
            curves.add(new Curve(waveform, env));
        }
            

        public void startNewCurve() {
            startNewCurve((String) waveformName.getSelectedItem(),
                          (String) envName.getSelectedItem());
        }


        // convert time coordinate to time in seconds
        private double x2time(int x) {
            return (x - left) * maxT / width;
        }
        
        private int time2x(double time) {
            return (int) Math.round(time * width / maxT) + left;
        }        
        
        // convert amp coordinate to real amplitude
        private double y2hz(int y) {
            double mx, mn, r;
            if (isLinear) {
                mx = maxHz;
                mn = minHz;
                r = mx - mn;
                return mx - ((y - top) * r / height);
            }
            mx = Math.log(maxHz);
            mn = Math.log(minHz);
            r = mx - mn;
            return Math.exp(mx - ((y - top) * r / height));
        }
        
        private int hz2y(double hz) {
            double mx, mn;
            if (isLinear) {
                mx = maxHz;
                mn = minHz;
            } else {
                mx = Math.log(maxHz);
                mn = Math.log(minHz);
                hz = Math.log(hz);
            }
            double r = mx - mn;
            return (int) Math.round((mx - hz) * height / r) + top;
        }
        

        //insert the time and amplitude in the vectors in time sorted order
        private void insertInOrder(double time, double amp) {
            Curve c = curves.get(curves.size() - 1);
            c.insertInOrder(time, amp);
            changed = true;
            setModified();
        }

            
    public void keyPressed(KeyEvent event) {
        System.out.println("key event detected");
        if (event.getKeyCode() == KeyEvent.VK_DELETE) {
            int i =  0;
            while (i < curves.size()) {
                if (curves.get(i).selected) {
                    curves.remove(i);
                } else {
                    i++;
                }
            }
            canvas.history.save(canvas);
            System.out.println("curve should be deleted");
            repaint();
        }
    }
        
    //fill rest of mouse functions
    public void mouseEntered(MouseEvent e) {}
    public void mouseExited(MouseEvent e) {}
    public void mouseClicked(MouseEvent e) {}
    public void mouseMoved(MouseEvent e) {}
    public void keyReleased(KeyEvent event) {}
    public void keyTyped(KeyEvent event) {}
    }
}
