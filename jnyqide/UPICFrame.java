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

package jnyqide;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.image.*;
import java.util.*;
import java.text.*;
import java.io.*;
import javax.imageio.ImageIO;

@SuppressWarnings("serial")
public class UPICFrame extends JInternalFrame implements ActionListener {

    double EPSILON = 0.00000001; // small number -- allow for rounding error
    int LEFT_BORDER = 3; // inset everything by 3 pixels
    int TOP_BORDER = 3;
    int BOTTOM_BORDER = 5;
    int RIGHT_BORDER = 5;
    MainFrame mainframe;
    
    private JPanel mainPanel;
    
    private JTextPane jInputArea;
    private PiecewiseCanvas canvas;
    
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
    private String name; // name of file
            
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

    private void AddMenuItem(JMenu menu, String name, String cmd, int code) {
        AddMenuItem(menu, name, cmd, code, false, false);
    }

    private void AddMenuItem(JMenu menu, String name, String cmd, int code, 
                             boolean shift) {
        AddMenuItem(menu, name, cmd, code, shift, false);
    }

    private void AddMenuItem(JMenu menu, String name, String cmd, 
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
        name = "UPIC Editor";
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

        JMenu backgroundMenu = new JMenu("Background");
        AddMenuItem(backgroundMenu, "C's", "showCs", 
                    java.awt.event.KeyEvent.VK_C, false, true);
        AddMenuItem(backgroundMenu, "Grand Staff", "showGrandStaff", 
                    java.awt.event.KeyEvent.VK_G, false, true);
        AddMenuItem(backgroundMenu, "Show Picture", "showPicture",
                    java.awt.event.KeyEvent.VK_V, false, true);
        AddMenuItem(backgroundMenu, "Load Picture...", "loadPicture", -1);

        menuBar.add(fileMenu);
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
        JFileChooser chooser = new JFileChooser();
        SalFileFilter salFilter = new SalFileFilter();
        chooser.setFileFilter(salFilter);
        File tmpfile;
        String tmp;

	File curdir = new File(mainframe.currentDir);
	chooser.setCurrentDirectory(curdir);
        
        while (true) { // loop until file is chosen
            int returnVal = chooser.showSaveDialog(this);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                tmpfile = chooser.getSelectedFile();
                tmp = tmpfile.getName();
                System.out.println("You chose to save this file: " + tmp);
                tmp = tmp.toLowerCase();
                if (tmp.endsWith(".sal")) break;
                JOptionPane dialog = new JOptionPane();
                System.out.println("creating dialog");
                int result = dialog.showConfirmDialog(this, 
                         "Do you really want to save a file without a " +
                         ".lsp or .sal extension?", "Warning", 
                         JOptionPane.YES_NO_OPTION);
                System.out.println("return from dialog " + result);
                if (result == JOptionPane.YES_OPTION) break;
            } else { // file chooser cancel, early return
	        return false;
            }
	}
        name = tmp;
        file = tmpfile;
        mainframe.changeDirectory(mainframe.fileDirectory(file));
        modified = true; // for saveUpicData, do not need to add "*"
        return saveUpicData();
    }


    private void setModified() {
        if (modified) return;
        setTitle(name + "*");
        modified = true;
    }

    public boolean saveUpicData()
    // saves the file if there is a file name, otherwise calls saveAs.
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
                setTitle(name);
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
        else if (cmd.equals("envNameSelected")) envNameSelected();
        else if (cmd.equals("waveformNameSelected")) waveformNameSelected();
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
            canvas.repaint();
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
        name = fileToOpen.getName();
        setTitle(name);
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
                canvas.startNewCurve();
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
        JFileChooser chooser = new JFileChooser();
        SalFileFilter salFilter = new SalFileFilter();
        chooser.addChoosableFileFilter(salFilter);
        File curdir = new File(mainframe.currentDir);
        chooser.setCurrentDirectory(curdir);
        int returnVal = chooser.showOpenDialog(this);
        System.out.println("openUpicData chooser returns " + returnVal);
        file = null; // in case open fails
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            file = chooser.getSelectedFile();
            mainframe.changeDirectory(mainframe.fileDirectory(file));
            System.out.println("reading from " + file);
            readUpicData(file);
            modified = false;
        }
    }

        /*        data = data.toLowerCase();
        //System.out.println("openUpicData: data |" + data + "| len " + data.length());
        envName.removeAllItems(); // clear and reload combo box
        while (data.length() > 0) {
            int eolx = data.indexOf("\n");
            if (eolx < 0) return; // shouldn't happen, but bail if it does
            String line = data.substring(0, eolx);
            //System.out.println("openUpicData: line " + line);
            data = data.substring(eolx + 1);
            String name = line.substring(0, line.indexOf(' '));
            System.out.println("openUpicData: name " + name);
            String env = line.substring(name.length() + 1);
            System.out.println("openUpicData: env " + env);
            if (name.length() > 0) envColl.put(name, env);
            envName.addItem(name);
            } */
    
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

        public Curve(String wave, String env) {
            waveform = wave; 
            envelope = env;
            times = new ArrayList<Double>();
            freqs = new ArrayList<Double>();
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
        public int selection;
        public History history;

        public boolean mouseDown = false; // used to detect version for undo
        private boolean changed = false; // used to detect version for undo
    
        //  Vectors to store the absolute parameters of the points in the 
        // envelope.
        public ArrayList<Curve> curves = new ArrayList<Curve>();
        public int selectedCurve;
        private int left, top, width, height;

        private BufferedImage background;

        // Constructor
        public PiecewiseCanvas() {
            setBackground(Color.white);
            addMouseListener(this);
            addMouseMotionListener(this);
            addKeyListener(this);
            selection = -1;
            history = new History();
            history.save(this);
            selectedCurve = -1;
            background = null;
        }
        
            
        public void loadPicture() {
            JFileChooser chooser = new JFileChooser();
            File curdir = new File(mainframe.currentDir);
            chooser.setCurrentDirectory(curdir);
            int returnVal = chooser.showOpenDialog(this);
            File file = null; // in case open fails
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                file = chooser.getSelectedFile();
                mainframe.changeDirectory(mainframe.fileDirectory(file));
                System.out.println("reading image from " + file);
                try {
                    background = ImageIO.read(file);
                } catch (Exception e) {
                    System.out.println("Image read failed " + e);
                    JOptionPane.showMessageDialog(this, 
                                                  "Could not open image file");
                    background = null;
                }
            }
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
    
        //        public String getExpression() {
        //            String env;
        //        
        //            /*if (type == PWL_TYPE) env = (valueType ? "pwlv" : "pwl");
        //            else env = (valueType ? "pwev" : "pwe");*/
        //
        //            env = "pwlv";
        //
        //            return outputEnv(env, true, 0.0, // getMinT(), 
        //                             getMaxT(), getMinHz(), getMaxHz());
        //        }
    
        //draw the canvas image
        public void paint(Graphics g) {
            super.paint(g); 
        
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
            for (int j = 0; j < curves.size(); j++) {
                g.setColor(selectedCurve == j ? Color.red : Color.blue);
                Curve c = curves.get(j);
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
                selection = -1;
                repaint();
            }
        }
        
        //set time and amplitude on click by inserting the point into the vectors.
        public void mousePressed(MouseEvent e) {
            requestFocus();
            cacheSize();
            // check for curve selection
            if (e.getButton() == MouseEvent.BUTTON3) {
                for (int x = 0; x < curves.size(); x++) {
                    Curve c = curves.get(x);
                    for (int y = 0; y < c.times.size(); y++) {
                        if (Math.abs(time2x(c.times.get(y)) - e.getX()) <= 8 && 
                            Math.abs(hz2y(c.freqs.get(y)) - e.getY()) <= 8) {
                            selectedCurve = x;
                            repaint();
                            System.out.println("Selected curve " + x);
                            System.out.println("e.getX(): " + e.getX()+ 
                                               ", e.getY: " + e.getY());
                            System.out.println("c.times.get(y): " + 
                                               c.times.get(y) + 
                                               ", c.freqs.get(y): " + 
                                               c.freqs.get(y));
                            return;
                        }
                    }
                }
                
                selectedCurve = -1;
                repaint();
                System.out.println("No curve selected");
                return;
            }

            // not button3, so start a new curve
            startNewCurve();
            mouseDown = true;
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
            currentTime = clipX(e.getX());
            currentHz = clipY(e.getY());

            if (currentTime <= left + width && currentTime >= left && 
                currentHz >= top && currentHz <= top + height) {

                insertInOrder(x2time(currentTime), y2hz(currentHz));
                repaint();
            }
        }

        
        public void startNewCurve() {
            curves.add(new Curve((String) waveformName.getSelectedItem(),
                                 (String) envName.getSelectedItem()));
        }

        public void mouseReleased(MouseEvent e) {
            requestFocus(); // otherwise, delete key may not work
            if (e.getButton() != MouseEvent.BUTTON1)
                return;
            if (changed) {
                history.save(this);
                changed = false;
            }
            mouseDown = false;
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
        
        private void addPoint(int i, double absT, double absA) {
            addPoint(curves.size() - 1, i, absT, absA);
        }

        private void addPoint(int curve, int i, double absT, double absA) {
            //System.out.println("addPoint: " + i + " " + absT + " " + absA);
            Curve c = curves.get(curve);
            c.times.add(i, absT);
            c.freqs.add(i, Math.max(absA, 0.0));
            //System.out.println("addPoint time: " + absT + ", text " + form.format(absT));
            selection = i;
            changed = true;
            setModified();
        }


        //insert the time and amplitude in the vectors in time sorted order
        private void insertInOrder(double time, double amp) {
            Curve c = curves.get(curves.size() - 1);
            int i = c.times.size();
            while (i > 0 && time < c.times.get(i - 1)) {
                i--;
            }
            addPoint(i, time, amp);
        }

            
        // Check if mouse click corresponds to existing envelope point
        //    return index of point or -1 if no point is close
    //        private int getSelection(int x, int y) {
    //            int cutoff = 7;
    //            int bestDist = cutoff * 2;
    //            int bestIndex = -1;
    //            if (times == null) return bestIndex;
    //            for (int i = 0; i < times.get(curCurve).size(); i++) {
    //                int xi = time2x(times.get(curCurve).get(i));
    //                int yi = hz2y(freqs.get(curCurve).get(i));
    //                int dx = Math.abs(x - xi);
    //                int dy = Math.abs(y - yi);
    //                if (dx < cutoff && dy < cutoff && dx + dy < bestDist) {
    //                    bestDist = dx + dy;
    //                    bestIndex = i;
    //                }
    //            }
    //            selection = bestIndex;
    //            return bestIndex;
    //        }
        
    //Check if mouse click corresponds with existing envelope point (to select point)
    //    private boolean checkSelection(int time, int amp) {
    //            int i = getSelection(time, amp);
    //            if (i < 0) return false;
    //            repaint();
    //            return true;
    //        }

    //output the envelope as a string
        //    public String outputEnv(String envType, boolean valueType, double minTime, double maxTime, double minHertz, double maxHertz) {
        //        String outputStr = "(sim";
        //        
        //        for (int j = 0; j < times.size() - 1; j++) {
        //            int start = 0;
        //            outputStr += " (osc-tri (" + envType;
        //            
        //            if (valueType) { // insert initial value
        //                if (within(times.get(j).get(0), 0.0, EPSILON)) {
        //                    outputStr += " " + form.format(freqs.get(j).get(0));
        //                    start = 1;
        //                } else
        //                    outputStr += " " + form.format(impliedAmp());
        //                }    
        //        
        //            for (int i = start; i < freqs.get(j).size(); i++) {
        //                    double time = times.get(j).get(i);
        //                    double amp  = freqs.get(j).get(i);
        //
        //                    if (time == 1)
        //                        break;
        //        
        //                    outputStr += " " + form.format(time) + " " + form.format(amp);
        //            }
        //            
        //                if (valueType) { // we're already ending with a value
        //                    if (within(times.get(j).lastElement(), maxTime, EPSILON)) {
        //                        // we're done because we output at maxTime
        //                    } else
        //                        outputStr += " " + form.format(maxTime) + " " + form.format(impliedAmp());
        //                } else
        //                    outputStr += " " + form.format(maxTime);
        //            
        //                outputStr += "))";
        //        }
        //        outputStr += ")";
        //        return outputStr;
        //    }
    
        // parse envelope from string and prepare to edit
    //    public void setEnv(String envData) {
    //
    //        System.out.println("\nCALLED setEnv\n");
    //        
    //       // System.out.println("setEnv: envData " + envData.substring(0, 100) + " ... " + envData.substring(envData.length()-5));
    //        if (envData == null) return; // just in case
    //        //check if envelope exists in collection
    //        //boolean nameIsEnv = false;
    //
    //        // trim the open and close parens from envData
    //        int startx = envData.indexOf("(") + 5;
    //        // if no open paren, startx will be 0
    //         int endx = envData.lastIndexOf(")");
    //        if (endx < 0) endx = envData.length();
    //        envData = envData.substring(startx, endx);
    //
    //        int x = 0;
    //        while (envData.indexOf("(") > -1) {
    //            String strCurve = envData.substring(envData.indexOf("(")+10, envData.indexOf(")"));
    //            envData = envData.substring(envData.indexOf(")")+2);
    //
    //            StringTokenizer st = new StringTokenizer(strCurve);
    //            String type = st.nextToken();
    //            System.out.println("setEnv: type " + type);
    //
    //            valueType = type.endsWith("v");
    //
    //            if (times.size() <= x) {
    //                times.add(new Vector<Double>());
    //                freqs.add(new Vector<Double>());
    //            }
    //
    //            times.get(x).removeAllElements();
    //            freqs.get(x).removeAllElements();
    //            int i = 0;
    //            // pretend mouse is down to avoid making each point undo-able
    //            boolean save = mouseDown;
    //            mouseDown = false;
    //            double time, amp;
    //            if (valueType) { // first element is value
    //                amp = new Double(st.nextToken().trim());
    //                addPoint(x, i++, 0.0, amp);
    //            }
    //            while (st.countTokens() >= 2) {
    //                String token1 = st.nextToken().trim();
    //                time = new Double(token1);
    //                String token2 = st.nextToken().trim();
    //                amp = new Double(token2);
    //                addPoint(x, i++, time, amp);
    //                /*System.out.println("time " + token1 + " amp " + token2 + 
    //                  " size " + times.get(x).size());*/
    //            }
    //            
    //            mouseDown = save; // restore the mouseDown state
    //            if (!valueType) { // last element is time
    //                maxT.setText(st.nextToken());
    //            }
    //            
    //            x++;
    //        }
    //        
    //        times.add(new Vector<Double>());
    //        freqs.add(new Vector<Double>());
    //        curCurve = x;
    //        System.out.println("curCurve: " + curCurve);
    //        
    //        modified = false;
    //        repaint();
    //    }


    public void keyPressed(KeyEvent event) {
        System.out.println("key event detected");
        if (event.getKeyCode() == KeyEvent.VK_DELETE) {
            curves.remove(selectedCurve);
            selectedCurve = -1;
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
