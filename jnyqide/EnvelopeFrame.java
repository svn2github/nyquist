// Code: Chris Yealy, 5/2/06
// Edited: Derek D'Souza 5/3/07
// Edited: Roger Dannenberg 23Jul07

// eq editor needs to do same with workspace

/* Saving and Restoring:
 *  the Save button saves one envelope locally, to Nyquist, and saves the 
 *   updated workspace
 *  the Load button loads all envelopes from Nyquist for editing. If no
 *   envelopes are loaded, suggests (load "workspace")
 *  selecting an envelope from the list saves the current envelope 
 *   (locally, to Nyquist, and to the workspace) and switches the editor
 *   to the selected envelope
 *  delete button removes the envelope from the workspace after a confirm
 */

/*
 Graphics organization:
 
 myParent -- the MainFrame
    EnvelopeFrame -- this editor, a JInternalFrame
        mainPanel -- a JPanel
            topPanel -- a JPanel
                envNamePanel -- a JPanel, border "Current Envelope"
                    currEnvNameLabel -- a JLabel("Name:")
                    currEnvName -- a JTextField("ENVELOPE", 20)
                    saveEnv -- a JButtton("Save")
                    envTypeLabel - a JTextField("Type", ...)
                    envTypes - a JComboBox
                envListPanel -- a JPanel, border "Saved Envelopes List"
                    envName -- a JComboBox
                    loadEnv -- a JButton("load")
                    deleteEnv -- a JButton("delete")
                envPointsPanel - a JPanel, border Envelope Points
                    envTimesLabel - a JLabel("Time:")
                    envTimes - JComboBox
                    envAmplitudesLabel - a JLabel("Amplitudes:")
                    envAmplitudes -  JTextField  
                    addPoint - a JButton("Add Point")
                    deletePoint - a JButton("Delete Point")
                    updatePoint - a JButton("Update Point")
                paramPanel -- a JPanel
                    rangePanel - a JPanel, border "Range Parameters"
                        maxTL - a JLabel("Stop Time")
                        maxT - a JTextField
                        minAL - a JLabel("Min. Amplitude")
                        maxAL - a JLabel("Max. Amplitude")
                        minA - a JTextField
                        maxA - a JTextField
                    update - JButton("Update Range"
            envEditPanel -- a JPanel, border "Graphic Envelope Editor"
                envEditButtonPanel - a JPanel
                    undo - a JButton("Undo")
                    redo - a JButton("Redo")
                    clear - a JButton("Clear")
                    dispCoord - a JButton("Coordinates")
                    output - a JButton("Output Envelope")
                canvas - a PiecewiseCanvas (a JPanel)
                
 to drag point:
    on mouse pressed, set selection to index of point
    on mouse drag, delete selection and insert new point
    
if endpoints are specified, e.g. if times[0] is 0 or last value in times
    matches the ending time, then use PWLV or PWEV version
    Be careful not to drop first/last point unless the time matches the
    begin/end time.


 */

package jnyqide;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.image.BufferedImage;
import java.util.Vector;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Enumeration;
import java.io.File;
import java.lang.Math.*;
import java.text.DecimalFormat;
import jnyqide.*;


public class EnvelopeFrame extends JInternalFrame implements ActionListener {

    double EPSILON = 0.00000001; // small number -- allow for rounding error
    int LEFT_BORDER = 3; // inset everything by 3 pixels
    int TOP_BORDER = 3;
    int BOTTOM_BORDER = 5;
    int RIGHT_BORDER = 5;
    MainFrame myParent;
    
    //JComponents for envelope window
    private JComboBox envTypes;
    
    private JPanel mainPanel;
    
    private JTextPane jInputArea;
    //private JPanel canvasPanel;
    private PiecewiseCanvas canvas;
    
    private JTextField envAmplitudes;
    // private JTextField minT;
    private JTextField maxT;
    private JTextField minA;
    private JTextField maxA;
    private JTextField currEnvName;
    private JComboBox envName;
    private JComboBox envTimes;
    private JButton dispCoord;
    private String currentEnvName;
    public String[] types = {"Piecewise Linear", "Piecewise Exponential"};
    public int PWL_TYPE = 0;
    public int PWE_TYPE = 1;
    // when saving envelopes, we copy current envelope to the collection,
    // but this makes the combobox think that an envelope was selected, 
    // and we get a request to save the same envelope were saving. To 
    // avoid this, the "saving" flag is used to disable the check.
    private boolean saving = false;
            
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
    
    // Constructor
    public EnvelopeFrame(final MainFrame parent, JTextPane inputArea)	{
        super();
        setTitle("Untitled");
        myParent = parent;
        jInputArea = inputArea;
        //canvasPanel = new JPanel();
        //canvasPanel.setPreferredSize(new Dimension(575, 256));
        mainPanel = (JPanel) getContentPane();
                        
        envColl = new Hashtable<String, String>();
        setTitle("Piecewise Envelope Generator");
        setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
        final EnvelopeFrame envelopeFrame = this;
        modified = false;
        addInternalFrameListener(
            new InternalFrameListener() {
                public void internalFrameClosing(InternalFrameEvent e) {
                    //System.out.println("FrameClosing");
                    int r = JOptionPane.OK_OPTION;
                    if (envelopeFrame.modified) {
                        r = JOptionPane.showConfirmDialog(envelopeFrame,
                                "Really close without saving?",
                                "Alert", JOptionPane.OK_CANCEL_OPTION);
                    }
                    if (r == JOptionPane.OK_OPTION) {
                        envelopeFrame.dispose();
                    }
                }
                public void internalFrameOpened(InternalFrameEvent e) {
                }
                public void internalFrameClosed(InternalFrameEvent e) {
                    parent.disconnectEnv();
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
	      
        JLabel currEnvNameLabel = new JLabel("Name: ");
        currEnvName = new JTextField("envelope", 10);
        currentEnvName = "envelope";
	
        JButton saveEnv = new JButton("Save");
        saveEnv.addActionListener(this);
    	saveEnv.setActionCommand("saveEnvelopes");
        
        JLabel currEnvTypeLabel = new JLabel("Type: ");
	envTypes = new JComboBox(types);
        envTypes.addActionListener(this);
        envTypes.setActionCommand("envTypeChanged");
            
    	//  components for envelope list panel
    	envName = new JComboBox();
        envName.setEditable(false);
        envName.addActionListener(this);
        envName.setActionCommand("envNameSelected");
    
        JButton loadEnv = new JButton("Load");
        loadEnv.addActionListener(this);
        loadEnv.setActionCommand("loadEnvelope");
    
    	JButton deleteEnv = new JButton("Delete");
    	deleteEnv.addActionListener(this);
    	deleteEnv.setActionCommand("deleteEnvelope");	
    
    	JPanel envNamePanel = new JPanel();
    	envNamePanel.setBorder(BorderFactory.createTitledBorder("Current Envelope"));
    	GridBagLayout layout0 = new GridBagLayout();
    	envNamePanel.setLayout(layout0);
    
    	GridBagConstraints cons0 = new GridBagConstraints();
    
    	cons0.fill = GridBagConstraints.NONE;
    	cons0.anchor = GridBagConstraints.EAST;
    	cons0.weightx = 0;
    	cons0.weighty = 0;
    	cons0.gridx = 0;
    	cons0.gridy = 0;
    	cons0.gridheight = 1;
    	cons0.gridwidth = 1;
    	envNamePanel.add(currEnvNameLabel, cons0);
    	cons0.anchor = GridBagConstraints.WEST;
    	cons0.gridx = 1;
    	envNamePanel.add(currEnvName, cons0);
    	cons0.anchor = GridBagConstraints.CENTER;
    	cons0.gridx = 2;
    	envNamePanel.add(saveEnv, cons0);
    	cons0.anchor = GridBagConstraints.EAST;
        cons0.gridx = 0;
        cons0.gridy = 1;
        envNamePanel.add(currEnvTypeLabel, cons0);
    	cons0.anchor = GridBagConstraints.WEST;
        cons0.gridx = 1;
        cons0.gridwidth = 2;
        envNamePanel.add(envTypes, cons0);
    
    	JPanel envListPanel = new JPanel();
    	envListPanel.setBorder(BorderFactory.createTitledBorder("Saved Envelopes List"));	
    	envListPanel.add(envName);
    	envListPanel.add(loadEnv);
    	envListPanel.add(deleteEnv);
    	
    	envTimes = new JComboBox();
    	envTimes.setEditable(true);
        envTimes.addActionListener(this);
        envTimes.setActionCommand("envTimeChange");
        envTimes.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                System.out.println("itemStateChanged " + e);
            }
        });
    	JLabel envTimesLabel = new JLabel("Time: ");
    
    	envAmplitudes = new JTextField(6);	 
    	JLabel envAmplitudesLabel = new JLabel("Ampl: ");
        envTimes.setPreferredSize(envAmplitudes.getPreferredSize());
        
    	JButton addPoint = new JButton("Add Point");
    	addPoint.addActionListener(this);
    	addPoint.setActionCommand("addPoint");
    
    	JButton deletePoint = new JButton("Delete Point");
    	deletePoint.addActionListener(this);
    	deletePoint.setActionCommand("deletePoint");
        
        JButton updatePoint = new JButton("Update Point");
    	updatePoint.addActionListener(this);
    	updatePoint.setActionCommand("updatePoint");
    
    	GridBagLayout layout1 = new GridBagLayout();
    	JPanel envPointsPanel = new JPanel();
    	envPointsPanel.setBorder(BorderFactory.createTitledBorder("Envelope Points"));
    	envPointsPanel.setLayout(layout1);
    
    	GridBagConstraints cons = new GridBagConstraints();
    
    	cons.fill = GridBagConstraints.NONE;
    	cons.anchor = GridBagConstraints.EAST;
    	cons.weightx = 0;
    	cons.weighty = 0;
    	cons.gridx = 0;
    	cons.gridy = 0;
    	cons.gridheight = 1;
    	cons.gridwidth = 1;
    	envPointsPanel.add(envTimesLabel, cons);
    	cons.anchor = GridBagConstraints.WEST;
    	cons.gridx = 1;
    	envPointsPanel.add(envTimes, cons);
    	cons.anchor = GridBagConstraints.EAST;
    	cons.gridx = 0;
    	cons.gridy = 1;
    	envPointsPanel.add(envAmplitudesLabel, cons);
    	cons.anchor = GridBagConstraints.WEST;
    	cons.gridx = 1;
    	envPointsPanel.add(envAmplitudes, cons);
    	cons.anchor = GridBagConstraints.CENTER;
    	cons.gridx = 2;
    	cons.gridy = 0;
    	envPointsPanel.add(addPoint, cons);
    	cons.gridy = 1;
    	envPointsPanel.add(deletePoint, cons);
        cons.gridy = 2;
        envPointsPanel.add(updatePoint, cons);
            	
	// panel to contain time and amplitude parameters
        JPanel rangePanel = new JPanel();
        JPanel paramPanel = new JPanel();
        paramPanel.setBorder(BorderFactory.createTitledBorder("Range"));
        rangePanel.setLayout(new GridBagLayout());
        paramPanel.setLayout(layout1);
	
	// components for parameter panel
        ActionListener stateChange = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                canvas.history.save(canvas);
            }
        };
        maxT = new JTextField("1.0", 5);
        minA = new JTextField("0.0", 5);
        maxA = new JTextField("1.0", 5);
        	
        JLabel maxTL = new JLabel("Stop: ");
        JLabel minAL = new JLabel("Min: ");
        JLabel maxAL = new JLabel("Max: ");
			
        cons.gridx = 2;
        cons.gridy = 0;
	rangePanel.add(maxTL, cons);
        cons.gridx = 3;
        cons.gridy = 0;
	rangePanel.add(maxT, cons);
        cons.gridx = 0;
        cons.gridy = 1;
	rangePanel.add(minAL, cons);	
        cons.gridx = 2;
	rangePanel.add(maxAL, cons);
        cons.gridx = 1;
	rangePanel.add(minA, cons);
        cons.gridx = 3;
	rangePanel.add(maxA, cons);
	JButton update = new JButton("Update Range");
		
	update.addActionListener(new ActionListener() {
	    public void actionPerformed(ActionEvent e) {			
		if (getMaxT() <= 0) {
                    JOptionPane.showMessageDialog(mainPanel, 
                            "Stop Time cannot be negative or zero");
                } else if (getMinA() > getMaxA()) {
                    JOptionPane.showMessageDialog(mainPanel, 
                            "Minimum Amplitude cannot be greater than Maximum Amplitude");
                } else if ((canvas.times.size() > 0) &&
                           (getMaxT() < canvas.times.lastElement())) {
                    JOptionPane.showMessageDialog(mainPanel, 
                        "Stop Time is less than the time of an existing envelope point");
                } else {
                    modified = true;
                    canvas.history.save(canvas);
                    canvas.repaint();
                    return;
		}
                // an error occurred, reset the Range (using complete restore)
                canvas.restore();
            }
        });
	
	//insert components into the larger panels
	cons.fill = GridBagConstraints.NONE;
    	cons.anchor = GridBagConstraints.WEST;
    	cons.weightx = 0;
    	cons.weighty = 0;
    	cons.gridx = 0;
    	cons.gridy = 0;
    	cons.gridheight = 1;
    	cons.gridwidth = 1;
    	//cons.insets = new Insets(5,0,0,5);
    	paramPanel.add(rangePanel, cons);
    	cons.fill = GridBagConstraints.NONE;
    	cons.anchor = GridBagConstraints.CENTER;
    	cons.weightx = 0;
    	cons.weighty = 0;
    	cons.gridx = 0;
    	cons.gridy = 1;
    	cons.gridheight = 1;
    	cons.gridwidth = 1;
    	//cons.insets = new Insets(0,0,0,5);		
        paramPanel.add(update, cons);
	
        // components for envelope edit panel
	JButton undo = new JButton("Undo");
        undo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                canvas.history.undo();
                canvas.restore();
            }
        });
	JButton redo = new JButton("Redo");
        redo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                canvas.history.redo();
                canvas.restore();
            }
        });
	JButton clear = new JButton("Clear");
	clear.addActionListener(new ActionListener() {
	    public void actionPerformed(ActionEvent e) {
                System.out.println("calling canvas.clear\n");
	        canvas.clear();
            }
	});
	dispCoord = new JButton("Coordinates");
	dispCoord.addActionListener(new ActionListener() {
	    public void actionPerformed(ActionEvent e) {
                displayCoord = !displayCoord;
                canvas.repaint();
            }
        });
        JButton output = new JButton("Output Envelope");
        output.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String outputStr = canvas.getExpression();
                jInputArea.setText(jInputArea.getText().concat(outputStr));
            }
        });
		
        JPanel envEditButtonPanel = new JPanel();
        envEditButtonPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 10, 0));
        envEditButtonPanel.add(undo);
        envEditButtonPanel.add(redo);
        envEditButtonPanel.add(clear);
        envEditButtonPanel.add(dispCoord);
        envEditButtonPanel.add(output);	
        JPanel topPanel = new JPanel();
        //topPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 30, 0));
        topPanel.setLayout(new GridBagLayout());
        cons.anchor = GridBagConstraints.NORTHWEST;
        cons.gridx = 0;
        cons.gridy = 0;
        topPanel.add(envNamePanel, cons);
        cons.gridx = 1;
        topPanel.add(envListPanel, cons);
        cons.gridx = 0;
        cons.gridy = 1;
        topPanel.add(envPointsPanel, cons);
        cons.gridx = 1;
        topPanel.add(paramPanel, cons);

        JPanel envEditPanel = new JPanel();
        envEditPanel.setBorder(BorderFactory.createTitledBorder(
                "Graphical Envelope Editor"));
        //envEditPanel.setLayout(new BoxLayout(envEditPanel, BoxLayout.Y_AXIS));
        envEditPanel.setLayout(new BorderLayout());
        envEditPanel.add(BorderLayout.NORTH, envEditButtonPanel);
        canvas = new PiecewiseCanvas();
        //canvasPanel.add(canvas);
	//canvasPanel.setBorder(BorderFactory.createEtchedBorder());
        envEditPanel.add(BorderLayout.CENTER, canvas);
		
        //insert panels into main frame and display
        //mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
	//mainPanel.setBorder(BorderFactory.createEmptyBorder());
        //canvasPanel.add(canvas);		
	mainPanel.add(BorderLayout.NORTH, topPanel);
	mainPanel.add(BorderLayout.CENTER, envEditPanel);
			
	pack();
		
        //resize and center the window
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

        setLocation(100, 100);
        setSize(650, 580);
        // setBackground(Color.white);
        setResizable(true);
        setVisible(true);
        setClosable(true);
        setMaximizable(true);
        setIconifiable(true);
        System.out.println("EnvelopeFrame constructor 2 after setIcnifiable");
        // synchronize env data by loading data from Nyquist process
        loadEnvelopes();
        repaint();
        // canvas.repaint();
    }

    public void envNameSelected() {
        if (saving) return; // ignore selection generated by "save" button
        // If the name is different from the current envelope name, do
        // a "save". Then switch to editing the newly selected envelope.
        String name = (String) envName.getSelectedItem();
        // when we load the JComboBox with new names, the contentsChanged
        // method of JComboBox invokes a selection action, even if nothing
        // is selected. I don't know why, but we have to handle the null
        // selection case.
        if (name == null) return;
        
        String originalName = currentEnvName;
        currentEnvName = currEnvName.getText().trim();
        if (!originalName.equals(currentEnvName)) {
            modified = true;
        }
        if (modified) {
            Object[] options = { "OK", "CANCEL" };
            int i = JOptionPane.showOptionDialog(mainPanel, 
                    currentEnvName + " is being edited. Save it?", 
                    "Warning",
                    JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE,
                    null, options, options[0]);
            if (i == JOptionPane.OK_OPTION) {
                saveEnvelopes();
            }
        }
        
        // store envelope under old name
        String edited = canvas.getExpression();
        System.out.println("expression existed, putting " + currentEnvName +
                            " " + edited + ", changing currentEnvName to " + name);
        if (currentEnvName.length() > 0) envColl.put(currentEnvName, edited);
        // install name as new envelope to edit
        String expression = envColl.get(name);
        canvas.setEnv(expression);
        currEnvName.setText(name);
        // make name be the selected name
        envName.setSelectedItem(name);
       // }
        currentEnvName = name;
    }
    
    
    //public double getMinT() { return Double.parseDouble(minT.getText().trim()); }
    public double getMaxT() { return Double.parseDouble(maxT.getText().trim()); }
    public double getMinA() { return Double.parseDouble(minA.getText().trim()); }
    public double getMaxA() { return Double.parseDouble(maxA.getText().trim()); }
    public int getEnvType() {
        if (envTypes != null) {
            String env = (String) envTypes.getSelectedItem();
            if (env.matches(types[PWL_TYPE])) return PWL_TYPE;
            return PWE_TYPE;
        } else /* initializing */ return PWL_TYPE;
    }
    
    public boolean within(double x, double y, double eps) {
        return Math.abs(x - y) < eps;
    }

    // write current envelope definition to Nyquist
    public void saveEnvelopes() {
        // make sure current envelope has been stored in collection
        if (currEnvName.getText().length() == 0) {
            JOptionPane.showMessageDialog(mainPanel, "Please Enter an Envelope Name");
            return;
        }
        currentEnvName = currEnvName.getText().trim();
        
        // now write all to Nyquist
        saving = true;
        boolean foundIt = false;
        for (Enumeration keys = envColl.keys(); keys.hasMoreElements(); ) {
            String name = (String) keys.nextElement();
            // update envelope collection with current envelope
            if (name.equals(currentEnvName)) {
                envColl.remove(name);
                //envName.removeItem(name);
                envColl.put(currentEnvName, canvas.getExpression());
                //envName.addItem(currentEnvName);
                foundIt = true;
            }
            String expression = envColl.get(name);
            String defn = "(define-env '" + name + " '" + expression + ")";
            System.out.print("in saveEnvelopes: " + defn);
            myParent.sendInputLn(defn); // send to Nyquist for evaluation
        }
        // if the current envelope was not in the list, add it and save it
        if (!foundIt) {
            String expr = canvas.getExpression();
            envColl.put(currentEnvName, expr);
            envName.addItem(currentEnvName);
            String defn = "(define-env '" + currentEnvName + " '" + expr + ")";
            System.out.print("in saveEnvelopes: " + defn);
            myParent.sendInputLn(defn); // send to Nyquist for evaluation
        }
        envName.setSelectedItem(currentEnvName);
        modified = false;
        saving = false;
        System.out.println("modified false in saveEnvelopes\n");
    }
    
    
    public void loadEnvelopes() {
        myParent.callFunction("get-env-data", "");
    }
    
    public void deleteEnvelope() {
        Object[] options = { "OK", "CANCEL" };
        int i = JOptionPane.showOptionDialog(mainPanel, 
                    "Deletion cannot be undone, click OK to continue", 
                    "Warning",
                    JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE,
                    null, options, options[0]);
        if (i != JOptionPane.OK_OPTION) return;
        // it appears as though currentEnvName gets changed when you remove
        // it from the envName comboBox, so make a local copy. Previously,
        // we were calling DELETE-ENV after the name changed!
        String name = currentEnvName;
        // delete the envelope from hashtable
        envColl.remove(name);
        // delete the envelope from the combobox
        envName.removeItem(name);
        // delete the envelope from the workspace
        myParent.sendInputLn(
                "(delete-env '" + name + ")");
    }
    
    
    public void actionPerformed(ActionEvent e)
    {
        //System.out.println(e.getActionCommand());
        String actionCommand = e.getActionCommand();
	
        // File Menu options
        if (actionCommand.equals("saveEnvelopes")) {
        	saveEnvelopes();
        } else if (actionCommand.equals("loadEnvelope")) {
            loadEnvelopes();
        } else if (actionCommand.equals("deleteEnvelope")) {
            deleteEnvelope();
        } else if (actionCommand.equals("envNameSelected")) {
            envNameSelected();
        } else if (actionCommand.equals("envTypeChanged")) {
            int type = getEnvType();
            if (type != canvas.type) {
                canvas.type = type;
                canvas.history.save(canvas);
                canvas.repaint();
            }

            //set initial amplitude and time parameters
            if (canvas.times.size() < 1) {
            	if (type == PWE_TYPE) {
                    minA.setText("1.0");
                    maxA.setText("2.0");
            	} else {
                    minA.setText("0.0");
                    maxA.setText("1.0");
            	}
            }            
            canvas.repaint();
            validate();
            setVisible(true);
        } else if (actionCommand.equals("deletePoint")) {
            int index = envTimes.getSelectedIndex();
            System.out.println("at deletePoint, index " + index);
            if (index >= 0) {
                canvas.selection = index;
                System.out.println("at deletePoint before deleteSelection");
                canvas.deleteSelection();
                index = envTimes.getSelectedIndex();
                System.out.println("deletePoint new index " + index);
                if (index >= 0) {
                    canvas.selection = index;
                    envAmplitudes.setText(form.format(canvas.amps.get(index)));
                }
                canvas.repaint();
            }
        } else if (actionCommand.equals("addPoint")) {
            String text = (String) envTimes.getSelectedItem();
            if (text.equals("")) return;
            double time = Double.parseDouble(text.trim());
            text = envAmplitudes.getText();
            double amp = Double.parseDouble(text.trim());
            canvas.insertInOrder(time, amp);
            canvas.repaint();
        } else if (actionCommand.equals("updatePoint")) {
            String text = (String) envTimes.getSelectedItem();
            if (text.equals("")) return;
            double time = Double.parseDouble(text.trim());
            text = envAmplitudes.getText();
            double amp = Double.parseDouble(text.trim());
            System.out.println("updatePoint selection " + canvas.selection);
            canvas.deleteSelection();
            canvas.insertInOrder(time, amp);
            canvas.repaint();
        } else if (actionCommand.equals("envTimeChange")) {
            // sometimes this action gets called in the middle of
            // doing an update and in an inconsistent state. If this
            // happens, don't try to set amplitude text.
            if (canvas.amps.size() != envTimes.getItemCount()) return;
            int index = envTimes.getSelectedIndex();
            System.out.println("envTimeChange " + index);
            if (index >= 0) {
                canvas.selection = index;
                envAmplitudes.setText(form.format(canvas.amps.get(index)));
                canvas.repaint(); // update selection marker
            }
        } else {
            System.out.println("ACTION NOT HANDLED: " + actionCommand);
        }
    }
    
    public void loadEnvData(String data) {
        data = data.toLowerCase();
        System.out.println("loadEnvData: data |" + data + "| len " + data.length());
        envName.removeAllItems(); // clear and reload combo box
        envTimes.removeAllItems(); // clear times 
        envAmplitudes.setText(""); // clear amplitude box
        while (data.length() > 0) {
            int eolx = data.indexOf("\n");
            if (eolx < 0) return; // shouldn't happen, but bail if it does
            String line = data.substring(0, eolx);
            System.out.println("loadEnvData: line " + line);
            data = data.substring(eolx + 1);
            String name = line.substring(0, line.indexOf(' '));
            System.out.println("loadEnvData: name " + name);
            String env = line.substring(name.length() + 1);
            System.out.println("loadEnvData: env " + env);
            if (name.length() > 0) envColl.put(name, env);
            envName.addItem(name);
        }
    }
    
    private class State {
        public int type;
        public double maxT;
        public double minA;
        public double maxA;
        
        public State(int t, double stop, double low, double hi) {
            type = t; maxT = stop; minA = low; maxA = hi;
        }
    }
    
    private class History {
    /* consider a sequence of undo/redo to be a single edit operation -- thus
     * the end of the versions list is set to the latest undo/redo selection
     */
        private boolean undoRedo = false;
        private Vector<Vector<Double>> t_history = new Vector<Vector<Double>>();
        private Vector<Vector<Double>> a_history = new Vector<Vector<Double>>();
        private Vector<State> state_history = new Vector<State>();
        private int version = -1;
    
        public void save(PiecewiseCanvas canvas) {
            t_history.add((Vector<Double>) (canvas.times.clone()));
            a_history.add((Vector<Double>) (canvas.amps.clone()));
            state_history.add(new State(canvas.type, getMaxT(), 
                                        getMinA(), getMaxA()));
            version = t_history.size() - 1;
            System.out.println("Saved version " + version);
            undoRedo = false; /* clear flag for next undoRedo sequence */
        }

        public boolean canGet() { 
            boolean result = version >= 0 && version < t_history.size();
            System.out.println("canGet returns " + result + " version " +
                version);
            return result;
        }
        
        public Vector<Double> getTimes() { return t_history.get(version); }
        public Vector<Double> getAmps() { return a_history.get(version); }
        public State getState() { return state_history.get(version); }
        
        private void processUndoRedo() {
            if (!undoRedo) { /* extend with copy of the version */
                t_history.add((Vector<Double>) (t_history.get(version).clone()));
                a_history.add((Vector<Double>) (a_history.get(version).clone()));
                state_history.add(state_history.get(version));
            } else { /* replace with different version */
                t_history.set(t_history.size() - 1, 
                              (Vector<Double>) (t_history.get(version).clone()));
                a_history.set(t_history.size() - 1, 
                              (Vector<Double>) (a_history.get(version).clone()));
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
            if (version < t_history.size() - 1) {
                version++; 
                processUndoRedo();
            }
        }
    }
    
    
    // Class for the drawing area
    //private class PiecewiseCanvas extends Canvas implements MouseListener, 
    private class PiecewiseCanvas extends JPanel implements MouseListener, 
            MouseMotionListener, KeyListener {
	private int currentAmp;
	private int currentTime;
	public BufferedImage image = null;
	public boolean selectCheck = false;
        public int selection;
        public History history;

        private boolean mouseDown = false; // used to detect version for undo
        private boolean changed = false; // used to detect version for undo
	
	//  Vectors to store the absolute parameters of the points in the 
        // envelope.
	public Vector<Double> times = new Vector<Double>();
	public Vector<Double> amps = new Vector<Double>();
        public int type = PWL_TYPE; /* PWL_TYPE or PWE_TYPE */
	
	// Constructor
	public PiecewiseCanvas() {
	    setBackground(Color.white);
	    addMouseListener(this);
	    addMouseMotionListener(this);
	    addKeyListener(this);
            selection = -1;
            history = new History();
            history.save(this);
	}
        
        public boolean isValueType() {
            if (times.size() == 0) return false;
            return (times.get(0) == 0 || 
                    within(times.lastElement(), getMaxT(), EPSILON));
	}
        
        public boolean isImpliedFirstPoint() {
            return (times.size() == 0) || 
                   !within(times.get(0), 0, EPSILON);
        }
        
        public boolean isImpliedLastPoint() {
            return (times.size() == 0) || 
                   !within(times.lastElement(), getMaxT(), EPSILON);
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
            return (type == PWL_TYPE ? 0.0 : 1.0);
        }


        /*
        public Dimension getMaximumSize() {
	    return getPreferredSize();
	}*/
        
        // draw the graphics inside the full canvas, so use these
        // functions to get the size and coordinates of the drawing
        // area that is usable
        private int graphLeft() { return LEFT_BORDER; }
        private int graphRight() { return getWidth() - RIGHT_BORDER; }
        private int graphWidth() { return getWidth() - 
                                          (LEFT_BORDER + RIGHT_BORDER); }
        private int graphTop() { return TOP_BORDER; }
        private int graphBottom() { return getHeight() - BOTTOM_BORDER; }
        private int graphHeight() { return getHeight() - 
                                           (TOP_BORDER + BOTTOM_BORDER); }
        private int clipX(int x) {
            return Math.max(LEFT_BORDER, Math.min(graphRight(), x)); }
        private int clipY(int y) {
            return Math.max(BOTTOM_BORDER, Math.min(graphBottom(), y)); }
	
        public String getExpression() {
            boolean valueType = isValueType();
            String env;
        
            if (type == PWL_TYPE) env = (valueType ? "pwlv" : "pwl");
            else env = (valueType ? "pwev" : "pwe");

            return outputEnv(env, valueType, 0.0, // getMinT(), 
                             getMaxT(), getMinA(), getMaxA());
        }
    
    
	//draw the canvas image
	public void paint(Graphics g) {
	    super.paint(g);
            // test: g.drawLine(0, 0, 100, 100); 
	    
	    Graphics2D drawArea = (Graphics2D) g;
            canvas.drawCircles(drawArea);
            canvas.drawSelectionCircle(drawArea);
            canvas.connectTheDots(drawArea);
            drawArea.dispose();
        }

	//erase the canvas, then draw all of the points in the envelope
	private void drawCircles(Graphics2D g)	{
	    //erase the previous image
	    //clearCanvas(g);
	    double maxTime = getMaxT();

            // is the initial point implicit?
            if (isImpliedFirstPoint()) { // implied
                double amp = impliedAmp();
                int y = amp2y(amp);
                // System.out.println("implied: amp " + amp + " y " + y);
                drawDot(g, graphLeft(), y);
	    	if (displayCoord) {
                    g.drawString("(0," + form.format(amp) + ")", 
                                 graphLeft(), y - 3);
                }
            }

            // is the final point implicit?
            if (isImpliedLastPoint()) {
                double amp = impliedAmp();
                int y =  amp2y(amp);
                drawDot(g, graphRight(), y);
                if (displayCoord) {
                    g.drawString("(" + form.format(getMaxT()) + "," +
                                 form.format(amp) + ")", graphWidth() - 36,
                                 y - 9);
                }
            }

	    //draw points for each point in the envelope
	    for (int i = 0; i < times.size(); i++) {
	    	int t = time2x(times.get(i));
	    	int a = amp2y(amps.get(i));
	    	if (displayCoord) 
                    g.drawString("(" + form.format(times.get(i)) + "," + 
                                 form.format(amps.get(i)) + ")", t, a);
	    	drawDot(g, t, a);
                //System.out.println("drawDot t " + t + " a " + a + " width " +
                //                getWidth());
	    }
	}
	
	//given coordinates, draw a circle on the canvas
	private void drawDot(Graphics2D g, int t, int a) {
	    //draw a black circle at the specified point 
		g.setColor(Color.black);
            //System.out.println("drawDot: " + t + "," + a);
	    g.fillOval(t - 2, a - 2, 5, 5);
	}
	
	// given coordinates, draw a circle around selected envelope point
	private void drawSelectionCircle(Graphics2D g) {
            if (selection >= 0 && selection < times.size()) {
                int t = time2x(times.get(selection));
                int a = amp2y(amps.get(selection));
                //draw a red circle around the specified point
                g.setColor(Color.red);
                g.drawOval(t - 4, a - 4, 9, 9);
            }
	}
        
        private void draw_connect(Graphics2D g, double t1, double a1, 
                                                double t2, double a2) {
            int x1 = time2x(t1);
            int x2 = time2x(t2);
            int y1 = amp2y(a1);
            int y2 = amp2y(a2);
            if (type == PWL_TYPE) {
                g.drawLine(x1, y1, x2, y2);
            } else {
                // pwe type, graph is linear along a log scale
                if (a1 <= EPSILON || a2 <= EPSILON) {
                    g.drawLine(x1, y1, x1, graphBottom());
                    g.drawLine(x1, graphBottom(), x2, graphBottom());
                    g.drawLine(x2, graphBottom(), x2, y2);
                } else {
                    double log1 = Math.log(a1);
                    double log2 = Math.log(a2);
                    int startline = y1;
                    double logIncr = (log2 - log1) / (x2 - x1);
                    for (int j = x1 + 1; j <= x2; j++) {
                        double loga = log1 + logIncr * (j - x1);
                        int a = amp2y(Math.exp(loga));
                        g.drawLine(j - 1, startline, j, a);
                        startline = a;
                    }
                }
            }
        }
	
	//connect adjacent points in the envelope by lines (pwl, pwlv) or by an
	// exponential curve (pwe, pwev)
	private void connectTheDots(Graphics2D g) {
	    g.setColor(Color.blue);
	    // System.out.println("connectTheDots\n");
            if (times.size() > 0) {
                if (isImpliedFirstPoint()) {
                    draw_connect(g, 0, impliedAmp(), 
                                 times.get(0), amps.get(0));
                }
                if (isImpliedLastPoint()) {
                    draw_connect(g, times.lastElement(), 
                                 amps.lastElement(),
                                 getMaxT(), impliedAmp());
                }
                //connect the non-endpoints in the envelope
                double t1 = times.get(0);
                double a1 = amps.get(0);
	        for (int i = 0; i < times.size() - 1; i++) {
	    	    double t2 = times.get(i + 1);
                    double a2 = amps.get(i + 1);
                    draw_connect(g, t1, a1, t2, a2);
                    t1 = t2;
                    a1 = a2;
	        }
            } else { // size == 0, so both points are implied
                draw_connect(g, 0, impliedAmp(), getMaxT(), impliedAmp());
            }

        }
	
	// erase the canvas and clear the parameter vectors - 
        // completely reset the envelope
	public void clear() {
	    if (times != null)
		times.removeAllElements();
	    if (amps != null)
		amps.removeAllElements();
            envTimes.removeAllItems();
            modified = true;
            repaint();
            history.save(this);
	}
        
        public void restore() {
            if (history.canGet()) {
                times = (Vector<Double>) (history.getTimes().clone());
                amps = (Vector<Double>) (history.getAmps().clone());
                State state = history.getState();
                type = state.type;
                maxT.setText(String.valueOf(state.maxT));
                minA.setText(String.valueOf(state.minA));
                maxA.setText(String.valueOf(state.maxA));
                envTypes.setSelectedItem(types[type]);
                selection = -1;
                // put times in combo box
                envTimes.removeAllItems();
                for (int i = 0; i < times.size(); i++) {
                    envTimes.insertItemAt(form.format(times.get(i)), i);
                }
                envAmplitudes.setText("");
                repaint();
            }
        }
        
	//set time and amplitude on click by inserting the point into the vectors.
	// if delete is checked, try to delete a point from the envelope
	public void mousePressed(MouseEvent e) {
	    mouseDown = true;
            System.out.println("mouseDown true\n");
            this.requestFocus();
	    currentTime = e.getX();
	    currentAmp = e.getY();
	    selectCheck = checkSelection(currentTime, currentAmp);		
	    if (selectCheck) {
	    	return;
	    }
            insertInOrder(x2time(currentTime), y2amp(currentAmp));
	    repaint();
	}
	
	public void mouseDragged(MouseEvent e) {
            currentTime = clipX(e.getX());
	    currentAmp = clipY(e.getY());
            if (currentTime <= graphRight() && currentTime >= graphLeft() && 
                currentAmp >= graphTop() && currentAmp <= graphBottom()) {
                deleteSelection();
                insertInOrder(x2time(currentTime), y2amp(currentAmp));
                repaint();
            }
	}
        
        public void mouseReleased(MouseEvent e) {
            System.out.println("mouseReleased\n");
            if (changed) {
                history.save(this);
                changed = false;
            }
            mouseDown = false;
            System.out.println("mouseDown false\n");
        }

        // convert time coordinate to time in seconds
        private double x2time(int x) {
            return (x - graphLeft()) * getMaxT() / graphWidth();
        }
        
        private int time2x(double time) {
            int x = (int) Math.round(time * graphWidth() / getMaxT()) + 
                    graphLeft();
            return x;
        }        
        
        // convert amp coordinate to real amplitude
        private double y2amp(int y) {
	    double maxAmp = getMaxA();
            double aRange = maxAmp - getMinA();
	    double amp = maxAmp - ((y - graphTop()) * aRange / graphHeight());
            return amp;
        }
        
        private int amp2y(double amp) {
	    double maxAmp = getMaxA();
	    double aRange = maxAmp - getMinA();
            int y = (int) Math.round((maxAmp - amp) * graphHeight() / aRange) +
                    graphTop();
            // System.out.println("amp2y: amp " + amp + " y " + y);
            return y;
        }
        
        private void deleteSelection() {
            if (selection < 0 || selection >= times.size()) return;
            times.remove(selection);
            amps.remove(selection);
            modified = true;
            System.out.println("deleteSelection at " + selection);
            envTimes.removeItemAt(selection);
            // make the Amp: box correspond to the new selection:
            String amp = "";
            if (times.size() > 0) {
                int index = envTimes.getSelectedIndex();
                if (index >= 0) amp = form.format(amps.get(index));
            }
            envAmplitudes.setText(amp);
            selection = -1;
            if (!mouseDown) history.save(this);
        }
        
        private void addPoint(int i, double absT, double absA) {
            System.out.println("addPoint: " + i + " " + absT + " " + absA);
            times.add(i, absT);
            amps.add(i, absA);
            envTimes.insertItemAt(form.format(absT), i);
            System.out.println("addPoint time: " + absT + ", text " + form.format(absT));
            envTimes.setSelectedIndex(i);
            envAmplitudes.setText(form.format(amps.get(i)));
            selection = i;
            changed = true;
            if (!mouseDown) history.save(this);
        }

	//insert the time and amplitude in the vectors in time sorted order
	private void insertInOrder(double time, double amp) {
	    int i = 0;
	    modified = true;
	    if (times != null)	{
		while (i < times.size() && time > times.get(i)) i++;
	    }
            addPoint(i, time, amp);
            // System.out.println("absT: " + absT + " absA: " + absA + " i: " + i);
            return;
	}
	        
        // Check if mouse click corresponds to existing envelope point
        //    return index of point or -1 if no point is close
        private int getSelection(int x, int y) {
            int cutoff = 7;
            int bestDist = cutoff * 2;
            int bestIndex = -1;
            if (times == null) return bestIndex;
            for (int i = 0; i < times.size(); i++) {
                int xi = time2x(times.get(i));
                int yi = amp2y(amps.get(i));
                int dx = Math.abs(x - xi);
                int dy = Math.abs(y - yi);
                if (dx < cutoff && dy < cutoff && dx + dy < bestDist) {
                    bestDist = dx + dy;
                    bestIndex = i;
                }
            }
            selection = bestIndex;
            return bestIndex;
        }
        
	//Check if mouse click corresponds with existing envelope point (to select point)
	private boolean checkSelection(int time, int amp) {
            int i = getSelection(time, amp);
            if (i < 0) return false;
            envTimes.setSelectedIndex(i);
            envAmplitudes.setText(form.format(amps.get(i)));
            repaint();
            return true;
        }

	//output the envelope as a string
	public String outputEnv(String envType, boolean valueType,
				double minTime, double maxTime, double minAmp, 
				double maxAmp) {
	    String outputStr = new String();
            int start = 0;
	    
	    outputStr += ("(" + envType + " ");
            
	    if (valueType) { // insert initial value
                if (within(times.get(0), 0.0, EPSILON)) {
                    outputStr += (form.format(amps.get(0)) + " ");
                    start = 1;
                } else outputStr += form.format(impliedAmp()) + " ";
            }    
	    
	    for (int i = start; i < amps.size(); i++) {
		double time = times.get(i);
		double amp  = amps.get(i);		
		outputStr += form.format(time) + " " + form.format(amp) + " ";
	    }
            
            if (valueType) { // we're already ending with a value
                if (within(times.lastElement(), maxTime, EPSILON)) {
                    // we're done because we output at maxTime
                } else {
                    outputStr += form.format(maxTime) + " " + 
                                 form.format(impliedAmp());
                }
            } else {
                outputStr += form.format(maxTime);                
	    }
            outputStr += ")";	    
	    
	    return outputStr;
	}
	
        // parse envelope from string and prepare to edit
	public void setEnv(String envData) {
		
	    System.out.println("setEnv: envData " + envData);
            if (envData == null) return; // just in case
            //check if envelope exists in collection
            boolean nameIsEnv = false;

	    // trim the open and close parens from envData
	    int startx = envData.indexOf("(") + 1;
	    // if no open paren, startx will be 0
 	    int endx = envData.indexOf(")");
	    if (endx < 0) endx = envData.length();
	    envData = envData.substring(startx, endx);

            System.out.println("setEnv: envData(2) " + envData);
            StringTokenizer st = new StringTokenizer(envData);
            String type = st.nextToken();
            System.out.println("setEnv: type " + type);
            envTypes.setSelectedItem(type);
            //clear();
	    
            valueType = type.endsWith("v");
            int limit = (valueType ? 2 : 1);
            times.removeAllElements();
	    amps.removeAllElements();
            int i = 0;
            // pretend mouse is down to avoid making each point undo-able
            boolean save = mouseDown;
            mouseDown = false;
            double time, amp;
            if (valueType) { // first element is value
                amp = new Double(st.nextToken().trim());
                addPoint(i++, 0.0, amp);
            }
            while (st.countTokens() >= 2) {
                String token1 = st.nextToken().trim();
                time = new Double(token1);
                String token2 = st.nextToken().trim();
                amp = new Double(token2);
                addPoint(i++, time, amp);
                System.out.println("time " + token1 + " amp " + token2 + 
                                   " size " + times.size());
            }
            mouseDown = save; // restore the mouseDown state
            if (!valueType) { // last element is time
                maxT.setText(st.nextToken());
            }
	    System.out.println("times " + times + " amps " + amps);
            //calculateDraws(true);
            modified = false;
            repaint();
        }
	
	public void keyPressed(KeyEvent event) {
            //Graphics2D drawArea = image.createGraphics();
		
            if (event.getKeyCode() == KeyEvent.VK_DELETE) {			
                deleteSelection();
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
