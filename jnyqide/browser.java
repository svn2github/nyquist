package jnyqide;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import javax.swing.ListModel;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;
import javax.swing.SpringLayout;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;


/***
** Class: Browser
** Author: Priyanka Raghavan and Roger B. Dannenberg
** Description: This is the frame class that implements the sound browser. 
**/

class Browser extends JInternalFrame {
    
    NyquistThread nyquistThread;
    MainFrame mainFrame;
    ArrayList instruments;
    InstrumentCharacteristics currentInstr;

    /**
     * @ desc Default constructor, takes the nyquist thread from main frame
     *  @author prirags - removed all references to the log factory
     */
    
    public Browser(MainFrame mainFrame, NyquistThread nyquistThread) {
        this.mainFrame = mainFrame;
	this.nyquistThread=nyquistThread;
	instruments = new ArrayList();
	currentInstr = null;
        try {
	    System.out.println("in Browser()");
	    initGUI();
	    setLocation(50+10, 50+10);
	    setSize(600, 500);
	    setResizable(true);
	    setVisible(true);
	    setClosable(true);
	    setMaximizable(true);
	    setIconifiable(true);
	    // setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
	    System.out.println("at end of Browser()");
        }
        catch (Exception exception) {
            exception.printStackTrace();
        }
    }
    
    /**
     * @ desc intializes gui elements
     *  @author prirags - removed all references to the log factory
     */
    
    private void initGUI() {
	setTitle("Sound Browser");
	this.setSize(800, 513);
	jPanel1 = new JPanel(new SpringLayout());
	Border etchedLine;
	etchedLine = BorderFactory.createEtchedBorder(EtchedBorder.RAISED);
	this.getContentPane().add(jPanel1, BorderLayout.NORTH);
	jPanel1.setBorder(etchedLine);
	// jPanel1.setPreferredSize(new java.awt.Dimension(638, 101));
	categoryLabel = new JLabel();
	jPanel1.add(categoryLabel);
	categoryLabel.setText("Sound Category:");
	ComboBoxModel categoryComboModel = new DefaultComboBoxModel();
	categoryCombo = new JComboBox();
	categoryCombo.addItemListener(
	    new ItemListener() {	
		public void itemStateChanged(ItemEvent evt) {
		    if (evt.getStateChange() != evt.SELECTED) return;
		    if (subcategoryCombo == null) return; // just in case
		    // set up subcategory
		    subcategoryCombo.removeAllItems();
		    subcategoryCombo.addItem("Select One");
		    String category = (String) evt.getItem();
		    System.out.println("category is " + category);
		    for (int i = 0; i < instruments.size(); i++) {
			if (((InstrumentCharacteristics) instruments.get(i)).
			    getCategoryName().equalsIgnoreCase(category)) {
			    subcategoryCombo.addItem(
				((InstrumentCharacteristics) instruments.
				     get(i)).getSubcategoryName());
			}
		    }
		}
	    });
	jPanel1.add(categoryCombo);
	categoryCombo.setModel(categoryComboModel);
	// categoryCombo.setPreferredSize(new java.awt.Dimension(306, 25));
	subcategoryLabel = new JLabel();
	jPanel1.add(subcategoryLabel);
	subcategoryLabel.setText("Sound Name:");
	ComboBoxModel subcategoryComboModel = new DefaultComboBoxModel();
	subcategoryCombo = new JComboBox();
	subcategoryCombo.addItemListener(
	    new ItemListener() {	
		public void itemStateChanged(ItemEvent evt) {
		    for (int i = 0; i < instruments.size(); i++) {
			InstrumentCharacteristics ic =
			    (InstrumentCharacteristics) instruments.get(i);
			String name = (String) evt.getItem();
			if (ic.getSubcategoryName().equalsIgnoreCase(name)) {
			    currentInstr = ic;
			    visitSound();
			}
		    }
		}
	    });
	jPanel1.add(subcategoryCombo);
	subcategoryCombo.setModel(subcategoryComboModel);
	// subcategoryCombo.setPreferredSize(new java.awt.Dimension(304, 28));
	SpringUtilities.makeCompactGrid(jPanel1, 2, 2, 6, 6, 6, 6);


	jPanel3 = new JPanel(new SpringLayout());
	this.getContentPane().add(jPanel3, BorderLayout.CENTER);
	etchedLine = BorderFactory.createEtchedBorder(EtchedBorder.RAISED);
	jPanel3.setBorder(etchedLine);
	// jPanel3.setPreferredSize(new java.awt.Dimension(638, 148));
	
	BorderLayout bl = new BorderLayout();
	bl.setHgap(6);
	bl.setVgap(6);
	jPanel2 = new JPanel(bl);
	this.getContentPane().add(jPanel2, BorderLayout.SOUTH);
	etchedLine = BorderFactory.createEtchedBorder(EtchedBorder.RAISED);
	jPanel2.setBorder(etchedLine);
	// jPanel2.setPreferredSize(new java.awt.Dimension(791, 159));

	playButton = new JButton();
	playButton.setText("PLAY");
	// playButton.setPreferredSize(new java.awt.Dimension(100, 36));
	// playButton.setBounds(4, -5, 100, 36);
	playButton.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    playSound();
		}
		
	    });

	// try this: 
	// (west: (north: play button))
	//         center: (north: 
	//                  (center: "Lisp Expression that produced the sound")
	//                  center: scrolling text))
	bl = new BorderLayout();
	bl.setHgap(6);
	bl.setVgap(6);
	JPanel jPanel2a = new JPanel(bl);
	jPanel2.add(jPanel2a, BorderLayout.WEST);
	jPanel2a.add(playButton, BorderLayout.NORTH);
	bl = new BorderLayout();
	bl.setHgap(6);
	bl.setVgap(6);
	JPanel jPanel2b = new JPanel(bl);
	jPanel2.add(jPanel2b, BorderLayout.CENTER);
	bl = new BorderLayout();
	bl.setHgap(6);
	bl.setVgap(6);
	JPanel jPanel2b1 = new JPanel(bl);
	jPanel2b.add(jPanel2b1, BorderLayout.NORTH);
	JLabel label = new JLabel("Lisp Expression that produced the sound:");
	label.setHorizontalAlignment(label.CENTER);
	jPanel2b1.add(label, BorderLayout.CENTER);
	jScrollPane1 = new JScrollPane();
	expArea = new JTextArea();
	jScrollPane1.setViewportView(expArea);
	
	// expArea.setPreferredSize(new java.awt.Dimension(650, 60));
	
	jPanel2b.add(jScrollPane1, BorderLayout.CENTER);
	jScrollPane1.setPreferredSize(new java.awt.Dimension(700, 62));
	// SpringUtilities.makeCompactGrid(jPanel2, 2, 2, 6, 6, 6, 6);

	categoryCombo.addItem("Select One");

	// read the instrument.txt text file from lib
	// and create the InstrumentCharacteristic objects.
	//
	BufferedReader br;
	try {
	    br = new BufferedReader(
		       new FileReader(nyquistThread.soundBrowser));
	} catch (Exception e) {
	    System.out.println("ERROR -- could not open " + 
			       nyquistThread.soundBrowser);
	    return;
	}
	while (true) {
	    InstrumentCharacteristics ic = 
		new InstrumentCharacteristics();
	    if (!ic.readData(br)) break;
	    System.out.println("new IC: " + ic.getCategoryName() + ":" +
			       ic.getSubcategoryName());
	    instruments.add(ic);
	    boolean foundIt = false;
	    for (int k = 0; k < categoryCombo.getItemCount(); k++) {
		if (categoryCombo.getItemAt(k).toString().
		        equalsIgnoreCase(ic.getCategoryName())) {
		    foundIt = true;
		    break;
		}
	    } //end of for
	    if (!foundIt) {
		System.out.println("Added " + ic.getCategoryName());
		categoryCombo.addItem(ic.getCategoryName());
	    }					
	}
    }

    private JPanel jPanel1;
    private JComboBox subcategoryCombo;
    private JLabel subcategoryLabel;
    private JLabel categoryLabel;
    private JComboBox categoryCombo;
    
    private JScrollPane jScrollPane1;
    
    private JPanel jPanel3;
    private JTextArea expArea;
    private JButton playButton;
    
    private JPanel jPanel2;
    private JLabel[] paramLabels = null;
    private JSlider[] paramSliders = null;
    
    public void newParameterCount(int n) {
	// make array for new labels and sliders
	JLabel[] newParamLabels = paramLabels;
	JSlider[] newParamSliders = paramSliders;
	int oldN = 0;
	if (paramLabels != null) {
	    oldN = paramLabels.length;
	}
	if (n > oldN) {
	    newParamLabels = new JLabel[n];
	    newParamSliders = new JSlider[n];
	}
	// make old labels and sliders invisible before reuse
	if (paramLabels != null) {
	    // paramLabel0.setVisible(false);
	    for (int i = 0; i < oldN; i++) {
		paramLabels[i].setVisible(false);
		newParamLabels[i] = paramLabels[i];

		paramSliders[i].setVisible(false);
		newParamSliders[i] = paramSliders[i];
	    }
	} else {
	    // paramLabel0 = new JLabel();
	    // paramLabel0.setText("SLIDERS");
	    // jPanel3.add(paramLabel0);
	}
	// if we don't have enough, make more
	if (n > oldN) {
	    for (int i = oldN; i < n; i++) {
		newParamLabels[i] = new JLabel();
		jPanel3.add(newParamLabels[i]);
		newParamSliders[i] = new JSlider();
		jPanel3.add(newParamSliders[i]);
	    }
	    paramLabels = newParamLabels;
	    paramSliders = newParamSliders;
	}
	SpringUtilities.makeCompactGrid(jPanel3, n, 2, 6, 6, 6, 6);

	// now we have n labels and sliders in arrays
	// we also have paramLabel0
    }


    public void visitSound() {
	System.out.println("visitSound called: " + 
			   currentInstr.getSubcategoryName());
	/*
	 * This hides the labels and the sliders that could have 
	 * been created in previous runs
	 */
	int numParams = currentInstr.getParameters().size();
	newParameterCount(numParams);
	System.out.println("visitSound: numParams " + numParams);

	for(int i = 0; i < numParams; i++) {
	    Parameter p = (Parameter) (currentInstr.getParameters().get(i));
	    paramLabels[i].setText(p.getName().toUpperCase());
	    System.out.println("Parameter" + p.getName());
	    paramLabels[i].setVisible(true);
	    if (p.getType().equals("int")) {
		final JSlider s = paramSliders[i];
		int max = Integer.parseInt(p.getMaxValue());
		s.setMaximum(max);
		int min = Integer.parseInt(p.getMinValue());
		s.setMinimum(min);
		int range = max - min;
		int major = 1;
		while (major < range) major *= 10;
		s.setMajorTickSpacing(major / 10);
		s.setMinorTickSpacing(major / 100);
		System.out.println("major " + major);
		s.setPaintTicks(true);
		s.setPaintLabels(true);
		s.setValue(Integer.parseInt(p.getDefaultValue()));
	    } else if (p.getType().equals("float")) {
		final float min = Float.valueOf(p.getMinValue()).floatValue();
	        final float max = Float.valueOf(p.getMaxValue()).floatValue();
		final float width = max - min;
		final Parameter param = p;
		final JSlider s = paramSliders[i];
		float fval = Float.valueOf(p.getDefaultValue()).floatValue();
		param.setValue(fval);
		s.setMinimum(0);
		s.setMaximum(1000);
		s.setValue((int) ((fval - min) / width * 1000));
		java.util.Hashtable labels = new java.util.Hashtable(3);
		labels.put(new Integer(0), new JLabel(p.getMinValue()));
		labels.put(new Integer(500), 
			   new JLabel(new Float((max + min) / 2.0f).
				              toString()));
		labels.put(new Integer(1000), new JLabel(p.getMaxValue()));
		s.setLabelTable(labels);
		s.setPaintTicks(false);
		s.setPaintLabels(true);
		// s.setBorder(new TitledBorder("border text"));
		s.addChangeListener(new ChangeListener() {
		    public void stateChanged(ChangeEvent e) {
                        int j = s.getValue();
			float f = min + (j * width / 1000.0f);
			param.setValue(f);
		    }
		    });
 	    } else {
		System.out.println("Expected int or float in " + 
				   i + " |" + p.getType() + "|");
	    }
						      
	    if (numParams > 1) {
		paramSliders[i].setOrientation(JSlider.HORIZONTAL);
	    } else {
		paramSliders[i].setOrientation(JSlider.HORIZONTAL);
	    }
	    paramSliders[i].setVisible(true);
	}	    
    }	    

    
    public void playSound() {
        boolean isSal = mainFrame.jScrollPane.isSal;
        /* debugging 
        System.out.println("isSal: " + isSal + "\n" +
                           currentInstr.getFunctionName() + "\nSal: " +
                           currentInstr.getSalImplementation() + "\nLisp: " +
                           currentInstr.getLispImplementation() + "\nReq: " +
                           currentInstr.getRequire());
           end debugging */
	String cmd = (isSal ? "play " : "(play (") + 
                     currentInstr.getFunctionName();
        String sep = (isSal ? ", " : " ");
        if (isSal) cmd = cmd + "(";
        boolean first = isSal;
        for (int i = 0; i < currentInstr.getParameters().size(); i++) {
	    Parameter p = (Parameter) currentInstr.getParameters().get(i);
	    // is this a keyword parameter? If so, put it in
	    if (p.getName().charAt(0) == ':') {
                if (!first) cmd = cmd + sep;
		cmd = cmd + p.getName();
                first = false;
	    }
	    String actual;
	    if (p.getType().equals("int")) {
		actual = new Integer(paramSliders[i].getValue()).toString();
	    } else if (p.getType().equals("float")) {
		actual = new Float(p.getValue()).toString();
	    } else {
		actual = "";
		System.out.println("bad parameter type");
	    }
            if (!first) cmd = cmd + sep;
            first = false;
	    cmd = cmd + actual;
	}
        if (!isSal) cmd = cmd + ")"; // to match (play
	cmd = cmd + ")\n";
        String impl = (isSal ? currentInstr.getSalImplementation() :
                               currentInstr.getLispImplementation());
        if (impl == null && currentInstr.getRequire() != null) {
            impl = currentInstr.getRequire();
            if (isSal) {
                impl = "if ! fboundp(quote(" + currentInstr.getFunctionName() +
                       ")) then load " + impl + "\n";
            } else {
                impl = "(if (not (fboundp '" + currentInstr.getFunctionName() +
                       ")) (load " + impl + "))\n";
            }
        }
        if (impl != null) {
            if (isSal) { // for SAL, need one command
                cmd = "begin\n  " + impl + "  " + cmd + "end\n";
            } else {
                cmd = impl + cmd; // lisp can take sequence of commands
            }
        }
        cmd = cmd + "\n"; // need extra newline to cause parser to run
	expArea.setText(cmd);
	mainFrame.sendInput(cmd);
    }
}    
	
