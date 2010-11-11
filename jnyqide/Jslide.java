package jnyqide;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.event.*;
import java.util.*;

public class Jslide extends JFrame implements ActionListener {
    MainFrame myParent;
    static final int numSliders = 8;
    static final int numEqualizers = 10;
    /* Sliders and labels that will be used on the gui*/
    JSlider[] js;
    float[][] eqState;
    
    /* Buttons that will appear on the Equalizer*/
    JButton setEqValue = new JButton("Set");
    JButton loadEqValue = new JButton("Load");
    JButton restore = new JButton("Reset");
    JComboBox eqList = new JComboBox();
    JPanel p1 = new JPanel();
    JPanel GraphEqPanel;
    String extension = "";
    // Create text fields to display slider position
    String[] labels = {"84", "167", "330", "652", "1.3k", "2.5k", "5k", "10k"};
    JLabel[] jlabels;
    boolean modified = false;
    
    public Jslide(MainFrame parent)
    {
        myParent = parent;
    	/*Open the file and set sliders to the default*/
    	//fileName = "eq" + extension + ".dat";
    	//setAllSliders(fileName);
    
    	/* Action listener for the restore button. This
    	 * restores all slider values to zero.  Right now
    	 * this has no effect on the file itself.
    	 */
        js = new JSlider[numSliders];
        eqState = new float[numEqualizers][numSliders];
        for (int i = 0; i < numSliders; i++) {
            js[i] = initializeSlider();
            for (int j = 0; j < numEqualizers; j++) eqState[j][i] = 0;
        }
        
        setEqValue.addActionListener(this);
        setEqValue.setActionCommand("set-eq-value");
        loadEqValue.addActionListener(this);
        loadEqValue.setActionCommand("load-eq-value");  
        
        restore.addActionListener(
            new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    System.out.println("reset listener");
                    for (int i = 0; i < numSliders; i++) {
                        js[i].setValue(0);
                    }
                }
            }
        );
    	   	
        /*Create individual Panels to be added to the Pane*/
     
        /* Slider panel*/
        p1.setBackground(Color.WHITE);
        p1.setSize(500,300);
        p1.setLayout(new GridLayout(1, 8));
        /* Panel with all the labels*/
        JPanel p2 = new JPanel();
        p2.setBackground(Color.WHITE);
        p2.setLayout(new GridLayout(1, 8));
        /* add the sliders and labels */
        jlabels = new JLabel[numSliders];
        for (int i = 0; i < numSliders; i++) {
            p1.add(js[i]);
            jlabels[i] = new JLabel(labels[i]);
            p2.add(jlabels[i]);
        }
        /* Combo box with a liste of numbers of equalizer values*/
        String[] eqNums = {"0","1", "2", "3", "4","5","6","7","8","9"};
        eqList = new JComboBox(eqNums);
        eqList.addActionListener(this);
        eqList.setActionCommand("eq-selection");
     
        /*Panel the buttons, and the combo box on it*/
        JPanel pA = new JPanel();
        pA.setLayout(new GridLayout(1,4));
        pA.setBackground(Color.WHITE);
        pA.add(setEqValue);
        pA.add(loadEqValue);
        pA.add(restore);
        pA.add(eqList);
     	 
        pA.setSize(50,50);

        GraphEqPanel = new JPanel();
        GraphEqPanel.setLayout(new BorderLayout());
        GraphEqPanel.setBackground(Color.WHITE);
        GraphEqPanel.setSize(300,300);
        GraphEqPanel.add(p1, BorderLayout.NORTH);
        GraphEqPanel.add(p2, BorderLayout.CENTER);
        GraphEqPanel.add(pA, BorderLayout.SOUTH);
        
        loadEqValue();
    } //Close constructor
     
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();
        if (command.equals("eq-selection")) {
            equalizerSelected();
        } else if (command.equals("set-eq-value")) {
            setEqValue();
        } else if (command.equals("load-eq-value")) {
            loadEqValue();
        }
    }
     
    public void equalizerSelected() {
        String name = (String) eqList.getSelectedItem();
        int index = new Integer(name);
        // load values from state
        for (int i = 0; i < numSliders; i++) {
            js[i].setValue((int) eqState[index][i]);
        }
    }
    
    public void setEqValue() {
        // store slider values into state
        String name = (String) eqList.getSelectedItem();
        int index = new Integer(name);
        // store value into state
        String definition = "(define-eq " + name + " #(";
        for (int i = 0; i < numSliders; i++) {
            eqState[index][i] = js[i].getValue();
            definition += (" " + eqState[index][i]);
        }
        definition += "))\n\n";
        myParent.sendInput(definition);
    }
    
    public void loadEqValue() {
        myParent.callFunction("get-eq-data", "");
    }
    
    /**
     * Public function that returns the graphic equalizer panel
     * @return
     */
    public JPanel getGraphEq() {
  	    return GraphEqPanel;
    }

    /**The getter for the GUI sliders
     * @return db value as an int
     */
   	private int get_db_val(JSlider j){
   		return j.getValue();
   	}
    
     /**
      * A Private method that will initialize the sliders.  This is
      * to clean up the code and make it shorter.
      * @param j
      */
     private JSlider initializeSlider() {
    	 JSlider j = new JSlider(JSlider.VERTICAL, -15,15,0);
         j.setMajorTickSpacing(10);
         j.setMinorTickSpacing(2);
         j.setPaintTicks(true);
         j.setPaintLabels(true);
         j.setForeground(Color.BLACK);
         j.setBorder(BorderFactory.createEtchedBorder());
         return j;
     }

    public void loadEqData(String data) {
        System.out.println("loadEqData " + data);
        while (true) {
            int i = data.indexOf("\n");
            if (i < 0) {
                equalizerSelected(); // load sliders from eqState
                return;
            }
            String line = data.substring(0, i);
            System.out.println("loadEqData line " + line);
            data = data.substring(i + 1);
            // string has 9 numbers
            StringTokenizer st = new StringTokenizer(line);
            String name = st.nextToken();
            int index = new Integer(name);
            for (i = 0; i < numSliders; i++) {
                eqState[index][i] = new Float(st.nextToken());
            }
        }
    }
}
     
     
     
