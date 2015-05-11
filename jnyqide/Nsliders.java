// Nsliders.java -- implementation of graphical sliders for
//   delivering parameters to Nyquist
//
// From the Nyquist side, functions are
//    (stop-on-zero s) -- a sound that returns s until s goes to zero, then
//            the sound terminates. If s comes from a slider and you multiply
//            a sound by (stop-on-zero s), you can interactively stop it
//    (make-slider-panel "name" color) -- sets panel name for the following sliders
//    (make-slider "param" [initial [low high]]) -- create slider named "param" with 
//            optional range and initial value. Also returns a sound.
//    (make-button "param" normal) -- create a button named "param" with
//            a starting value of normal (either 0 or 1). While the button
//            in the panel is pressed, the value changes to 1 or 0. Returns
//            a sound.
//    (slider "panel" "name" [dur]) -- make a signal from slider value
//    (slider "name" [dur]) -- make a signal from slider in current panel
//    (get-slider-value "panel" "name") -- get a float value
//    (get-slider-value "name") -- get a float in current panel
//    (slider-panel-close "name") -- close the panel window. Values of any 
//            existing sliders become undefined.
//
// From the Java side, slider commands are issued via text commands.
//     slider-panel-create: "panel" color  -- this creates a fresh new panel
//         with the given name and color. Any old panel by that name is deleted.
//         This also sets the current panel to be populated by sliders.
//     slider-create: "slidername" num initial low high -- adds a slider to current 
//         panel. The slider index is num, e.g. see GET-SLIDER-VALUE
//     button-create: "buttonname" num initial -- adds a button to current 
//         panel. The slider index is num, e.g. see GET-SLIDER-VALUE
//     slider-panel-close: "panel" -- removes panel
//
// Slider changes are issued via "hidden messages"
//     '\016' -- begin hidden message
//     'S' -- slider message
//     <n> -- slider number, integer as string
//     <space> -- a blank space separator
//     <v> -- slider value, float as string
//     '\021' -- end hidden message
//
// Button changes are issued as slider changes.

package jnyqide;

import javax.swing.*;
import java.awt.*;
import java.awt.Color;
import java.awt.event.*;
import javax.swing.event.*;
import java.util.ArrayList;

// Overview:
// Classes to support this are:
//   Nsliders
//   Nslider

public class Nsliders extends JInternalFrame {
    // implements ActionListener {

    static MainFrame mainFrame;

    class Nslider extends JSlider {
        public int num;
        JTextField text;
        Double unit;
        JSlider thisSlider;

        public void setSliderValue(double val) {
            mainFrame.sendInput("\016S" + num + " " + val + "\021", true);
        }


        public Nslider(JTextField tf, int id, 
                       double init, double low, double high) {
            unit = (high - low) / 1000.0;
            setMinimum((int) Math.round(low / unit));
            setMaximum((int) Math.round(high / unit));
            setValue((int) Math.round(init / unit));
            text = tf;
            num = id;
            thisSlider = this;
            String initStr = Double.toString(init);
            if (initStr.length() > 6) initStr = initStr.substring(0, 6);
            text.setText(initStr);
            setSliderValue(init); // make sure slider has initial value
            addChangeListener(new ChangeListener() {
                    public void stateChanged(ChangeEvent e) {
                        setSliderValue(getValue() * unit);
                        String s = Double.toString(getValue() * unit);
                        if (s.length() > 6) s = s.substring(0, 6);
                        text.setText(s);
                    }
                });
            text.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        thisSlider.setValue((int) Math.round(
                                Double.parseDouble(text.getText()) / unit));
                        System.out.println("text change: " + text.getText());
                    }
                });
        }
    }

    class Nbutton extends JButton {
        public int num;
        public int normal;
        JButton thisButton;

        public void setButtonValue(int val) {
            mainFrame.sendInput("\016S" + num + " " + val + "\021", true);
        }

        public Nbutton(int id, String name, int normal_) {
            super(name);
            thisButton = this;
            normal = normal_;
            num = id;
            setButtonValue(normal);
            addMouseListener(new MouseListener() {
                    public void mouseClicked(MouseEvent e) {}
                    public void mouseEntered(MouseEvent e) {}
                    public void mouseExited(MouseEvent e) {}
                    public void mousePressed(MouseEvent e) {
                        setButtonValue(1 - normal);
                        System.out.println("Nbutton set " + (1 - normal));
                    }
                    public void mouseReleased(MouseEvent e) {
                        setButtonValue(normal);
                        System.out.println("Nbutton set " + normal);
                    }
                });
        }
    }
        
    // These are background colors for slider boxes, named with index 0-12
    final static Color[] colors = { 
        new Color(192, 192, 192), // grey at index 0
        new Color(255, 102, 102), new Color(255, 178, 102), 
        new Color(255, 255, 102), new Color(178, 255, 102),
        new Color(102, 255, 102), new Color(102, 255, 178),
        new Color(102, 255, 255), new Color(102, 178, 255),
        new Color(102, 102, 255), new Color(178, 102, 255),
        new Color(255, 102, 255), new Color(255, 102, 178) };

    Container c;
    ArrayList<JComponent> sliders = new ArrayList<JComponent>();
    GridBagLayout layout = new GridBagLayout();

    public Nsliders(String name, int color, MainFrame m) {
        super(name, true, true);
        mainFrame = m;
        c = getContentPane();
        c.setLayout(layout);
        if (color < 0 || color >= colors.length) color = 0;
        c.setBackground(colors[color]);
        setLocation(500, 0);
        setVisible(true);
    }

    public void addSlider(String name, int id, 
                          double init, double low, double high) {
        int rows = sliders.size() + 1;

        GridBagConstraints constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.anchor = GridBagConstraints.WEST;
        constraints.insets = new Insets(1, 1, 1, 1);
        constraints.gridx = 0;
        constraints.gridy = rows - 1;
        constraints.weighty = 0;
        c.add(new JLabel(name), constraints);

        JTextField text = new JTextField("", 5);
        text.setColumns(5);

        Nslider s = new Nslider(text, id, init, low, high);
        
        sliders.add(s);

        constraints.gridx = 1;
        constraints.weightx = 1.0;
        c.add(s, constraints);

        constraints.gridx = 2;
        constraints.weightx = 0.0;
        c.add(text, constraints);
        c.layout();
        pack();
    }

    public void addButton(String name, int id, int normal) {
        int rows = sliders.size() + 1;
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.anchor = GridBagConstraints.WEST;
        constraints.insets = new Insets(1, 1, 1, 1);
        // constraints.gridx = 0;
        constraints.gridy = rows - 1;
        constraints.weighty = 0;
        // c.add(new JLabel(name), constraints);

        Nbutton s = new Nbutton(id, name, normal);
        
        sliders.add(s);

        constraints.gridx = 1;
        constraints.weightx = 0.0;
        c.add(s, constraints);

        c.layout();
        pack();
    }        

    /*
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();
        System.out.println(command);
    }
    */
}
