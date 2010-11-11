/*
 * Copyright (c) 1997 John Jensen. All rights reserved.
 *
 * This software is FREE FOR COMMERCIAL AND NON-COMMERCIAL USE,
 * provided the following condition is met.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that any copy or derivative of this software or documentation
 * retaining the name "John Jensen" also retains this condition and the
 * following disclaimer.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * CopyrightVersion 1.0
 */
package jnyqide;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import jnyqide.*;

class ReplaceDialog extends Dialog implements WindowListener, ActionListener
{
    private Button fbutton,rbutton,cbutton;
    private TextField	pattern;
    private TextField	replace;
    private MainFrame mainFrame;
    private NyquistFile	nyquistFile;
    private Properties	strings;

    private boolean foundOnce = false;

    public ReplaceDialog(NyquistFile tf, MainFrame mainFrame_)
    {
        super(mainFrame_, "Replace", true);

        mainFrame = mainFrame_;

        setBackground(Color.lightGray);

        nyquistFile = tf;

        Panel p1 = new Panel();
        
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints constraints = new GridBagConstraints();
        p1.setLayout(gridbag);

        Label flabel = new Label("Find");
        constraints.anchor = GridBagConstraints.NORTHWEST;
        gridbag.setConstraints(flabel, constraints);

        pattern = new TextField();
        pattern.setColumns(35);

        if(tf.pane.getSelectedText() == null)           // BWP
	    pattern.setText(mainFrame.findPattern);
        else                                            // BWP
	    pattern.setText(tf.pane.getSelectedText()); // BWP

        constraints.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(pattern, constraints);

        p1.add(flabel);
        p1.add(pattern);

        Label rlabel = new Label("Replace");
        constraints.anchor = GridBagConstraints.WEST;
        constraints.gridwidth = 1;
        gridbag.setConstraints(rlabel, constraints);

        replace = new TextField();
        replace.setColumns(35);
        
        replace.setText(mainFrame.replacePattern);

        constraints.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(replace, constraints);

        p1.add(rlabel);
        p1.add(replace);

        add("Center", p1);

        Panel p3 = new Panel();
        fbutton = new Button("Find Next");
        fbutton.addActionListener(this);
        p3.add(fbutton);
        rbutton = new Button("Replace");
        rbutton.addActionListener(this);
        p3.add(rbutton);
        cbutton = new Button("Close");
        cbutton.addActionListener(this);
        p3.add(cbutton);
        add("South",p3);

        Dimension size = new Dimension(400,120);
        setSize(size);
	Point tfloc = tf.getLocation();
	Point mfloc = mainFrame.getLocation();
        setLocation(mfloc.x + tfloc.x,
		    mfloc.y + tfloc.y + 75);

        addWindowListener(this);
	setVisible(true);
    }
    
    public void windowDeiconified(WindowEvent event) {}
    public void windowIconified(WindowEvent event) {}
    public void windowActivated(WindowEvent event) {}
    public void windowDeactivated(WindowEvent event) {}
    public void windowOpened(WindowEvent event) {}
    public void windowClosed(WindowEvent event) {}
    public void windowClosing(WindowEvent event)
    {
	mainFrame.findPattern = pattern.getText();
	mainFrame.replacePattern = replace.getText();
	dispose();
    }

    public void actionPerformed(ActionEvent evt)
    {
        if (evt.getSource() == cbutton)
        {
            mainFrame.findPattern = pattern.getText();
	    mainFrame.replacePattern = replace.getText();
            dispose();
            return;
        }

        if (evt.getSource() == fbutton)
            foundOnce = nyquistFile.find(pattern.getText());
        else if (evt.getSource() == rbutton)
        {
            if (!foundOnce)
            {
                String selection= nyquistFile.copy(false,false);
                if (selection != null)
                    foundOnce = selection.equals(pattern.getText());
            }

            if (foundOnce)
                nyquistFile.paste(replace.getText());

            foundOnce = nyquistFile.find(pattern.getText());
        }

        if (!foundOnce)
        {
            NotFoundDialog nf = new NotFoundDialog(mainFrame, strings,
						   getLocation());
            nf.setVisible(true);
        }
    }
}
