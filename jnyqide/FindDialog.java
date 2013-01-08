package jnyqide;
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



import java.util.*;
import java.awt.*;
import java.awt.event.*;
import jnyqide.*;

class FindDialog extends Dialog implements WindowListener, ActionListener
{
    private Button fbutton,cbutton;
    private MainFrame mainFrame;
    private NyquistFile nyquistFile;
    private TextField pattern;
    private Properties strings;

    public FindDialog(NyquistFile tf, MainFrame mainFrame_)
    {
        super(mainFrame_, "Find", true);

        setBackground(Color.lightGray);

        nyquistFile = tf;
        mainFrame = mainFrame_;

        Panel p1 = new Panel();
        p1.setLayout(new FlowLayout());
        p1.add(new Label("Find:"));


        pattern = new TextField();
        pattern.setColumns(35);

	if (tf.pane.getSelectedText() == null)           // BWP
	    pattern.setText(mainFrame.findPattern);
        else                                             // BWP
	    pattern.setText(tf.pane.getSelectedText());  // BWP

        p1.add(pattern);
        p1.doLayout();
        add("North", p1);

        Panel p2 = new Panel();
        fbutton = new Button("Find Next");
        fbutton.addActionListener(this);
        p2.add(fbutton);
        cbutton = new Button("Close");
        cbutton.addActionListener(this);
        p2.add(cbutton);
        add("South",p2);

        Dimension size = new Dimension(400, 110);
        setSize(size);
	    Point tfloc = tf.getLocation();
	    Point mfloc = mainFrame.getLocation();
        setLocation(mfloc.x + tfloc.x,
		            mfloc.y + tfloc.y + 85);

        addWindowListener(this);
	    setVisible(true);
    }
    
    public void windowDeiconified(WindowEvent event) {}
    public void windowIconified(WindowEvent event) {}
    public void windowActivated(WindowEvent event) {}
    public void windowDeactivated(WindowEvent event) {}
    public void windowOpened(WindowEvent event) {}
    public void windowClosed(WindowEvent event) {}
    public void windowClosing(WindowEvent event) {
        mainFrame.findPattern = pattern.getText();
        nyquistFile.lastFound = pattern.getText();  // BWP
	dispose();
    }

    public void actionPerformed(ActionEvent evt)
    {
        if (evt.getSource() == cbutton) {
            mainFrame.findPattern = pattern.getText();
	    nyquistFile.lastFound = pattern.getText();
            dispose();
            return;
        }

        if (evt.getSource() == fbutton)
            if (!nyquistFile.find(pattern.getText())) {
                NotFoundDialog nf = new NotFoundDialog(mainFrame, strings, 
						       getLocation());
                nf.setVisible(true);
            }
    }

}
