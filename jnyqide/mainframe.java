package jnyqide;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.event.*;
// import javax.swing.JOptionPane.*;
import javax.swing.filechooser.FileFilter;
import java.io.FileInputStream;
import java.io.File;
import jnyqide.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

class ScrollUpdate implements Runnable {
    MainFrame frame;
    ScrollUpdate(MainFrame mainframe) {
	frame = mainframe;
    }

    public void run() {
	frame.ScrollToEnd();
	// System.out.println("scroll to end");
    }
}


public class MainFrame extends JFrame {
    JPanel contentPane;
    JMenuBar jMenuBar1 = new JMenuBar();

    JMenu jMenuFile = new JMenu();
    JMenuItem jMenuFileExit = new JMenuItem();

    JMenu jMenuEdit = new JMenu();
    JMenuItem jMenuEditFind = new JMenuItem();
    JMenuItem jMenuEditReplace = new JMenuItem();
    JMenuItem jMenuEditPrevious = new JMenuItem();
    JMenuItem jMenuEditNext = new JMenuItem();

    JMenu jMenuProcess = new JMenu();
    JMenuItem jMenuProcessPlot = new JMenuItem();

    JMenu jMenuHelp = new JMenu();
    JMenuItem jMenuHelpAbout = new JMenuItem();

    JToolBar jToolBar = new JToolBar();
    JButton buttonNew = new JButton();
    JButton buttonOpen = new JButton();
    JButton buttonSave = new JButton();
    JButton buttonLoad = new JButton();
    JButton buttonReplay = new JButton();
    JButton buttonInfo = new JButton();
    JButton buttonBreak = new JButton();
    JButton buttonCont = new JButton();
    JButton buttonTop = new JButton();
    JButton buttonUp = new JButton();
    JButton buttonFn[] = new JButton[11];
    ImageIcon image1;
    ImageIcon image2;
    ImageIcon image3;
    JLabel statusBar = new JLabel();
    BorderLayout borderLayout1 = new BorderLayout();
    JDesktopPane jDesktop;
    JTextArea jOutputArea;
    JScrollPane jOutputPane;
    JTextPane jInputArea;
    NyquistThread nyquistThread;
    JInternalFrame jFrame;
    SyntaxThread syntaxThread;
    String currentDir = "";
    Runnable update = new ScrollUpdate(this);
    PlotFrame plotFrame;
    public String findPattern = "";
    public String replacePattern = "";

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

    int inputStringsX = 0;
    int inputStringsCursor = 0;
    int inputStringsLen = 20;
    String inputStrings[] = new String[inputStringsLen];
    
    //Construct the frame
    public MainFrame() {
        enableEvents(AWTEvent.WINDOW_EVENT_MASK);
        try {
            jbInit();
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }


    // create a button
    private void buttonInit(JButton button, String name, 
			    String tip, ActionListener listener) {
        button.setText(name);
	button.setToolTipText(tip);
	button.addActionListener(listener);
	jToolBar.add(button);
    }

    //Component initialization
    private void jbInit() throws Exception  {
        image1 = new ImageIcon("openFile.gif");
        image2 = new ImageIcon("closeFile.gif");
        image3 = new ImageIcon("help.gif");
        //setIconImage(Toolkit.getDefaultToolkit().createImage(MainFrame.class.getResource("[Your Icon]")));
        contentPane = (JPanel) this.getContentPane();
        contentPane.setLayout(borderLayout1);
        this.setSize(new Dimension(400, 300));
        this.setTitle("Nyquist IDE");
        statusBar.setText(" ");
        
        // Menu Bar
        jMenuFile.setText("File");
        
        jMenuFileExit.setText("Exit");
        jMenuFileExit.addActionListener(new ActionListener()  {
                public void actionPerformed(ActionEvent e) {
                    jMenuFileExit_actionPerformed(e);
                }
            });
        
        JMenuItem jMenuFileNew = new JMenuItem();
        jMenuFileNew.setText("New");
        jMenuFileNew.setMnemonic('n');
        jMenuFileNew.setAccelerator(
	        KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N,
				       java.awt.Event.CTRL_MASK));
        jMenuFileNew.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    jMenuFileNew_actionPerformed(e);
                }
            });
        
        JMenuItem jMenuFileOpen = new JMenuItem();
        jMenuFileOpen.setText("Open");
        jMenuFileOpen.setMnemonic('o');
        jMenuFileOpen.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O,
                                                            java.awt.Event.CTRL_MASK));
        jMenuFileOpen.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    jMenuFileOpen_actionPerformed(e);
                }
            });
        
        JMenuItem jMenuFileSave = new JMenuItem();
        jMenuFileSave.setText("Save");
        jMenuFileSave.setMnemonic('s');
        jMenuFileSave.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S,
                                                            java.awt.Event.CTRL_MASK));
        jMenuFileSave.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    jMenuFileSave_actionPerformed(e);
                }
            });
        
        JMenuItem jMenuFileSaveAs = new JMenuItem();
        jMenuFileSaveAs.setText("Save As...");
        jMenuFileSaveAs.setMnemonic('a');
        jMenuFileSaveAs.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    jMenuFileSaveAs_actionPerformed(e);
                }
            });
        
        JMenuItem jMenuFileLoad = new JMenuItem();
        jMenuFileLoad.setText("Load");
        jMenuFileLoad.setMnemonic('s');
        jMenuFileLoad.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_K,
                                                            java.awt.Event.CTRL_MASK));
        jMenuFileLoad.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    jMenuFileLoad_actionPerformed(e);
                }
            });

	jMenuEdit.setText("Edit");
	jMenuEditFind.setText("Find");
        jMenuEditFind.setAccelerator(
	        KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F,
				       java.awt.Event.CTRL_MASK));
	jMenuEditFind.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    jMenuEditFind_actionPerformed(e);
		}
	    });
	jMenuEditReplace.setText("Replace");
        jMenuEditReplace.setAccelerator(
	        KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_H,
				       java.awt.Event.CTRL_MASK));
	jMenuEditReplace.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    jMenuEditReplace_actionPerformed(e);
		}
	    });
	jMenuEditPrevious.setText("Previous");
        jMenuEditPrevious.setAccelerator(
	        KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,
				       java.awt.Event.CTRL_MASK));
	jMenuEditPrevious.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    jMenuEditPrevious_actionPerformed(e);
		}
	    });
	jMenuEditNext.setText("Next");
        jMenuEditNext.setAccelerator(
  	        KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D,
		 		       java.awt.Event.CTRL_MASK));
	jMenuEditNext.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    jMenuEditNext_actionPerformed(e);
		}
	    });



        jMenuHelp.setText("Help");
        jMenuHelpAbout.setText("About");
        jMenuHelpAbout.addActionListener(new ActionListener()  {
                public void actionPerformed(ActionEvent e) {
                    jMenuHelpAbout_actionPerformed(e);
                }
            });
        
        jMenuProcess.setText("Process");
        jMenuProcessPlot.setText("Plot");
        jMenuProcessPlot.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    jMenuProcessPlot_actionPerformed(e);
                }
            });
        
        JMenuItem jMenuProcessReplay = new JMenuItem();
        jMenuProcessReplay.setText("Repeat");
        jMenuProcessReplay.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    jMenuProcessReplay_actionPerformed(e);
                }
            });
        
        
        // buttonNew.setIcon(image1);
	buttonInit(buttonNew, "New File", "New File", 
		   new ActionListener() {
		       public void actionPerformed(ActionEvent e) {
			   jMenuFileNew_actionPerformed(e);
		       }
		   });
        
        // buttonOpen.setIcon(image1);
        buttonInit(buttonOpen, "Open File", "Open File", 
		   new ActionListener() {
		       public void actionPerformed( ActionEvent e ) {
			   jMenuFileOpen_actionPerformed(e);
		       }
		   });
        
        // buttonSave.setIcon(image3);
        buttonInit(buttonSave, "Save File", "Save File",
		   new ActionListener() {
		       public void actionPerformed( ActionEvent e ) {
			   jMenuFileSave_actionPerformed(e);
		       }
		   });

	buttonInit(buttonLoad, "Load", "Load File into Nyquist", 
		   new ActionListener() {
		       public void actionPerformed(ActionEvent e) {
			   jMenuFileLoad_actionPerformed(e);
		       }
		   });

        buttonInit(buttonReplay, "Replay", "Replay the last sound",
		   new ActionListener() {
		       public void actionPerformed(ActionEvent e) {
			   jMenuProcessReplay_actionPerformed(e);
		       }
		   });

        buttonInit(buttonInfo, "Info", "Print Lisp memory status",
		   new ActionListener() {
		       public void actionPerformed(ActionEvent e) {
			   jMenuProcessInfo_actionPerformed(e);
		       }
		   });

        buttonInit(buttonBreak, "Break", "Break/interrupt Lisp interpreter",
		   new ActionListener() {
		       public void actionPerformed(ActionEvent e) {
			   jMenuProcessBreak_actionPerformed(e);
		       }
		   });

        buttonInit(buttonCont, "Cont", "Continue Lisp execution",
		   new ActionListener() {
		       public void actionPerformed(ActionEvent e) {
			   jMenuProcessCont_actionPerformed(e);
		       }
		   });

        buttonInit(buttonTop, "Top", "Exit to Lisp top-level",
		   new ActionListener() {
		       public void actionPerformed(ActionEvent e) {
			   jMenuProcessTop_actionPerformed(e);
		       }
		   });

        buttonInit(buttonUp, "Up", "Return from this break level",
		   new ActionListener() {
		       public void actionPerformed(ActionEvent e) {
			   jMenuProcessUp_actionPerformed(e);
		       }
		   });

	int i;
	for (i = 0; i < 11; i++) {
	    String name = "F" + (i + 2);
            String tip = "Evaluate (" + name + ")";
	    buttonFn[i] = new JButton();
	    buttonFn[i].setActionCommand(name);
	    buttonInit(buttonFn[i], name, tip,
		       new ActionListener() {
			   public void actionPerformed(ActionEvent e) {
			       jMenuProcessFn_actionPerformed(e);
			   }
		       });
	}
        jMenuFile.add(jMenuFileNew);
        jMenuFile.add(jMenuFileOpen);
        jMenuFile.add(jMenuFileSave);
        jMenuFile.add(jMenuFileSaveAs);
        jMenuFile.add(jMenuFileLoad);
        jMenuFile.add(jMenuFileExit);
	jMenuEdit.add(jMenuEditFind);
	jMenuEdit.add(jMenuEditReplace);
	jMenuEdit.add(jMenuEditPrevious);
	jMenuEdit.add(jMenuEditNext);
        jMenuHelp.add(jMenuHelpAbout);
        jMenuProcess.add( jMenuProcessReplay );
        jMenuProcess.add( jMenuProcessPlot );
        jMenuBar1.add(jMenuFile);
        jMenuBar1.add(jMenuEdit);
        jMenuBar1.add(jMenuProcess);
        jMenuBar1.add(jMenuHelp);
        this.setJMenuBar(jMenuBar1);
        
        jOutputArea = new JTextArea();
	jOutputArea.setLineWrap(true);
	jOutputArea.setEditable(false);
        jOutputPane = new JScrollPane( jOutputArea );
        jOutputPane.setHorizontalScrollBarPolicy( JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS );
        jOutputPane.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_ALWAYS );
        
        // Command window
        final DefaultStyledDocument doc = new DefaultStyledDocument();
        jInputArea = new JTextPane(doc);
        final JScrollPane jInputPane = new JScrollPane( jInputArea );
        jInputPane.setHorizontalScrollBarPolicy( JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS );
        jInputPane.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_ALWAYS );
        jInputPane.setPreferredSize(new Dimension(400, 200));
        jInputArea.addKeyListener( new KeyAdapter() {
                public void keyTyped( KeyEvent ke ) {
                    if (ke.getKeyChar() == '\n') {
			// we just entered a newline into the document,
			// so first we need to take it back out 
			int pos = jInputArea.getCaretPosition();
			// I can't figure out how to edit the document,
                        // so we'll take out the text, do the edit on 
			// the string
                        JTextPane area = (JTextPane)ke.getSource();
			StringBuffer text = new StringBuffer(area.getText());
			// sometimes newlines are CRLF sequence. First step
			// is to convert them to \n's 
			filterCRLF(text);
			System.out.println("text |" + text +  "| pos " + pos);
			text.deleteCharAt(pos - 1);
			int i;
			for (i = 0; i < text.length(); i++) {
			    int j = text.charAt(i);
			    System.out.println(j);
			}
                        if (syntaxThread.isParensEven()) {
			    inputStrings[inputStringsX] = new String(text);
			    // for some reason, text sometimes ends up
			    // with a CR LF at end. Make sure it's gone
			    // before we output to Nyquist
			    sendInput(inputStrings[inputStringsX]);
			    sendInput("\n");
			    System.out.println("text sent to Nyquist");
			    inputStringsX = inputStringsX + 1;
			    if (inputStringsX >= inputStringsLen) {
				inputStringsX = 0;
			    }
			    inputStringsCursor = inputStringsX;
			    area.setText("");
			} else {
			    jOutputArea.append(
				   "Invalid command - paren mismatch\n");
			}
		    }
                }
            });
        syntaxThread = new SyntaxThread(jInputPane, jInputArea, doc);
        syntaxThread.start();
	syntaxThread.update();
        // TextColor.format(jInputPane, jInputArea, doc, syntaxThread);
        doc.addDocumentListener(new DocumentListener() {
                public void changedUpdate(DocumentEvent e) {
                }
                public void removeUpdate(DocumentEvent e) {
                    syntaxThread.update();
                }
                public void insertUpdate(DocumentEvent e) {
                    syntaxThread.update();
                }
            });
        
        
        // Top panel for command entry, plot, and toolbar
        JPanel jCommands = new JPanel( new BorderLayout() );
	JPanel jInputAndPlot = new JPanel(new BorderLayout(3, 0));
        jInputAndPlot.add(jInputPane, BorderLayout.WEST);
        jCommands.add(jToolBar, BorderLayout.SOUTH);
        jCommands.add(jInputAndPlot, BorderLayout.CENTER);
        jCommands.setPreferredSize(new Dimension(300, 150));
        
        // Main desktop
        jDesktop = new JDesktopPane();
        jDesktop.setPreferredSize( new Dimension(300, 300) );
        
        JInternalFrame jOutputFrame = new JInternalFrame("Output");
	// make this wide enough so XLISP GC messages do not wrap 
	//   (it's annoying)
        jOutputFrame.setSize( new Dimension(500, 580) );
        jOutputFrame.setVisible(true);
        jOutputFrame.getContentPane().add(jOutputPane);
        jOutputFrame.setResizable( true );
        jDesktop.add( jOutputFrame );
        
        contentPane.add( jCommands, BorderLayout.NORTH);
        contentPane.add( jDesktop, BorderLayout.CENTER );
        contentPane.add(statusBar, BorderLayout.SOUTH);
        setSize( new Dimension(800, 800) );
        
        TextColor.init();
        
        /*   JInternalFrame jIFrame = new JInternalFrame("Hello");
             jIFrame.setLocation(50+10, 50+10);
             jIFrame.setSize(200, 150);
             jIFrame.setBackground(Color.white);
             jIFrame.setVisible(true);
             jDesktop.add( jIFrame );
             jDesktop.getDesktopManager().openFrame( jIFrame );*/

        plotFrame = new PlotFrame(jInputAndPlot);

        nyquistThread = new NyquistThread();
        // nyquistThread.setInputArea( jInputArea );
        // nyquistThread.getReady(jOutputArea, update, );
        nyquistThread.start(jOutputArea, update, plotFrame, this);
    }
    
    //File | Exit action performed
    public void jMenuFileExit_actionPerformed(ActionEvent e) {
	JInternalFrame[] frames = jDesktop.getAllFrames();
	boolean flag = false;
	int i;
	for (i = 0; i < frames.length; i++) {
	    if (frames[i] instanceof NyquistFile) {
		NyquistFile nyquistFile = (NyquistFile) frames[i];
		if (nyquistFile.modified) flag = true;
	    }
	}
	int r = JOptionPane.OK_OPTION;
	if (flag) {
	    r = JOptionPane.showConfirmDialog(this,
		        "Really close without saving?",
			"alert", JOptionPane.OK_CANCEL_OPTION);
	}
	if (r == JOptionPane.OK_OPTION) {
	    System.exit(0);
	}
	// otherwise do not quit
    }
    
    public void jMenuFileNew_actionPerformed(ActionEvent e) {
        final NyquistFile file = new NyquistFile();
        jDesktop.add(file);
        jDesktop.getDesktopManager().activateFrame(file);
        jDesktop.setSelectedFrame(file);
	file.addInternalFrameListener(
	    new InternalFrameListener() {
                public void internalFrameClosing(InternalFrameEvent e) {
		    System.out.println("FrameClosing");
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
		    System.out.println("FrameClosed");
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
    
    public String fileDirectory(File file) {
	String path = file.getAbsolutePath();
	String name = file.getName();
	return path.substring(0, path.length() - name.length());
    }



    //File | Exit action performed
    public void jMenuFileOpen_actionPerformed(ActionEvent e) {
        JFileChooser chooser = new JFileChooser();
        LispFileFilter filter = new LispFileFilter();
        chooser.setFileFilter(filter);
        // note if current directory setting fails on some platform,
	// consider this code using getCanonicalPath():
        //      File f = new File(new File(".").getCanonicalPath());
	File curdir = new File(currentDir);
	chooser.setCurrentDirectory(curdir);
        int returnVal = chooser.showOpenDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            System.out.println("You chose to open this file: " +
                               chooser.getSelectedFile().getAbsoluteFile());
	    // see if file is already open
	    JInternalFrame[] frames = jDesktop.getAllFrames();
	    int i;
	    for (i = 0; i < frames.length; i++) {
		if (frames[i] instanceof NyquistFile) {
		    NyquistFile file = (NyquistFile) frames[i];

		    if (file.getFile() != null &&
                        file.getFile().getAbsolutePath().equals( 
			        chooser.getSelectedFile().getAbsolutePath())) {
			jDesktop.setSelectedFrame(file);
			try {
   			    file.setSelected(true); 
			}
			catch(PropertyVetoException ve) {
			    System.out.println("setSelected was vetoed");
			}
						       
			JInternalFrame jInternalFrame = 
			    jDesktop.getSelectedFrame();
			if (jInternalFrame instanceof NyquistFile) {
			    NyquistFile nf = (NyquistFile) jInternalFrame;
			    System.out.println("selected is " + 
					       nf.getAbsolutePath());
			} else {
			    System.out.println("selected not a NyquistFile");
			}
			return;
		    }
		}
	    }
	    // Didn't find it. Open it in a new frame.
            final NyquistFile file = 
		new NyquistFile(chooser.getSelectedFile());
            jDesktop.add(file);
            jDesktop.getDesktopManager().activateFrame(file);
            jDesktop.setSelectedFrame(file);
            changeDirectory(fileDirectory(chooser.getSelectedFile()));
	    // file.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
	    file.addInternalFrameListener(
		new InternalFrameListener() {
                    public void internalFrameClosing(InternalFrameEvent e) {
			System.out.println("FrameClosing");
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
			System.out.println("FrameClosed");
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
    }
    
    public void jMenuFileSave_actionPerformed(ActionEvent e) {
        if (jDesktop.getSelectedFrame() instanceof NyquistFile) {
            NyquistFile file = (NyquistFile)jDesktop.getSelectedFrame();
            if (file.save(currentDir)) {
                changeDirectory(fileDirectory(file.getFile()));
	    }
        }
    }
    
    public void jMenuFileSaveAs_actionPerformed(ActionEvent e) {
        if (jDesktop.getSelectedFrame() instanceof NyquistFile) {
            NyquistFile file = (NyquistFile)jDesktop.getSelectedFrame();
            if (file.saveAs(currentDir)) {
		changeDirectory(fileDirectory(file.getFile()));
	    }
        }
    }
    
    public void jMenuFileLoad_actionPerformed(ActionEvent e) {
        JInternalFrame frame = jDesktop.getSelectedFrame();
	if (frame instanceof NyquistFile) {
            NyquistFile file = (NyquistFile) frame;
	    if (file.save(currentDir)) {
		changeDirectory(fileDirectory(file.getFile()));
		loadFile(file.getAbsolutePath());
	    }
        }
    }
    
    //Edit | Find action performed
    public void jMenuEditFind_actionPerformed(ActionEvent e) {
        JInternalFrame frame = jDesktop.getSelectedFrame();
	if (frame instanceof NyquistFile) {
            NyquistFile file = (NyquistFile) frame;
            FindDialog findDialog = new FindDialog(file, this);
	}
    }

    //Edit | Replace action performed
    public void jMenuEditReplace_actionPerformed(ActionEvent e) {
        JInternalFrame frame = jDesktop.getSelectedFrame();
	if (frame instanceof NyquistFile) {
            NyquistFile file = (NyquistFile) frame;
            ReplaceDialog replaceDialog = new ReplaceDialog(file, this);
	}
    }

    public void filterCRLF(StringBuffer buf)
    {
	int i = buf.indexOf("\r\n");
        while (i >= 0) {
	    buf.replace(i, i + 2, "\n");
	    i = buf.indexOf("\r\n", i);
	}
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
    public void jMenuEditPrevious_actionPerformed(ActionEvent e) {
	inputStringsCursor = inputStringsCursor - 1;
	if (inputStringsCursor < 0) inputStringsCursor = inputStringsLen - 1;
	String text = inputStrings[inputStringsCursor];
	if (text != null) {
	    // remove the newline at the end
	    jInputArea.setText(trimNewline(text));
	}
    }

    //Edit | Next action performed
    public void jMenuEditNext_actionPerformed(ActionEvent e) {
	inputStringsCursor = inputStringsCursor + 1;
	if (inputStringsCursor >= inputStringsLen) inputStringsCursor = 0;
	String text = inputStrings[inputStringsCursor];
	if (text != null) {
	    jInputArea.setText(trimNewline(text));
	}
    }

    //Help | About action performed
    public void jMenuHelpAbout_actionPerformed(ActionEvent e) {
        MainFrame_AboutBox dlg = new MainFrame_AboutBox(this);
        Dimension dlgSize = dlg.getPreferredSize();
        Dimension frmSize = getSize();
        Point loc = getLocation();
        dlg.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
        dlg.setModal(true);
        dlg.show();
        
        Graphics g = jFrame.getContentPane().getGraphics();
        g.setColor(Color.cyan);
        g.fillRect(50, 50, 100, 100);
        
    }
    
    public void jMenuProcessReplay_actionPerformed(ActionEvent e)
    {
        sendInput("(r)\n");
    }
    
    public void jMenuProcessInfo_actionPerformed(ActionEvent e)
    {
        sendInput("(info)\n");
    }
    
    public void jMenuProcessBreak_actionPerformed(ActionEvent e)
    {
        sendInput("\02\n");
    }
    
    public void jMenuProcessCont_actionPerformed(ActionEvent e)
    {
        sendInput("(continue)\n");
    }
    
    public void jMenuProcessTop_actionPerformed(ActionEvent e)
    {
        sendInput("(top)\n");
    }
    
    public void jMenuProcessUp_actionPerformed(ActionEvent e)
    {
        sendInput("(up)\n");
    }
    
    public void jMenuProcessFn_actionPerformed(ActionEvent e)
    {
        sendInput("(" + e.getActionCommand() + ")\n");
    }
    
    // Plot command
    public void jMenuProcessPlot_actionPerformed(ActionEvent e) {
        NyqPlot.plot("points.dat", plotFrame);
    }
    
    //Overridden so we can exit (if confirmed) when window is closed
    protected void processWindowEvent(WindowEvent e) {
        // super.processWindowEvent(e);
        if (e.getID() == WindowEvent.WINDOW_CLOSING) {
            jMenuFileExit_actionPerformed(null);
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
        if (!currentDir.equals(dir)) {
            currentDir = dir;
	    String escapedDir = escape_backslashes(dir);
	    String cmd = "(setdir \"" + escapedDir + "\")\n";
            sendInput(cmd);
        }
    }

    // tell nyquist to load a file
    public void loadFile(String path) {
	path = escape_backslashes(path);
	String cmd = "(load \"" + path + "\")\n";
	sendInput(cmd);
    }

    // send data to Nyquist process
    public void sendInput(String text) {
	jOutputArea.append(text);
	//jOutputArea.addNotify();
	SwingUtilities.invokeLater(update); //new ScrollUpdate(this));
	//jOutputArea.scrollRectToVisible(
	//        new Rectangle(0, jOutputArea.getHeight() - 2, 1, 1));
	nyquistThread.sendInput(text);
    }	

    public void ScrollToEnd() {
	JScrollBar scroll = jOutputPane.getVerticalScrollBar();
        scroll.setValue(scroll.getMaximum() - scroll.getVisibleAmount());
    }
}

