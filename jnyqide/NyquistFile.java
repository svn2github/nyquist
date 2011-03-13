package jnyqide;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.event.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.undo.*;
import jnyqide.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */


// BWP -- added "implements ActionListener"
public class NyquistFile extends JInternalFrame 
                         implements ActionListener, ComponentListener {
    DefaultStyledDocument doc;
    JTextPane pane;
    public final CodePane filePane;
    JLabel statusBar = new JLabel();
    UndoManager undo;
    File file;
    public boolean modified = false;
    NyquistFile thisFile;
    MainFrame myParent; //BWP
    
    String lastFound = ""; //BWP -- BUG, shouldn't these be global?
    //String revertTo = "";  //BWP

    public File getFile() {
	return file;
    }


    // BWP: Constructors now take a MainFrame object to associate
    //      the document window with the parent window for Find
    //      and Find/Replace operations
    public NyquistFile(MainFrame parent, int fontSize) { // BWP
        this(null, parent, fontSize);                    // BWP
        modified = true;
        setTitle(getTitle()+"*");
    }
    
    
    public void readFile(File file) {
        if (file != null) {
            try {
                FileInputStream openFileStream = new FileInputStream(file);
                
                byte b[] = new byte[1000]; // read 1K at a time
                int pos = 0;
		//System.out.println("starting to read file");
                doc.remove(0, doc.getLength()); // clear the current content
                while (openFileStream.available() > 0) {
                    int count = openFileStream.read(b);
                    doc.insertString(pos, new String(b, 0, count),
                                     TextColor.attrNormal);
                    pos += count;
                }
            }
            catch (Exception e2) { System.out.println(e2); }
        }
    }
    
    
    public void determineType() {
        String name = file.getName();
        filePane.isSal = name.toLowerCase().endsWith(".sal");
    }

    
    public NyquistFile(File f, MainFrame parent, int fontSize) {
        super();

        setTitle(f != null ? f.getName() : "Untitled");
        file = f;
        thisFile = this;

        myParent = parent;

        int keyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        // Menu Bar for top of Document Windows
        JMenuBar myMenuBar = new JMenuBar();

        // File Menu
        JMenu myFileMenu = new JMenu("File");
        
        JMenuItem myLoad = new JMenuItem("Load");
        myLoad.setActionCommand("load");
        myLoad.addActionListener(this);
        myLoad.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_K, keyMask));

        JMenuItem mySave = new JMenuItem("Save");
        mySave.setActionCommand("save");
        mySave.addActionListener(this);
        mySave.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, keyMask));

        JMenuItem mySaveAs = new JMenuItem("Save As...");
        mySaveAs.setActionCommand("save as");
        mySaveAs.addActionListener(this);
        mySaveAs.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, 
				keyMask | java.awt.event.InputEvent.SHIFT_MASK));

        JMenuItem myRevert = new JMenuItem("Revert");
        myRevert.setActionCommand("revert");
        myRevert.addActionListener(this);

        JMenuItem myClose = new JMenuItem("Close");
        myClose.setActionCommand("close");
        myClose.addActionListener(this);
        myClose.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, keyMask));

        myFileMenu.add(myLoad);
        myFileMenu.add(mySave);
        myFileMenu.add(mySaveAs);
        myFileMenu.add(myRevert);
        myFileMenu.add(myClose);

        // Edit Menu
        JMenu myEditMenu = new JMenu("Edit");

        JMenuItem myCut = new JMenuItem("Cut");
        myCut.setActionCommand("cut");
        myCut.addActionListener(this);
        myCut.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, keyMask));

        JMenuItem myCopy = new JMenuItem("Copy");
        myCopy.setActionCommand("copy");
        myCopy.addActionListener(this);
        myCopy.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C, keyMask));

        JMenuItem myPaste = new JMenuItem("Paste");
        myPaste.setActionCommand("paste");
        myPaste.addActionListener(this);
        myPaste.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, keyMask));

        JMenuItem myFind = new JMenuItem("Find...");
        myFind.setActionCommand("find");
        myFind.addActionListener(this);
        myFind.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F, keyMask));

        JMenuItem myNext = new JMenuItem("Find Next");
        myNext.setActionCommand("next");
        myNext.addActionListener(this);
        myNext.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G, keyMask));

        JMenuItem myReplace = new JMenuItem("Replace");
        myReplace.setActionCommand("replace");
        myReplace.addActionListener(this);
        myReplace.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, keyMask));

        JMenuItem mySelect = new JMenuItem("Select Expression");
        mySelect.setActionCommand("select expression");
        mySelect.addActionListener(this);
        mySelect.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_E, keyMask));

        myEditMenu.add(myCut);
        myEditMenu.add(myCopy);
        myEditMenu.add(myPaste);
        myEditMenu.addSeparator();
        myEditMenu.add(myFind);
        myEditMenu.add(myNext);
        myEditMenu.add(myReplace);
        myEditMenu.add(mySelect);

        // Set up Menu Bar
        myMenuBar.add(myFileMenu);
        myMenuBar.add(myEditMenu);
        thisFile.setJMenuBar(myMenuBar);
	/* BWP END */ // end menu stuff

        filePane = new CodePane(null, null, statusBar, fontSize);
        pane = filePane.pane;
        System.out.println("NyquistFile creation, this " + this + " pane = " + pane);
        doc = filePane.doc;

        getContentPane().add(filePane);
        statusBar.setPreferredSize(new Dimension(200, 8));
        getContentPane().add(statusBar, java.awt.BorderLayout.SOUTH);
        statusBar.setText("");

        if (file != null) {
            readFile(file);
            determineType();
        }
        
	/////////////// BWP BEGIN
	// Right-Click Menu for the Document Window
	JPopupMenu myContext = new JPopupMenu();

	JMenuItem contextCut = new JMenuItem("Cut");
	contextCut.setActionCommand("context cut");
	contextCut.setName("context cut");
	contextCut.addActionListener(this);

	JMenuItem contextCopy = new JMenuItem("Copy");
	contextCopy.setActionCommand("context copy");
	contextCopy.setName("context copy");
	contextCopy.addActionListener(this);

	JMenuItem contextPaste = new JMenuItem("Paste");
	contextPaste.setActionCommand("context paste");
	contextPaste.setName("context paste");
	contextPaste.addActionListener(this);

	JMenuItem contextFind = new JMenuItem("Find");
	contextFind.setActionCommand("context find");
	contextFind.setName("context find");
	contextFind.addActionListener(this);

	JMenuItem contextReplace = new JMenuItem("Replace");
	contextReplace.setActionCommand("context replace");
	contextReplace.setName("context replace");
	contextReplace.addActionListener(this);

	JMenuItem contextSelect = new JMenuItem("Select Expression");
	contextSelect.setActionCommand("context select");
	contextSelect.setName("context select");
	contextSelect.addActionListener(this);

	myContext.add(contextCut);
	myContext.add(contextCopy);
	myContext.add(contextPaste);
	myContext.addSeparator();
	myContext.add(contextFind);
	myContext.add(contextReplace);
        myContext.add(contextSelect);

	MouseListener popupListener = new PopupListener(myContext, pane);
	pane.addMouseListener(popupListener);

        //////////////// BWP END

        setLocation(50+10, 50+10);
        setSize(600, 500);
        setBackground(Color.white);
        setResizable(true);
        setVisible(true);
        this.setClosable(true);
        this.setMaximizable(true);
        this.setIconifiable(true);
	setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
        
        System.out.println("Adding component listener");
        filePane.addComponentListener(this);

        doc.addDocumentListener(
            new DocumentListener() {
                public void changedUpdate(DocumentEvent e) {
                }
                public void removeUpdate(DocumentEvent e) {
                    //filePane.synchronousUpdate(null);
                    if (!modified) {
                        modified = true;
                        thisFile.setTitle(thisFile.getTitle() + "*");
                    }
                }
                public void insertUpdate(DocumentEvent e) {
		    // System.out.println("insertUpdate calls thread.update");
                    //filePane.synchronousUpdate(null);
                    if (!modified) {
                        modified = true;
                        thisFile.setTitle(thisFile.getTitle() + "*");
                    }
                }
            });
	//revertTo = pane.getText(); // BWP
    }
    
    public void doFileSave() {
        if (file != null) save(file.getParent());
        else saveAs("");
    }
    
    //// BWP BEGIN
    public void actionPerformed(ActionEvent e)
    {
	//System.out.println(e.getActionCommand());

	// File Menu options
        if (e.getActionCommand().equals("load")) {
            doFileSave();
            myParent.loadFile(file);
        } else if (e.getActionCommand().equals("save")) {
                doFileSave();
            } else if (e.getActionCommand().equals("save as")) {
                if (file != null) saveAs(file.getParent());
                else saveAs("");
            } else if (e.getActionCommand().equals("revert")) {
                if (modified && file != null) {
                    readFile(file);
                    myParent.loadFile(file);
                    //pane.setText(revertTo);
                    modified = false;
                    thisFile.setTitle(thisFile.getTitle().substring(0, 
                            thisFile.getTitle().length() - 1));
                }
            } else if (e.getActionCommand().equals("close")) {
                int input = 1;

                if (modified) 
                    input = JOptionPane.showConfirmDialog(null, 
                                    "Close without saving?", "Closing...", 
                                    JOptionPane.YES_NO_OPTION);
                else this.dispose();

                if (input == 0) this.dispose();
            // Edit Menu options
            } else if (e.getActionCommand().equals("cut")) {
                pane.cut();
            } else if (e.getActionCommand().equals("copy")) {
                pane.copy();
            } else if (e.getActionCommand().equals("paste")) {
                pane.paste();
            } else if (e.getActionCommand().equals("find")) {
                FindDialog findDialog = new FindDialog(thisFile, myParent);
            } else if (e.getActionCommand().equals("next")) {
                if (!lastFound.equals("")) find(lastFound);
            } else if (e.getActionCommand().equals("replace")) {
                ReplaceDialog replaceDialog = new ReplaceDialog(thisFile, myParent);
        } else if (e.getActionCommand().equals("select expression")) {
            myParent.doEditSelectExpression(e);
            // Context Menu Options
            } else if (e.getActionCommand().equals("context cut"))	{
                pane.cut();
            } else if (e.getActionCommand().equals("context copy")) {
                pane.copy();
            } else if (e.getActionCommand().equals("context paste")) {
                pane.paste();
            } else if (e.getActionCommand().equals("context find")) {
                FindDialog findDialog = new FindDialog(thisFile, myParent);
            } else if (e.getActionCommand().equals("context replace")) {
                ReplaceDialog replaceDialog = new ReplaceDialog(thisFile, myParent);
            } else if (e.getActionCommand().equals("context select")) {
            myParent.doEditSelectExpression(e);
        }
        //// BWP END
    }

    public void componentResized(ComponentEvent e) {
        System.out.println(e.getComponent().getClass().getName() + 
                           " --- Resized ");
        filePane.synchronousUpdate();
    }
    
    public void componentHidden(ComponentEvent e) {
        System.out.println(e.getComponent().getClass().getName() + 
                           " --- Hidden ");            
    }
  
    public void componentMoved(ComponentEvent e) {
        System.out.println(e.getComponent().getClass().getName() + 
                           " --- Moved ");            
    }
    

    public void componentShown(ComponentEvent e) {
        System.out.println(e.getComponent().getClass().getName() + 
                           " --- Shown ");            
    }
    

    String currentSelection() {
        System.out.println("currentSelection: this " + this + " pane " + pane);
	int p = pane.getSelectionStart();
	int q = pane.getSelectionEnd();
	try {
	    return doc.getText(p, q - p);
	} catch(Exception e) {
	    System.out.println(e);
	    return "";
	}
    }
    
    public void setCursor(int line, int col) {
        try { // because getText raises an exception BadLocationException
            String text = doc.getText(0, doc.getLength());
            int i = 0;
            int j = 0;
            while (j < line - 1) 
                if (text.charAt(i++) == '\n') j++;
            final int pos = i + col - 1;
            EventQueue.invokeLater(
                new Runnable() {
                    public void run() {
                        System.out.println("Automatically move caret to " + pos);
                        pane.select(pos, pos);
                    }
                });
        } catch(Exception e) {
            System.out.println(e);
        }
    }
    

    public void selectExpression() {
	int docLength;
	String text;
	try {
	    docLength = doc.getLength();
	    text = doc.getText(0, docLength);
	} catch (Exception e) { 
	    System.out.println(e); return;
	}
	int p = pane.getSelectionStart();
	int openParenLoc = CodePane.findOpenParen(text, p);
	if (openParenLoc == p) return;
	if (openParenLoc < 0) return; // no open paren enclosing start of selection
	// now findx is the beginning of the expression and closed == 0
	// go forward from p to find close paren
	
	int closed = 0;
	while (p < text.length() && closed < 1) {
	    char c = text.charAt(p);
	    if (c == '(' || c == ')') {
		if (!CodePane.inComment(text, p)) {
		    if (c == '(') closed--;
		    else if (c == ')') closed++;
		}
	    }
	    if (closed < 1) // not done, keep going
		p = forward(text, p, false);
	}
	
	pane.select(openParenLoc, p + 1); // add one to include close paren
    }
    
    
    private static int forward(String text, int pos, boolean inString)
	// find an index in text after pos by skipping over strings and
	// escaped characters of the form #\A, also skip comment
	// lines. If pos is text.length() - 1, return text.length()
    {
	boolean comment = false;
	boolean string = false;
        while (pos < text.length()) {
	    char c = text.charAt(pos);
	    if (comment) {
		if (c == '\n') {
		    comment = false;
		}
	    } else if (string) {
		if (c == '"') { // skip string
		    string = false;
		}
	    } else if (c == '\\') { // skip quoted char
		pos++;
	    } else if (c == '"') {
		string = true;
	    } else if (c == ';') {
		comment = true;
	    }
	    pos++;
	    if (!comment && !string) return pos;
	}
	return pos;
    }


    public boolean find(String pat) {
	String docText = "";
	int start = pane.getSelectionEnd();
	//System.out.print("location ");
	//System.out.println(start);
	try {
	    docText = doc.getText(0, doc.getLength());
	} catch(Exception e) {System.out.println(e);}

	//System.out.print(docText);
        int found = docText.indexOf(pat, start);
	if (found == -1) {
	    found = docText.indexOf(pat, 0);
        }
        if (found == -1) return false;
	pane.select(found, found + pat.length());
	return true;
    }

    public String copy(boolean a, boolean b)
    {
	String selectedText = pane.getSelectedText();
        pane.copy();
	return selectedText;
    }

    public boolean paste(String text) 
    {
	pane.replaceSelection(text);
	return true;
    }

    public String getAbsolutePath()
    {
	return file.getAbsolutePath();
    }

    public boolean save(String currentDir)
    // saves the file if there is a file name, otherwise calls saveAs.
    // returns false if the operation fails or is cancelled.
    // returns true if save succeeds or if file is unmodified.
    {
        if (modified) {
            if (file == null)
                return saveAs(currentDir);
            else {
                try {
                    long length = file.length();
                    long newLength = doc.getLength();
                    System.out.println("existing " + length + " new " + newLength);
                    String msg = null;
                    if (length > 0 && newLength == 0) {
                        msg = "Replace existing file with an empty document?";
                    } else if (length > newLength * 2) {
                        msg = "Replace file with new document that is less than 1/2 the existing size?";
                    }
                    if (msg != null) {
                        int n = JOptionPane.showConfirmDialog(this, msg, "WARNING",
                                                              JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
                        if (n == JOptionPane.NO_OPTION) return false;
                    }

                    FileOutputStream saveFileStream = 
                        new FileOutputStream(file);

                    String fileStr = doc.getText(0, doc.getLength());
                    saveFileStream.write(fileStr.getBytes());
                    saveFileStream.flush();
                }
                catch(Exception e) { 
		    System.out.println(e);
		    return false; // did not save file
		}
                
                modified = false;
                thisFile.setTitle(thisFile.getTitle().substring(
                    0, thisFile.getTitle().length() - 1));
            }
        }
	//revertTo = pane.getText();
	return true;
    }
    
    public boolean saveAs(String currentDir) {
	// select a file and write to it. Return true if success.
        JFileChooser chooser = new JFileChooser();
        LispFileFilter filter = new LispFileFilter();
        SalFileFilter salFilter = new SalFileFilter();
        String name; // new file name
        chooser.setFileFilter(filter);
        chooser.setFileFilter(salFilter);

	File curdir = new File(currentDir);
	chooser.setCurrentDirectory(curdir);
        
        while (true) { // loop until file is chosen
            int returnVal = chooser.showSaveDialog(this);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                file = chooser.getSelectedFile();
                name = file.getName();
                System.out.println("You chose to save this file: " + name);
                String lower = name.toLowerCase();
                if (lower.endsWith(".sal") ||
                    lower.endsWith(".lsp")) break;
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
        setTitle(name + "*");
        determineType();
        modified = true;
        return save(currentDir);
    }
    
}
