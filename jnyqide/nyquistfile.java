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

/* EXAMPLE CODE FOR UNDO

 JTextComponent textcomp = new JTextArea();
    final UndoManager undo = new UndoManager();
    Document doc = textcomp.getDocument();
    
    // Listen for undo and redo events
    doc.addUndoableEditListener(new UndoableEditListener() {
        public void undoableEditHappened(UndoableEditEvent evt) {
            undo.addEdit(evt.getEdit());
        }
    });
    
    // Create an undo action and add it to the text component
    textcomp.getActionMap().put("Undo",
        new AbstractAction("Undo") {
            public void actionPerformed(ActionEvent evt) {
                try {
                    if (undo.canUndo()) {
                        undo.undo();
                    }
                } catch (CannotUndoException e) {
                }
            }
       });
    
    // Bind the undo action to ctl-Z
    textcomp.getInputMap().put(KeyStroke.getKeyStroke("control Z"), "Undo");
    
    // Create a redo action and add it to the text component
    textcomp.getActionMap().put("Redo",
        new AbstractAction("Redo") {
            public void actionPerformed(ActionEvent evt) {
                try {
                    if (undo.canRedo()) {
                        undo.redo();
                    }
                } catch (CannotRedoException e) {
                }
            }
        });
    
    // Bind the redo action to ctl-Y
    textcomp.getInputMap().put(KeyStroke.getKeyStroke("control Y"), "Redo");

*/


public class NyquistFile extends JInternalFrame {
    DefaultStyledDocument doc;
    JTextPane pane;
    JScrollPane filePane;
    UndoManager undo;
    SyntaxThread thread;
    File file;
    public boolean modified = false;
    NyquistFile thisFile;
    
    public File getFile() {
	return file;
    }

    public void dispose() {
	// this method shuts down the syntax coloring thread and
	// then runs JInternalFrame's dispose to close the window.
	System.out.print("dispose called, thread ");
	System.out.println(thread);
	thread.running = false;
	super.dispose();
    }


    public NyquistFile() {
        this(null);
        modified = true;
        setTitle(getTitle()+"*");
    }
    
    public NyquistFile(File f) {
        super();
        
        if( f != null )
            setTitle(f.getName());
        else
            setTitle("Untitled");
        
        file = f;
        thisFile = this;
        
        doc = new DefaultStyledDocument();
        pane = new JTextPane( doc );
	pane.addKeyListener(new KeyAdapter() {
		public void keyTyped(KeyEvent ke) {
		    if (ke.getKeyChar() == ')') {
			// get caret location
			int pos = pane.getCaretPosition();
			thisFile.highlightParen(pos);
		    }
		}
	    });
        
        undo = new UndoManager();
    
	doc.addUndoableEditListener(new UndoableEditListener() {
                public void undoableEditHappened(UndoableEditEvent evt) {
		    // do not make style changes undoable -- these
		    // are fixed automatically
		    if (!evt.getEdit().getPresentationName().equals(
			    "style change")) {
			undo.addEdit(evt.getEdit());
		    }
		}
            });
    
        // Create an undo action and add it to the text component
	pane.getActionMap().put("Undo",
            new AbstractAction("Undo") {
                public void actionPerformed(ActionEvent evt) {
                    try {
                        if (undo.canUndo()) {
                            undo.undo();
                        }
                    } catch (CannotUndoException e) {
                    }
                }
           });
    
        // Bind the undo action to ctl-Z
        pane.getInputMap().put(KeyStroke.getKeyStroke("control Z"), "Undo");
    
        // Create a redo action and add it to the text component
        pane.getActionMap().put("Redo",
            new AbstractAction("Redo") {
                public void actionPerformed(ActionEvent evt) {
                    try {
                        if (undo.canRedo()) {
                            undo.redo();
                        }
                    } catch (CannotRedoException e) {
                    }
                }
            });
    
        // Bind the redo action to ctl-Y
        pane.getInputMap().put(KeyStroke.getKeyStroke("control Y"), "Redo");

	filePane = new JScrollPane( pane );
        filePane.setHorizontalScrollBarPolicy( 
            JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS );
        filePane.setVerticalScrollBarPolicy(
            JScrollPane.VERTICAL_SCROLLBAR_ALWAYS );
        getContentPane().add( filePane );
        
        if( file != null ) {
            try {
                FileInputStream openFileStream = new FileInputStream(file);
                
                byte b[] = new byte[10000]; // read 10K at a time
                int pos = 0;
                String fileStr = new String();
		//System.out.println("starting to read file");
                while( openFileStream.available() > 0 ) {
                    int count = openFileStream.read(b);
                    fileStr = fileStr + new String(b, 0, count);
                }
		//System.out.println("done reading file");
                doc.insertString(0, fileStr, TextColor.attrBlack);
            }
            catch( Exception e2 ) { System.out.println(e2); }
        }
        
        setLocation(50+10, 50+10);
        setSize(600, 500);
        setBackground(Color.white);
        setResizable(true);
        setVisible(true);
        this.setClosable(true);
        this.setMaximizable(true);
        this.setIconifiable(true);
	setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
        
        thread = new SyntaxThread(filePane, pane, doc);
        // TextColor.format(filePane, pane, doc, thread);
        doc.addDocumentListener(
            new DocumentListener() {
                public void changedUpdate(DocumentEvent e) {
                }
                public void removeUpdate(DocumentEvent e) {
                    thread.update();
                    if (!modified) {
                        modified = true;
                        thisFile.setTitle(thisFile.getTitle() + "*");
                    }
                }
                public void insertUpdate(DocumentEvent e) {
		    // System.out.println("insertUpdate calls thread.update");
                    thread.update();
                    if (!modified) {
                        modified = true;
                        thisFile.setTitle(thisFile.getTitle() + "*");
                    }
		    //int height = pane.getHeight() - 30;
		    //int width =  pane.getWidth() - 30;
		    /*
		    int height = filePane.getViewport().getExtentSize().height;
		    int width =  filePane.getViewport().getExtentSize().width;
		    int x = filePane.getViewport().getViewPosition().x;
		    int y = filePane.getViewport().getViewPosition().y;
		    System.out.print("viewport posn ");
		    System.out.print(x);
		    System.out.print(", ");
		    System.out.print(y);
		    System.out.print(", size ");
		    System.out.print(width);
		    System.out.print(", ");
		    System.out.println(height);
		    int loc = pane.viewToModel(new Point(x, y));
		    System.out.print("text loc for origin: "); 
		    System.out.println(loc);
		    loc = pane.viewToModel(new Point(x + width, y + height));
		    System.out.print("text loc for bottom right: "); 
		    System.out.println(loc);
		    */
                }
            });
        thread.start();
	thread.update();
    }
    
    private int backup(String text, int pos, boolean inString)
    // find an index in text before pos by skipping over strings
    // and escaped characters of the form #\A, but do not consider
    // comment lines. If findx is zero, return result is -1.
    {
	int findx = pos - 1;
	while (true) {
	    if (findx < 0) return findx;
	    char c = text.charAt(findx);
	    if (inString) {
		if (c == '"') {
		    // could it be escaped?
		    if (findx > 0) {
			char pre = text.charAt(findx - 1);
			if (pre == '"') findx--; // escaped as ""
			else if (pre == '\\') findx--; // escaped as \"
			else inString = false;
		    } else inString = false;
		} // else keep searching for string closing
	    } else { // not inString
		// test for escaped character
		if (findx > 0) {
		    char pre = text.charAt(findx - 1);
		    if (pre == '\\') findx--; // escaped
		    else inString = (c == '"');
		} // else if c == '"' then ...
		  // internal error: text begins with quote, but
		  // inString is false. Just ignore it because it
		  // may be that the coloring thread hasn't run yet
		  // and all will become well.
	    }
	    if (!inString || findx <= 0) 
		return findx;
            findx--;
	}
    }    

    public boolean inComment(String text, int pos) 
    // search back to newline for ";" indicating comment
    // assumes text[pos] is not escaped or in string
    {
	boolean inString = false;
	while (pos > 0) {
	    char c = text.charAt(pos);
	    if (c == ';') return true;
	    if (c == '\n') return false;
	    pos = backup(text, pos, false);
	}
	return false;
    }

    public void highlightParen(int pos)
    {
	try {
	    String docText = doc.getText(0, doc.getLength());
	    // System.out.print("highlightParen: "); System.out.println(pos);
	    if (doc == null) return;
	    boolean inString = false; // need to get it from text color
	    int findx = backup(docText, pos, inString);
	    if (findx == pos - 1 && docText.charAt(findx) == '\\') { 
                // escaped paren
		return;
	    } else if (inComment(docText, findx)) { // in comment
		return;
	    }
	    // at this point we know there is a closed paren. 
	    // go back until you find the matching open paren.
	    int closed = 1;
	    while (findx >= 0 && closed > 0) {
		char c = docText.charAt(findx);
		/*
		System.out.print("highlight, findx ");
		System.out.print(findx);
		System.out.print(", c ");
		System.out.print(c);
		System.out.print(", closed ");
		System.out.println(closed);
		*/
		if (c == '(' || c == ')') {
		    if (!inComment(docText, findx)) {
			if (c == '(') closed--;
			else if (c  == ')') closed++;
		    }
		}
		if (closed > 0) // not done, back up
		    findx = backup(docText, findx, false);
	    }
	    /*
            System.out.print("highlight matches at: "); 
	    System.out.println(findx);
	    */
	    thread.blink(findx);
	} catch( Exception e ) { System.out.println(e); }
    }

    public boolean find(String pat) 
    {
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
            if( file == null )
                return saveAs(currentDir);
            else {
                try {
                    FileOutputStream saveFileStream = 
                        new FileOutputStream( file );

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
	return true;
    }
    
    public boolean saveAs(String currentDir) {
	// select a file and write to it. Return true if success.
        JFileChooser chooser = new JFileChooser();
        LispFileFilter filter = new LispFileFilter();
        chooser.setFileFilter(filter);

	File curdir = new File(currentDir);
	chooser.setCurrentDirectory(curdir);
        
        int returnVal = chooser.showSaveDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            System.out.println("You chose to save this file: " +
                               chooser.getSelectedFile().getName());
            file = chooser.getSelectedFile();
            setTitle(file.getName()+"*");
            modified = true;
            return save(currentDir);
        } else {
	    return false;
	}
    }
}
