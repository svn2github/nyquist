//
//  CodePane.java
//  nyquist
//
//  Created by Roger Dannenberg on 12/15/07.
//  Copyright 2007 Roger B. Dannenberg. All rights reserved.
//

package jnyqide;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.undo.*;

// CodePane subclasses a JScrollPane and initializes it to contain
// a JTextPane which holds a DefaultStyleDocument. Syntax coloring
// and paren balancing is performed.
//
// a CodePane also has a Timer -- it is used to blink a character somewhere.
//
public class CodePane extends JScrollPane 
                      implements DocumentListener, CaretListener, 
                                 AdjustmentListener, KeyListener {
    public JTextPane pane;
    public DefaultStyledDocument doc;
    public Timer timer;
    public boolean evenParens; // used by MainFrame for command input
        // (when user types return, if parens are balanced, that is, evenParens
        // is true, then the command is sent to Nyquist)
    // when user types a close paren, the matching one is blinked, and 
    // when user moves cursor, the matching paren is highlighted in bold
    public int blinkLoc; // where to blink a matching paren
    public int highlightLoc; // where to highlight a matching paren
    public boolean blinkOn;
    // mainFrame is a handle to access some methods, but it is also a
    // flag to tell us if this is a command entry window:
    public MainFrame mainFrame;
    public boolean isSal;
    int caretLine;  // holds current line number
    int caretColumn; // holds current column
    int fontSize; // the font size
    JLabel statusBar;
    
    final int TIMER_DELAY = 1000; // ms
    
    public CodePane(Dimension preferredSize, MainFrame mf, JLabel sb, 
                    int fontSz) {
        super();
        blinkLoc = 0;
        blinkOn = false; // initialize
        fontSize = fontSz;
        mainFrame = mf;
        statusBar = sb;
        isSal = false;
        doc = new DefaultStyledDocument();
        pane = new JTextPane(doc);
        getViewport().add(pane);
        setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        
        // create timer for blinking
        ActionListener blinkOffTask = new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                timer.stop(); // timer is just a one-shot, not periodic
                blinkOn = false;
                synchronousUpdate();
            }
        };
        timer = new Timer(TIMER_DELAY, blinkOffTask);
        
        doc.addDocumentListener(this);
        pane.addCaretListener(this);
        this.getHorizontalScrollBar().addAdjustmentListener(this);
        this.getVerticalScrollBar().addAdjustmentListener(this);
        
        final UndoManager undo = new UndoManager();
        doc.addUndoableEditListener(
            new UndoableEditListener() {
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
        
        // add self as a key listener
        pane.addKeyListener(this);
        

        if (preferredSize != null) setPreferredSize(preferredSize);

    }
    
    
    public void updateCaretLoc() {
        String text;
        try {
            text = pane.getText();
        } catch(Exception ex) {
            System.out.println("exception in keyTyped");
            return;
        }
        int pos = pane.getCaretPosition();
        caretColumn = 0;
        caretLine = 0;
        int i = 0;
        while (i++ < pos) {
            if (text.charAt(i - 1) == '\n') {
                caretColumn = 0;
                caretLine++;
            } else {
                caretColumn++;
            }
        }
        // the first line is 1, not 0
        statusBar.setText(Integer.toString(caretLine + 1) + ":" + 
                          Integer.toString(caretColumn));
    }


    public void changedUpdate(DocumentEvent e) {
        //System.out.println("changedUpdate " + e);
    }
    
    
    public void removeUpdate(DocumentEvent e) {
        synchronousUpdate();
    }
    
    
    public void insertUpdate(DocumentEvent e) {
        // System.out.println("insertUpdate calls thread.update");
        synchronousUpdate();
    }


    public void caretUpdate(CaretEvent e) {
        // System.out.println("caretUpdate " + e.getDot() + " mark " +
        //                       e.getMark());
        if (e.getDot() == e.getMark()) {
            highlightMatchingParen(e.getDot() - 1);
            updateCaretLoc();
            //System.out.println("caretUpdate: " + e.getDot() + " is line " +
            //                   caretLine + " col " + caretColumn);
        } else {
            highlightMatchingParen(-1);
        }
    }
    
    public void adjustmentValueChanged(AdjustmentEvent evt) {
        // System.out.println("adjustmentValueChanged");
        synchronousUpdate();
    }


    //This method can be invoked from any thread.  It 
    //invokes the setText and modelToView methods, which 
    //must run in the event dispatching thread. We use
    //invokeLater to schedule the code for execution
    //in the event dispatching thread.
    protected void highlightMatchingParen(final int pos) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    if (pos == -1) {
                        highlightLoc = pos;
                    } else if (charAtPos(pos) == ')') {
                        try {
                            highlightLoc = findOpenParen(
                                    pane.getText(0, pos), pos);
                        } catch(BadLocationException e) {
                            System.out.println(e);
                            return;
                        }
                        if (highlightLoc == pos) highlightLoc = -1;
                    } else { // not at close paren, so turn off highlight
                        highlightLoc = -1;
                    }
                    synchronousUpdate();
                }
            }
        );
    }
    
    char charAtPos(int pos) {
        String text;
        try {
            text = doc.getText(pos, 1);
        } catch(BadLocationException e) {
            return 0;
        }
        return text.charAt(0);
    }
    
    // complete the KeyListener interface:
    public void keyReleased(KeyEvent ke) { return; }
    public void keyPressed(KeyEvent ke) { return; }
    
    public void keyTyped(KeyEvent ke) {
        // System.out.println("CodePane keyTyped |" + 
        //                   ke.getKeyChar() + "|");
        // cases:
        //   newline
        //      mainFrame
        //         evenParens: sendCommandToNyquist
        //         !evenParens:
        //            LISP: print "paren mismatch"
        //            indent handling
        //      else
        //         indent handling
        //   otherwise
        //      process key
        //         
        // this is where we put auto parentheses:
        char ch = ke.getKeyChar();
        int pos = pane.getCaretPosition();
        if (ch == '\n') {
            //insertIndentation(p);
            if (mainFrame != null) {
                if (evenParens) {
                    if (doc.getLength() == pos) {
                        // only do indentation if newline is at end of text
                        insertIndentation(pos);
                        // remove newline
                        pos = doc.getLength(); // take out newline at end
                        int newlinePos = pos - 1;
                        try {
                            while (!doc.getText(newlinePos, 1).equals("\n")) {
                                newlinePos--;
                            }
                        } catch(BadLocationException e) {
                            System.out.println(e);
                            return;
                        }
                        System.out.println("cut newline: newlinePos " + newlinePos +
                                           " pos " + pos);
                        pane.setSelectionStart(newlinePos);
                    } else { // no indentation, just remove newline
                        pane.setSelectionStart(pos - 1);
                    }
                    pane.setSelectionEnd(pos);
                    pane.replaceSelection("");
                    mainFrame.sendCommandToNyquist();
                } else {
                    if (!isSal) {
                        mainFrame.jOutputArea.append(
                                "Invalid command - paren mismatch\n");
                    }
                    insertIndentation(pos);
                }
            } else { 
                insertIndentation(pos);
            }
        } else if (ch == '\t') {
            int spaces = TextColor.INDENT - 
                         (caretColumn - 1) % TextColor.INDENT;
            // remove tab and replace with spaces
            pane.setSelectionStart(pos - 1);
            pane.setSelectionEnd(pos);
            pane.replaceSelection("        ".substring(0, spaces));
            System.out.println("Replaced TAB by " + spaces + " spaces");
        } else { // ordinary key handling
            if (ke.getKeyChar() == ')') {
                // get caret location
                blinkParen(pos);
            } else if (ch == '(') {
                if (MainFrame.prefParenAutoInsert) {
                    pane.replaceSelection(")");
                    pane.setCaretPosition(pos);
                }
            }
            String text;
            int start = Math.max(pos - 100, 0);
            try {
                text = pane.getText(start, pos - start);
            } catch (Exception ex) {
                System.out.println("exception in keyTyped: start " + start + 
                                   " pos " + pos + " pane " + pane);
                return;
            }
            // simulate typing the character unless it's a backspace
            if (ch != '\b') text += ch;
            else pos--;
            
            String identifier;
            int identLoc;
            boolean forceExact = false; // force an exact match to complete word
            if (isSal) { // generate completion list for SAL
                // delimiters are "{}(),[]\n #\""
                // look back for identifier immediately before pos
                // if none found, look back for unbalanced paren, then search
                // back from there
                identifier = getSalIdentifier(text, pos - start);
                int len = identifier.length();
                identLoc = pos - len;
                forceExact = (len > 0 && identifier.charAt(len - 1) == ' ');
                if (len == 0) { // not found
                    int openParenLoc = findOpenParen(text, pos - start);
                    identifier = getSalIdentifier(text, openParenLoc);
                    identLoc = start + openParenLoc - identifier.length();
                    forceExact = true;
                }
            } else { // generate completion list for Lisp
                // look back for unmatched open paren, then forward for identifier
                // look back a maximum of 100 characters
                int openParenLoc = findOpenParen(text, pos - start);
                openParenLoc++;
                identifier = getIdentifier(text, openParenLoc);
                identLoc = start + openParenLoc;
                int len = identifier.length();
                forceExact = (len > 0 && identifier.charAt(len - 1) == ' ');
            }
            //System.out.println("keyTyped identifier is: |" + identifier + "|");
            // put up words list
            WordList.printList(identifier, pane, identLoc, pos + 1, 
                               forceExact, isSal);
        }
    }

    
    // get an identifier starting at pos -- if a complete identifier (terminated by something)
    // is found, the identifier is terminated by a space character
    static String getIdentifier(String text, int pos) {
	int idEnd = pos; // search forward to find identifier
	String lispIdChars = "~!@$%^&*-_+={}|:<>?/";
        if (text.length() == 0 || pos < 0) return text; // special cases
	while (true) {
	    if (idEnd == text.length()) {
		text = text.substring(pos); // still typing
		break;
	    }
	    char idChar = text.charAt(idEnd);
	    if (!Character.isLetterOrDigit(idChar) &&
		lispIdChars.indexOf(idChar) == -1) {
		text = text.substring(pos, idEnd) + " "; // complete
		break; // idEnd is one past last character
	    }
	    idEnd++;
	}
	return text;
    }
    
    
    static int findColumnOf(String text, int pos) {
	int col = 0;
	pos--;
	while (pos >= 0 && text.charAt(pos) != '\n') {
	    col++;
	    pos--;
	}
	return col;
    }
    
    
    private void insertIndentation(int p)
    {
        String text;
        int desired = 0; // desired indentation of the previous line
            // initialized because compiler can't figure out that it's
            // initialized below before it is used
        try {
            text = pane.getText(0, p);
        } catch (Exception e) {
            System.out.println("exception in insertIndentation");
            return;
        }
        int indent;
        if (isSal) {
            indent = salIndentAmount(p);
            desired = TextColor.indentBefore;
        } else {
            indent = autoIndentAmount(text, p);
        }
        String indentation = "";
        while (indent > 0) {
            indentation += " ";
            indent--;
        }
        // System.out.println("before replaceSelection(indentation)");
        pane.replaceSelection(indentation);
        // System.out.println("after replaceSelection(indentation)");
        if (isSal) { // indent the previous line as well
            // first find the beginning of the previous line
            int prevStart = p - 1; // index of newline
            // System.out.println("prevStart " + prevStart + " char |" + 
            //                    text.charAt(prevStart) + "|");
            assert(text.charAt(prevStart) == '\n');
            while (prevStart - 1 >= 0 && 
                   text.charAt(prevStart - 1) != '\n') prevStart--;
            // System.out.println("PREV LINE BEGIN " + prevStart + " in |" + 
                                // text + "|");
            // find the actual indentation of the previous line
            int prevIndent = 0; // search forward from prevStart for nonspace
            while (text.charAt(prevStart + prevIndent) == ' ' ||
                   text.charAt(prevStart + prevIndent) == '\t') 
                prevIndent++;
            // System.out.println("PREV INDENT " + prevIndent + 
            //                    " DESIRED " + desired);
            // adjust the indentation
            int delta = desired - prevIndent;
            p = pane.getSelectionStart() + delta;
            if (delta > 0) {
                indentation = "";
                while (delta > 0) {
                    indentation += " ";
                    delta--;
                }
                // System.out.println("INSERT " + delta + 
                //                    " SPACES AT " + prevStart);
                pane.setSelectionStart(prevStart);
                pane.setSelectionEnd(prevStart);
                pane.replaceSelection(indentation);
            } else if (delta < 0) {
                // System.out.println("BACKSPACE " + -delta + 
                //                    " AT " + prevStart);
                pane.setSelectionStart(prevStart);
                pane.setSelectionEnd(prevStart - delta);
                pane.replaceSelection("");
            }
            // System.out.println("MOVE CARET TO " + p);
            pane.setSelectionStart(p);
            pane.setSelectionEnd(p);
        }
    }
    
    private int salIndentAmount(int p) {
        // how much is the default indentation?
        // p is position AFTER a newline, so back up one to get
        // the index of the newline character
        // System.out.println("salIndentAmount " + p);
        TextColor.format(this, p - 1);
        // System.out.println("salIndent return " + TextColor.indentAfter);
        return TextColor.indentAfter;
    }
    

    // find auto-indent position:
    // first, go back and find open paren that would match a close paren
    // second search forward to find identifier
    // if identifier is defun, defmacro, let, let*, or prog, etc.
    // indent to paren posn + 2
    // else indent to the first thing after the identifier
    int autoIndentAmount(String text, int pos) {
	int openParenLoc = findOpenParen(text, pos);
	// System.out.println("autoIndent: openParenLoc = " + openParenLoc);
	if (openParenLoc == -1) return 0;
	String ident = getIdentifier(text, openParenLoc + 1);
	if (ident.equals("defun ") || ident.equals("defmacro ") || 
            ident.equals("let ") || ident.equals("let* ") ||
            ident.equals("dotimes ") || ident.equals("dolist ") ||
            ident.equals("simrep ") || ident.equals("seqrep ") ||
	    ident.equals("prog ") || ident.equals("prog* ") ||
            ident.equals("progv ")) {
	    pos = openParenLoc + 2;
	} else {
	    pos = openParenLoc + ident.length();
	    System.out.println("auto-indent, pos " + pos + ", ident " + ident + 
			       ", length " + ident.length());
	    while (pos < text.length() && Character.isWhitespace(text.charAt(pos))) {
		if (text.charAt(pos) == '\n') {
		    // if the end of the line looks like "(foo \n" then the tab position
		    // will be indented two from the open paren (ignore the identifier):
		    pos = openParenLoc + 2;
		    break;
		}
		pos++;
	    }
	    // System.out.println("pos " + pos);
	}
	return findColumnOf(text, pos);
    }
    
    
    public static boolean inComment(String text, int pos) 
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
    
    static String SalIdChars = "{}(),[]\n #\"";
	
    public static String getSalIdentifier(String docText, int pos) {
        // System.out.println("|" + docText + "| " + pos);
        int start = pos;
        if (pos < 0) return ""; // special case: no place to search from
        // allow back up over single open paren
        if (docText.charAt(pos) == '(') start = start - 1;
        while (start >= 0 && 
               SalIdChars.indexOf(docText.charAt(start)) == -1) {
            start--;
        }
        // protect from bogus arguments
        if (start < -1 || pos >= docText.length()) return "";
        // if id is terminated by open paren, substitute blank so that
        // when we search lisp-syntax wordlist we get a more precise match.
        // E.g. "osc(" becomes "osc " which will not match "osc-enable ..."
        if (docText.charAt(pos) == '(') 
            return docText.substring(start + 1, pos) + " ";
        else
            return docText.substring(start + 1, pos + 1);
        
        
    }
        
        
    public static int findOpenParen(String docText, int pos) {
	int findx = -1;
	try {
	    boolean inString = false; // need to get it from text color
	    findx = backup(docText, pos, inString);
	    if (findx == pos - 1 && findx > 0 && docText.charAt(findx) == '\\') { 
                // escaped paren
		return pos;
	    } else if (inComment(docText, findx)) { // in comment
		return pos;
	    }
	    // at this point we know there is a closed paren. 
	    // go back until you find the matching open paren.
	    int closed = 1;
	    while (findx >= 0 && closed > 0) {
		char c = docText.charAt(findx);
                if (c == '(' || c == ')') {
		    if (!inComment(docText, findx)) {
			if (c == '(') closed--;
			else if (c  == ')') closed++;
		    }
		}
		if (closed > 0) // not done, back up
		    findx = backup(docText, findx, false);
	    }
        } catch( Exception e ) { 
	    // System.out.println("findOpenParen " + e); 
	}
	// System.out.println("findOpenParen returns " + findx);
	return findx;
    }
    
    
    private static int backup(String text, int pos, boolean inString)
    // find an index in text before pos by skipping over strings
    // and escaped characters of the form #\A, but do not consider
    // comment lines. If findx is zero, return result is -1.
    {
	int findx = pos - 1;
	while (true) {
	    if (findx < 0) {
		    return findx;
		}
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
		  // may be that the coloring hasn't run yet
		  // and all will become well.
	    }
	    if (!inString || findx <= 0) {
  		    return findx;
		}
            findx--;
	}
    }
	
	  
    void synchronousUpdate() {
        // System.out.println("synchronousUpdate called");
        final CodePane codePane = this;
        final JViewport v = getViewport();
        final Point pt = v.getViewPosition();
        final Dimension e = v.getExtentSize();
        EventQueue.invokeLater(
            new Runnable() {
                public void run() {
                    // System.out.println("calling TextColor");
                    codePane.evenParens = TextColor.format(codePane, 0);
                    // System.out.println("returned from TextColor");
                    codePane.updateFontSize(fontSize);
                }
            });
    }
    
    void blinkParen(int pos) {
        try {
            String docText = doc.getText(0, pos);
            int openParenLoc = findOpenParen(docText, pos);
            if (openParenLoc >= 0 && openParenLoc < pos) {
                blink(openParenLoc);
            }
        } catch (Exception e) { System.out.println(e); }
    }
                
    
    // the blink interface: call blink(loc) to make a character blink
    void blink(int loc) {
        timer.start();
        blinkOn = true;
        blinkLoc = loc;
    }

    public void updateFontSize(int size) {
        final MutableAttributeSet attributeSet = new SimpleAttributeSet();
        StyleConstants.setFontSize(attributeSet, size);
        doc.setCharacterAttributes(0, 1000000, attributeSet, false);
    }

    public void setFontSize(int size) { 
        fontSize = size; 
        updateFontSize(size);
    }

}
