package jnyqide;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.text.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.util.*;
import java.io.*;

public class TextColor {

    public static SimpleAttributeSet attrBlack = new SimpleAttributeSet();
    public static SimpleAttributeSet attrGreen = new SimpleAttributeSet();
    public static SimpleAttributeSet attrBlue = new SimpleAttributeSet();
    public static SimpleAttributeSet attrBrown = new SimpleAttributeSet();
    public static SimpleAttributeSet attrRed = new SimpleAttributeSet();
    public static SimpleAttributeSet attrBlink = new SimpleAttributeSet();

    public static Set keywords = new HashSet();

public static void init() {
    System.out.println("initializing TextColor.java");
    try {
	BufferedReader inf = new 
	    BufferedReader(new FileReader("jnyqide/keywords.txt"));
	String word;
	while ((word = inf.readLine()) != null) {
	    keywords.add(word.toUpperCase());
	}
	inf.close();
    } catch (IOException e) {
	System.out.println("Error: could not open keywords.txt");
    }
    
    // Note: there's probably a way to make all of these attributes "inherit" the
    // font family and size, but I don't know how (maybe you can copy a template
    // and color it?)

    attrBlack.addAttribute(StyleConstants.ColorConstants.Foreground, 
			   Color.black);
    StyleConstants.setFontFamily(attrBlack, "Courier");
    StyleConstants.setFontSize(attrBlack, 12);

    attrGreen.addAttribute(StyleConstants.ColorConstants.Foreground, 
			   new Color(0, 100, 0));
    StyleConstants.setFontFamily(attrGreen, "Courier");
    StyleConstants.setFontSize(attrGreen, 12);

    attrBlue.addAttribute(StyleConstants.ColorConstants.Foreground,Color.blue);
    StyleConstants.setFontFamily(attrBlue, "Courier");
    StyleConstants.setFontSize(attrBlue, 12);

    attrBrown.addAttribute(StyleConstants.ColorConstants.Foreground,
			   new Color(130, 140, 0));
    StyleConstants.setFontFamily(attrBrown, "Courier");
    StyleConstants.setFontSize(attrBrown, 12);

    attrRed.addAttribute(StyleConstants.ColorConstants.Foreground,
			 new Color(250, 0, 0));
    StyleConstants.setFontFamily(attrRed, "Courier");
    StyleConstants.setBold(attrRed, true);

    attrBlink.addAttribute(StyleConstants.ColorConstants.Background,
			   Color.black);
    attrBlink.addAttribute(StyleConstants.ColorConstants.Foreground,
			   Color.white);
    StyleConstants.setFontFamily(attrBlink, "Courier");
}


public static boolean format(JTextPane pane, 
			     DefaultStyledDocument doc, 
			     SyntaxThread thread, int x, int y,
			     int width, int height)
{
    //System.out.println("format begin");
    int start = pane.viewToModel(new Point(x, y));
    //System.out.print("text loc for origin: "); 
    //System.out.println(start);
    int end = pane.viewToModel(new Point(x + width, y + height));
    //System.out.print("text loc for bottom right: "); 
    //System.out.println(end);
    
    boolean b = formatVisible(doc, start, end, thread);
    //System.out.println("format end");
    return b;
}

/*
public static void formatLimited(DefaultStyledDocument doc, int pos1, int pos2, SyntaxThread thread)
{
  int start1 = pos1, end1 = pos1+1;
  int start2 = pos2, end2 = pos2+1;
  try {
  String docText = doc.getText(0, doc.getLength());

  while( start1 > 0 && (docText.charAt(start1-1) != '\n' ))
    start1--;
  while( end1 < doc.getLength() && (docText.charAt(end1-1) != '\n' ))
    end1++;

  while( start2 > 0 && (docText.charAt(start2-1) != '\n' ))
    start2--;
  while( end2 < doc.getLength() && (docText.charAt(end2-1) != '\n' ))
    end2++;

  formatVisible(doc, start1, end2, thread);

  } catch( Exception e ) { System.out.println(e); }
}
*/

public static void setColor(DefaultStyledDocument doc, int start, int len, 
			    SimpleAttributeSet color)
{
    
    //    if (color == attrBlack) System.out.print("black");
    //    else if (color == attrBrown) System.out.print("brown");
    //    else if (color == attrRed) System.out.print("red");
    //    else if (color == attrBlue) System.out.print("blue");
    //    else if (color == attrGreen) System.out.print("green");
    //    else System.out.print("color");
    //    System.out.print(" from "); System.out.print(start);
    //    System.out.print(" len "); System.out.println(len);
    
    doc.setCharacterAttributes(start, len, color, true);
}


public static boolean formatVisible(DefaultStyledDocument doc, int start, 
			     int end, SyntaxThread thread)
// start and end give the visible range over which we actually 
// change the document.
{
    boolean evenParens = false;

    try {
	int docLength = doc.getLength();
        String docText = doc.getText(0, docLength);
        //System.out.println(docText);

	// To parse the file, we use a state machine with these states:
	//   comment -- in a comment started by ";"
	//   word -- in normal text, possibly a keyword
	//   hash -- we detected a hash mark (this state is implied)
	//   escape -- we detected a backslash (this state is implied)
	//   hashComment -- we're in a comment #| ... |#
	//   string -- in a string
	//   barSymbol -- in a symbol like |...|
	//   space -- in space between other tokens
	//   normal -- this state is orthogonal to the others.
	//      it records when we are accumulating a run of normal
	//      (black) characters. To make things run a little faster
	//      we don't color each black character individually but
	//      group them together into a run. It also helps at the end
	//      of the string to be able to color what's left.
	boolean normal = false;
	int normalStart = 0; // start of normal black characters
        boolean comment = false;
        int commentStart = 0;
        boolean word = false;
        int wordStart = 0;
        boolean string = false;
        int stringStart = 0;
	boolean hashComment = false;
	int hashCommentStart = 0;
	boolean barSymbol = false;
	int barSymbolStart = 0;
	boolean space = false;
	int spaceStart = 0;
        int pos;
        Stack parenStack = new Stack();
	// originally, this code accumulated words in buf, but this
	// caused many string allocations and ran very slowly, so now
	// we just mark where the word begins and when the word ends
	// we extract a string from docText
	
	
	// process document text while you have not reached the end
	// of the visible area and while there are unmatched parens
	// (i.e. the parenStack isn't empty, but stop if you reach
	// the end of the whole file.
	// 
	//System.out.println("begin parse loop");
        for (pos = 0; pos < docLength && (pos < end || !parenStack.empty()); 
	     pos++) {
            if (thread.needsUpdate)
                return false;
            char c = docText.charAt(pos);
	    /*
	    System.out.print("parse ");
	    System.out.print(c);
	    System.out.print(" at ");
	    System.out.print(pos);
	    System.out.print(" word ");
	    System.out.print(word);
	    System.out.print(" space ");
	    System.out.print(space);
	    System.out.print(" stack ");
	    System.out.println(parenStack);
	    */
	    if (comment) {
		if (c == '\n') {
		    if (pos >= start) {
			setColor(doc, commentStart, pos - commentStart, 
				     attrGreen);
		    }
		    comment = false;
		} // note that escape chars are ignored in comments
            } else if (string) {
		if (c == '"') {
		    if (pos >= start)
			setColor(doc, stringStart, pos - stringStart + 1, 
				     attrBrown);
		    string = false;
		} else if (c == '\\') { // skip the quoted char
		    pos++;
		}
	    } else if (word) {
		// see if the word ends because of a character or end of text
		boolean terminated = (c == ' ' || c == '(' || c == ')' || 
				      c == '\n' || c == '"');
		if (terminated || pos >= docLength - 1) {
		    word = false;
		    if (pos >= start) {
			int stop = pos;
			if (!terminated) stop++;
			String buf = docText.substring(wordStart, stop);
			if (isKeyword(buf)) {
			    setColor(doc, normalStart, wordStart - normalStart,
					 attrBlack);
			    setColor(doc, wordStart, pos - wordStart + 1, 
					 attrBlue);
			    normal = true;
			    normalStart = pos + 1;
			} else {
			    // continue normal (black) color
			}
		    }
		} else if (c == '\\') {
		    pos++; // skip the quoted char
		}
		if (terminated) pos = pos - 1; // reexamine character at pos
            } else if (hashComment) {
		if (c == '|' && pos + 1 < docLength && 
		    docText.charAt(pos + 1) == '#') {
		    pos++;
		    if (pos >= start) {
			if (normal) {
			    setColor(doc, normalStart, 
				     hashCommentStart - normalStart,
				     attrBlack);
			}
			setColor(doc, hashCommentStart, 
				     pos - hashCommentStart + 1, attrGreen);
		    }
		    normal = false;
		    hashComment = false;
		} else if (c == '\\') pos++; // skip the quoted char
	    } else if (barSymbol) {
		if (c == '|') {
		    if (pos >= start) {
			if (normal) {
			    setColor(doc, normalStart, wordStart - normalStart,
					 attrBlack);
			}
			normal = false;
			setColor(doc, wordStart, pos - wordStart, attrBlack);
		    }
		} else if (c == '\\') pos++; // skip the quoted char
            // Starts
	    } else if (c == ' ' || c == '\t' || c == '\n') {
		space = true;
		spaceStart = pos;
		if (!normal) normalStart = pos;
		normal = true;
	    } else if (c == ';') { // Comment Start
                comment = true;
                commentStart = pos;
		space = false;
		if (pos >= start && normal) {
		    setColor(doc, normalStart, pos - normalStart, attrBlack);
		}
		normal = false;
            } else if (c == '"') { // String start
                string = true;
                stringStart = pos;
		space = false;
		if (pos >= start && normal) {
		    setColor(doc, normalStart, pos - normalStart, attrBlack);
		}
		normal = false;
            } else if (c == '(' || c == ')') { // Paren start and end
		space = false;
		if (pos >= start && normal) {
		    setColor(doc, normalStart, pos - normalStart, attrBlack);
		}
		normal = false;
		if (pos >= start)
		    setColor(doc, pos, 1, attrBlack);
                if (c == '(' ) {
                    parenStack.push(new Integer(pos));
                } else {
                    if (parenStack.empty()) {
			if (pos >= start)
			    setColor(doc, pos, 1, attrRed);
		    } else {
			// System.out.println("pop stack");
                        parenStack.pop();
		    }
                }
		// there could be a race condition here, but failure
		// is not critical -- this just blinks the closing paren
		if (thread.blinkCount > 0 && pos == thread.blinkx) {
		    setColor(doc, pos, 1, attrBlink);
		}
	    } else if (c == '#') {
		space = false;
		if (pos >= start && normal) {
		    setColor(doc, normalStart, pos - normalStart, attrBlack);
		}
		normal = true;
		normalStart = pos;
		if (pos + 1 < docLength) {
		    char c2 = docText.charAt(pos + 1);
		    if (c2 == '\'' || c2 == 'x' || c2 == 'o' || c2 == 'b' ||
			c2 == ':') {
			// ignore all these: #' #x #o #b #:
			word = true;
			wordStart = pos;
			pos++;
		    } else if (c2 == '|') {
			hashComment = true;
			hashCommentStart = pos;
			pos++;
			normal = false;
		    } else {
			word = true;
			wordStart = pos;
		    }
		}
	    } else { // Word start
		word = true;
		wordStart = pos;
		if (!normal) normalStart = pos;
		normal = true;
		if (c == '\\' && pos + 1 < docLength) {
		    pos++; // parse the quoted char
		}
	    }
	    /*
	    System.out.print("At end of loop, pos ");
	    System.out.println(pos);
	    */
        } // END FOR LOOP
	//System.out.println("end parse loop");
	if (normal) {
	    setColor(doc, normalStart, pos - normalStart + 1, attrBlack);
	} else if (comment) {
	    setColor(doc, commentStart, pos - commentStart + 1, attrGreen);
	} else if (hashComment) {
	    setColor(doc, hashCommentStart, pos - hashCommentStart + 1,
			 attrGreen);
	} else if (string) {
	    setColor(doc, stringStart, pos - stringStart + 1, attrBrown);
	}

        evenParens = parenStack.empty();
        while( !parenStack.empty()) {
            int parenPos = ((Integer)parenStack.pop()).intValue();
	    if (pos >= start)
		setColor(doc, parenPos, 1, attrRed);
        }
    }
    catch(Exception e ) {System.out.println(e);}

    return evenParens;
}

private static boolean isKeyword(String in) {
    int i = 0;
    /*
    while (i < in.length()) {
	char c = in.charAt(i);
	if (c == ' ' || c == '\n' || c == '\t') i++;
	else break;
    }
    in = in.substring(i, in.length());
    */
    // System.out.println("isKeyword of " + in);
    return keywords.contains(in.toUpperCase());
}

}
