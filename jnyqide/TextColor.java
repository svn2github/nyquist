package jnyqide;


import javax.swing.*;
// import java.awt.*; -- includes List, which conflicts
import java.awt.Color;
import java.awt.event.*;
import java.awt.Point;
import java.awt.Dimension;
import javax.swing.text.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.util.*;
/*
import java.util.Iterator;
import java.util.List;
import java.util.HashSet;
import java.util.Set;
import java.util.Point;
*/
import java.io.*;

public class TextColor {
    static final boolean TCDEBUG = false;
    static final String TEXT_FONT = "Courier";
    static final int TEXT_SIZE = 12;
    static final Color COLOR_NORMAL = Color.black;
    static final Color COLOR_COMMENT = new Color(0, 100, 0); // darkish green
    static final Color COLOR_BUILTIN = Color.blue;
    static final Color COLOR_STRING = new Color(130, 140, 0); // brown
    static final Color COLOR_ERROR = new Color(250, 0, 0);
    static final Color COLOR_COMMAND = new Color(0, 0, 196); // darkBlue;
    static final Color COLOR_RESERVED = new Color(96, 0, 196); // purple
    static final Color COLOR_KEYWORD = new Color(255, 128, 128); // pink
    static final Color COLOR_HIGHLIGHT_FG = Color.black;
    static final Color COLOR_HIGHLIGHT_BG = Color.lightGray;
    static final Color COLOR_BLINK_FG = Color.white;
    static final Color COLOR_BLINK_BG = Color.black;
    
    // TextColor is set up to be non-reentrant, with some state saved
    // as static variables that can be referenced by helper functions
    // like setColor. For example, you call format to colorize the
    // visible portions of a window. It saves some state that setColor
    // (called from many places) can use to find the document, the
    // visible region, etc. When format returns, TextColor is ready
    // for another call to fix another window.
    
    static DefaultStyledDocument doc; // the document being colorized
    static int start;  // the start of the visible region
    static int end;    // the end of the visible region
    static int indentPos;   // where we want indent information
    static int indentBefore; // special return values for editing
    static int indentAfter;

    public static SimpleAttributeSet attrNormal, attrComment, attrBuiltin, 
            attrString, attrError, attrCommand, attrKeyword, attrReserved,
            attrHighlight, attrBlink;
    
    private static SimpleAttributeSet newTextAttr(Color color) {
        SimpleAttributeSet attr = new SimpleAttributeSet();
        attr.addAttribute(StyleConstants.ColorConstants.Foreground, color);
        StyleConstants.setFontFamily(attr, TEXT_FONT);
        StyleConstants.setFontSize(attr, TEXT_SIZE);
        return attr;
    }
    
    private static SimpleAttributeSet newTextHighlight(Color fg, Color bg) {
        SimpleAttributeSet attr = new SimpleAttributeSet();
        attr.addAttribute(StyleConstants.ColorConstants.Foreground, fg);
        attr.addAttribute(StyleConstants.ColorConstants.Background, bg);
        return attr;
    }

    // public static Set<String> keywords = new HashSet<String>();

    public static void init() {
        // System.out.println("initializing TextColor.java");
        attrNormal = newTextAttr(COLOR_NORMAL);
        attrComment = newTextAttr(COLOR_COMMENT);
        attrBuiltin = newTextAttr(COLOR_BUILTIN);
        attrCommand = newTextAttr(COLOR_COMMAND);
        attrKeyword = newTextAttr(COLOR_KEYWORD);
        attrReserved = newTextAttr(COLOR_RESERVED);
        attrError = newTextAttr(COLOR_ERROR);
        attrString = newTextAttr(COLOR_STRING);
        attrHighlight = newTextHighlight(COLOR_HIGHLIGHT_FG, COLOR_HIGHLIGHT_BG);
        attrBlink = newTextHighlight(COLOR_BLINK_FG, COLOR_BLINK_BG);
    }


    public static boolean format(CodePane codePane, int nlLoc)
    {
        JTextPane pane = codePane.pane;
        doc = codePane.doc;
        JViewport v = codePane.getViewport();
        indentBefore = 0;
        indentAfter = 0;
        indentPos = nlLoc; // position of interest for indentation info
        //System.out.println("format begin");
        Point point = v.getViewPosition();
        start = pane.viewToModel(point);
        //System.out.print("text loc for origin: "); 
        //System.out.println(start);
        Dimension extent = v.getExtentSize();
        point.translate(extent.width, extent.height);
        end = pane.viewToModel(point);
        //System.out.print("text loc for bottom right: "); 
        //System.out.println(end);
        boolean b;
        int docLength = doc.getLength();
        String docText;
        try {
           docText = doc.getText(0, docLength);
        } catch(Exception e ) { System.out.println(e); 
                                return false; }
        if (codePane.isSal && (docLength == 0 || docText.charAt(0) != '(')) {
            b = formatVisibleSal(codePane, docText, docLength);
        } else {
            b = formatVisible(codePane, docText, docLength);
        }
        // System.out.println("format end: isSal " + codePane.isSal + 
        //                   " indentBefore " + indentBefore + 
        //                   "  After " + indentAfter);
        return b;
    }


    public static void setColor(int pos, int len, 
                                SimpleAttributeSet color)
    {
        // if color region overlaps visible region, update doc with color
        if (!(end < pos || pos + len < start)) {
            doc.setCharacterAttributes(pos, len, color, true);
        }
    }
    
    static final int INDENT = 2; // how much to indent
    // these are codes for different bracket types:
    static final int ONESHOT = 0;
    static final int IF_THEN = 1;
    static final int COMMAND = 2;
    static final int BRACE = 3;
    static final int BRACKET = 4;
    static final int PAREN = 5;
    static final int IF = 6;
    static final int BEGIN = 7;
    static final int TOP = 8; // imaginary initial/final brackets used
        // to detect and mark any unclosed real brackets like begin, (, etc.
    // debugging support (I guess I should have used enumerated type):
    static String[] stateName = { "ONESHOT", "IF_THEN", "COMMAND",
            "BRACE", "BRACKET", "PAREN", "IF", "BEGIN", "TOP" };

    
    // find the column of pos in text by searching back to newline
    public static int columnFor(String text, int pos) {
        int col = 0;
        while (pos > 0 && text.charAt(pos - 1) != '\n') {
            col++;
            pos--;
        }
        return col;
    }
    
    // placed here so it is accessible by inner classes -- not ideal
    static int lineIndent;

    public static boolean formatVisibleSal(CodePane pane, 
            final String docText, int docLength) {
    // start and end give the visible range over which we actually 
    // change the document.
    //
    // Parse the whole file (well, almost):
    // Use a stack for matching tokens and to compute indent:
    // The stack contains the indentation to restore to when
    // the matching close token is found. Also, if the close
    // token does not match the top of the stack, there is some
    // balance problem -- use precedence to decide what to match to, e.g.
    // and END will skip an unclosed open-paren to match a BEGIN because
    // BEGIN/END has higher precedence.
    // 
    // This idea breaks down when there are local syntax errors, e.g.
    // if you type a "todo" list at the top of a file, and the text
    // is (intentionally) full of SAL syntax errors (because it's not
    // supposed to be SAL), then far down in the file, you may find
    // indentation breaks down completely. To work around this problem
    // the syntax coloring is RESET whenever "define" appears
    //
    // In SAL, IF-THEN-ELSE does not have close tokens, so indentation
    // is very tricky. IF is actually closed by THEN, but THEN must
    // be followed by a statement. We record this by pushing IF_THEN,
    // meaning that there's an IF-THEN construct holding a place for
    // a possible ELSE. Then we push a COMMAND,
    // a placeholder for the next statement. After any statement word,
    // we pop COMMAND. If we find an ELSE, we pop the matching IF-THEN.
    // If we get a statement when we're not expecting COMMAND, we pop
    // all the IF-THEN's because the statement is going to stop any
    // ELSE from matching and therefore we have to pop the indentation
    // level back before any pending IF-THENs.
    // 
    // Note that indentBefore and indentAfter and indentPos are used
    // so that indentation can be computed and inserted from external
    // editing functions. These basically serve as probes into the
    // parsing algorithm.
    //
    // Coloring:
    //    as for LISP:
    //        known functions are in blue
    //        strings are in brown
    //        comments are in green
    //    as in Common Music SAL:
    //        reserved words are in purple
    //        keyword parameters are in pink
    //        commands are in dark blue
    //
        boolean allMatched = true;
        boolean afterNewline = true; // set to true after each newline, and
            // false after first token after each newline; used for indentation
        lineIndent = 0; // will hold the correct indentation for the
            // current line when a newline is reached.
            // This is in contrast to the ParseStack.indent number, which is
            // the "nominal" indentation that will be adjusted if the first
            // token on the line closes an open bracket (e.g. begin or "(")
            // or if the first token is not an expected command word
        class ParseState {
            public int indent; // the indentation level
            public int indent2; // indent level if not a command
            public int elseIndent; // used only for IF and IF_THEN to
                // remember indentation for a possible ELSE clause
            public int pos;    // location of the token
            public int len;    // length of the token
            public int state; // the saved state
            public ParseState(int i, int i2, int p, int l, int s) {
                indent = i; indent2 = i2; pos = p; len = l; state = s;
                elseIndent = 0;
            }
        };
        
        class ParseStack {
            public Stack<ParseState> stack;
            public int indent;
            public int indent2; // the indentation if a command is continued
            public ParseStack() { 
                stack = new Stack<ParseState>(); 
                stack.push(new ParseState(0, 0, 0, 0, TOP));
                indent = 0; 
                indent2 = 0;
            }
            
            // push an indentation level on the stack, the keyword responsible
            // starts at pos and had length len. The new indentation level will
            // be at the column of pos + offset
            public void push(int offset, int offset2, int pos, int len, int state) {
                offset += columnFor(docText, pos) - indent;
                pushOffset(offset, offset2, pos, len, state);
            }
            
            // pushOffset is like push except the offset is relative to the
            // current indentation, not the position of the keyword (useful
            // for "define function foo()" where function causes the indent
            // (because you can have "define var" that does not indent), but
            // you want to indent relative to define rather than function.
            public void pushOffset(int offset, int offset2, int pos, int len, 
                                   int state) {
                stack.push(new ParseState(indent, indent2, pos, len, state));
                indent += offset;
                indent2 = indent + offset2;
                if (TCDEBUG) { System.out.println("pushOffset " + offset + 
                                                  " indent " + indent); }
            }
            
            // pop stack back to a matching token: bracket tokens have
            // priorities, e.g. begin is higher priority than "(", so
            // and "end" will jump over an unmatched "(" to find a "begin"
            // This is done to obtain more reasonable warnings to user,
            // otherwise, for example, "begin ( end" would all be red
            // instead of just showing the "(" in red. If anything is 
            // unmatched, return false so caller knows there is an error
            // somewhere.
            boolean pop(int state) {
                boolean matched = true;
                // System.out.println("pop " + stack);
                while (!stack.empty() && stack.peek().state <= state) {
                    ParseState ps = stack.pop();
                    indent = ps.indent;
                    indent2 = ps.indent2;
                    // System.out.println("pop indent " + indent + " indent2 " + indent2);
                    if (ps.state == state) return matched;
                    if (ps.state != IF_THEN) { // ignore these                        ;
                        matched = false;
                        setColor(ps.pos, ps.len, attrError);
                    }
                }
                if (TCDEBUG) System.out.println("pop return false at end");
                return false;
            }
            
            // after a command like define, the indentation level
            // is incremented, but the level should be returned as soon as
            // a command word is encountered
            public void popCommandAndIfThen() {
                // System.out.println("popCommand");
                if (stack.peek().state == COMMAND) {
                    pop(COMMAND);
                    if (TCDEBUG) {
                        System.out.println(" --> poppedCommand"); print(); }
                } else while (stack.peek().state == IF_THEN) {
                    pop(IF_THEN);
                    lineIndent = indent2 = indent;
                    if (TCDEBUG) {
                        System.out.println(" --> popped IF_THEN"); print(); }
                }
            }
            
            // add COMMAND to stack after an IF_THEN in response to "else"
            // this is tricky because the column inherits from the IF_THEN,
            // not the current indent value, and indent gets elseIndent
            public int ifElseCommand(int pos, int len) {
                ParseState ps = stack.peek();
                if (ps.state == IF_THEN) { // good, "else" follows if...then
                    // rather than popping IF_THEN and pushing COMMAND, just
                    // change ps:
                    indent = indent2 = ps.elseIndent + INDENT;
                    ps.pos = pos; 
                    ps.len = len;
                    ps.state = COMMAND;
                    return ps.elseIndent;
                }
                return 0;
            }
                

            public void startCommand() {
                // remove COMMAND if present and retain indentation
                // otherwise pop out of IF_THEN's
                if (stack.peek().state == COMMAND) {
                    ParseState ps = stack.pop();
                    indent2 = indent = ps.indent;
                    // back up indentation to prepare for an ELSE:
                    ps = stack.peek();
                    if (ps.state == IF_THEN) {
                        indent2 = indent = ps.elseIndent;
                    }
                    //lineIndent = indent;
                    //indent = indent2 = ps.indent;
                } else {
                    popCommandAndIfThen();
                }
            }
            
            public void convertCommand(int pos, int len, int state) {
                // remove COMMAND if present and replace with state
                if (stack.peek().state == COMMAND) {
                    ParseState ps = stack.peek();
                    ps.pos = pos;
                    ps.len = len;
                    ps.state = state;
                } else {
                    popCommandAndIfThen();
                    pushOffset(0, len + 1, pos, len, state);
                }
            }
            
            public void finishCommand() {
                // remove COMMAND if present
                if (stack.peek().state == COMMAND) {
                    pop(COMMAND);
                } else {
                    popCommandAndIfThen();
                }
            }
            
            public boolean convertIf() {
                if (stack.peek().state == IF) {
                    ParseState ps = stack.peek();
                    ps.state = IF_THEN;
                    return true;
                } else {
                    return false;
                }
            }
            
            public void print() {
                ParseState state = stack.peek();
                System.out.println("State: indent " + state.indent +
                        " indent2 " + state.indent2 + 
                        " pos " + state.pos +
                        " len " + state.len +
                        " state " + stateName[state.state] + 
                        " (cur) indent " + indent + 
                        " (cur) indent2 " + indent +
                        " (cur) lineIndent " + lineIndent);
                for (int i = 0; i < stack.size(); i++) {
                    System.out.print(stateName[stack.get(i).state] + " " + 
                                     stack.get(i).indent + " ");
                }
                System.out.println();
            }
            
            //public boolean expectingStmt() {
            //    int s = stack.peek().state;
            //    return (s == BEGIN || s == TOP);
           // }
        };
                
        //System.out.println("formatVisibleSal");
        try {
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
            boolean barSymbol = false;
            int barSymbolStart = 0;
            boolean space = false;
            int spaceStart = 0;
            int pos;
            ParseStack stack = new ParseStack();
            
            // originally, this code accumulated words in buf, but this
            // caused many string allocations and ran very slowly, so now
            // we just mark where the word begins and when the word ends
            // we extract a string from docText
            
            
            // process document text while you have not reached the end
            // of the visible area and while there are unmatched parens
            // (i.e. the parenStack isn't empty, but stop if you reach
            // the end of the whole file.
            // 
            // System.out.println("begin parse loop, docLength " + docLength);
            for (pos = 0; 
                 pos < docLength && (pos < end || !stack.stack.empty());
                 pos++) {
                char c = docText.charAt(pos);
                // System.out.print("CHAR |" + c + "| ");
                if (comment) {
                    if (c == '\n') {
                        if (pos >= start) {
                            setColor(commentStart, pos - commentStart, 
                                         attrComment);
                        }
                        comment = false;
                    } // note that escape chars are ignored in comments
                } else if (string) {
                    if (c == '"') {
                        if (pos >= start)
                            setColor(stringStart, pos - stringStart + 1, 
                                         attrString);
                        string = false;
                    } else if (c == '\\') { // skip the quoted char
                        pos++;
                    }
                } else if (word) {
                    // see if the word ends because of a character or end of text
                    boolean terminated = (c == ' ' || c == '(' || c == ')' || 
                                          c == '\n' || c == '"' || c == '{' ||
                                          c == '}' || c == '[' || c == ']');
                    if (terminated || pos >= docLength - 1) {
                        word = false;
                        int stop = pos;
                        if (!terminated) stop++;
                        String buf = docText.substring(wordStart, stop);
                        if (TCDEBUG) 
                            System.out.println("formatVisibleSal: " + buf);
                        buf = buf.toLowerCase(); // for simplified lookup
                        SimpleAttributeSet color;
                        // WordList.printList(buf); // to get words list.
                        if (SalWordList.isReservedWord(buf)) {
                            color = attrReserved;
                            // note that "then" is not included here because
                            // "then" in a "for" clause does not need to be
                            // followed by a COMMAND
                            if (buf.equals("function") || buf.equals("finally")) {
                                if (afterNewline) {
                                    lineIndent = stack.indent;
                                }
                                stack.startCommand();
                                stack.pushOffset(INDENT, 0, wordStart, 
                                                 buf.length(), COMMAND);
                            } else if (buf.equals("else")) {
                                int i = stack.ifElseCommand(wordStart, 4);
                                if (afterNewline) {
                                    lineIndent = i;
                                }
                                if (TCDEBUG) {
                                    System.out.println("ifElseCommand"); 
                                    stack.print(); }
                            } else if (buf.equals("when") || 
                                       buf.equals("unless")) {
                                stack.push(INDENT, buf.length() + 1 - INDENT, 
                                           wordStart, buf.length(), COMMAND);
                            } else if (buf.equals("variable")) {
                                stack.startCommand();
                                stack.indent2 = columnFor(docText, wordStart) + 
                                                buf.length() + 1;
                            } else if (buf.equals("with") ||
                                       buf.equals("while") ||
                                       buf.equals("until")) {
                                stack.startCommand();
                                stack.push(0, buf.length() + 1,
                                           wordStart, buf.length(), COMMAND);
//                                stack.indent2 = columnFor(docText, wordStart) +
//                                                buf.length() + 1;
                            } else if (buf.equals("then")) {
                                if (TCDEBUG) { 
                                    System.out.println("at then"); 
                                    stack.print(); }
                                // if there's an if on the stack, pop it
                                if (stack.convertIf()) {
                                    if (afterNewline) {
                                        lineIndent = stack.indent;
                                    }
                                    stack.pushOffset(INDENT, INDENT, wordStart, 
                                                     4, COMMAND);
                                } else {
                                    stack.indent2 = 
                                            columnFor(docText, wordStart) +
                                            buf.length() + 1;
                                }
                                if (TCDEBUG) {
                                    System.out.println("after then"); 
                                    stack.print(); }
                            } else if (buf.equals("for")) {
                                stack.popCommandAndIfThen();
                                stack.indent2 = stack.indent + INDENT;
                            }
                            afterNewline = false;
                        } else if (SalWordList.isCommandWord(buf)) {
                            stack.indent2 = stack.indent;
                            color = attrCommand;
                            if (afterNewline) {
                                lineIndent = stack.indent;
                            }
                            if (buf.equals("define")) {
                                // reset syntax coloring
                                stack.pop(TOP); // color unmatched stuff
                                stack = new ParseStack(); // reset stack
                            } else if (buf.equals("begin") || buf.equals("loop")) {
                                stack.push(INDENT, INDENT, wordStart, 5, BEGIN);
                            } else if (buf.equals("end")) {
                                if (!stack.pop(BEGIN)) {
                                    allMatched = false;
                                    setColor(wordStart, 3, attrError);
                                }
                                if (TCDEBUG) {
                                    System.out.println("end token after stack.pop"); 
                                    stack.print(); }
                                // if this is the first word on the line, then
                                // match indentation to the matching BEGIN
                                if (afterNewline) {
                                    lineIndent = stack.indent;
                                }
                                stack.finishCommand();
                                if (TCDEBUG) { 
                                    System.out.println("end end"); 
                                    stack.print(); }
                            } else if (buf.equals("if")) {
                                if (TCDEBUG) { 
                                    System.out.println("at if"); 
                                    stack.print(); }
                                // int i = stack.indent;
                                stack.convertCommand(wordStart, 2, IF);
                                stack.stack.peek().elseIndent = stack.indent;
                                // stack.indent = stack.indent2 = i;
                                // stack.push(0, 0, wordStart, 2, IF);
                                if (TCDEBUG) { 
                                    System.out.println("after if"); 
                                    stack.print(); }
                            } else if (buf.equals("print")) {
                                stack.startCommand();
                                stack.indent2 = columnFor(docText, wordStart) +
                                                buf.length() + 1;
                            } else {
                                stack.startCommand();
                                stack.indent2 = stack.indent + INDENT + INDENT;
                            }
                            afterNewline = false;
                        } else if (WordList.isKeyword(buf)) {
                            if (afterNewline) {
                                lineIndent = stack.indent2;
                                afterNewline = false;
                            }
                            color = attrBuiltin;
                        } else if (buf.charAt(buf.length() - 1) == ':') {
                            if (afterNewline) {
                                lineIndent = stack.indent2;
                                afterNewline = false;
                            }
                            color = attrKeyword;
                        } else {
                            if (afterNewline) {// && stack.expectingStmt()) {
                                lineIndent = stack.indent2;
                                if (TCDEBUG) {
                                    System.out.println("buf " + buf + 
                                        " afterNewline " + afterNewline +
                                        " lineIndent becomes " + lineIndent); }
                                afterNewline = false;
                            }
                            color = attrNormal;
                        }
                        // System.out.println(color);
                        if (color != attrNormal) {
                            setColor(normalStart, 
                                     wordStart - normalStart, attrNormal);
                            setColor(wordStart, pos - wordStart + 1, 
                                     color);
                            normal = true;
                            normalStart = pos + 1;
                        } else {
                            // continue normal (black) color
                        }
                    } else if (c == '\\') {
                        pos++; // skip the quoted char
                    }
                    if (terminated) {
                        pos = pos - 1; // reexamine character at pos
                        continue; // back to for loop, skip indentation tests
                    }
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
                        setColor(normalStart, pos - normalStart, attrNormal);
                    }
                    normal = false;
                } else if (c == '"') { // String start
                    if (afterNewline) {
                        lineIndent = stack.indent2;
                        afterNewline = false;
                    }
                    string = true;
                    stringStart = pos;
                    space = false;
                    if (pos >= start && normal) {
                        setColor(normalStart, pos - normalStart, attrNormal);
                    }
                    normal = false;
                } else if (c == '(' || c == ')' || c == '{' || c == '}' ||
                           c == '[' || c == ']') { 
                    // Paren/Brace start and end
                    if ((c == '(' || c == '{' || c == '[') && afterNewline) {
                        lineIndent = stack.indent2;
                        afterNewline = false;
                    }
                    space = false;
                    if (pos >= start && normal) {
                        setColor(normalStart, pos - normalStart, attrNormal);
                    }
                    normal = false;
                    if (pos >= start)
                        setColor(pos, 1, attrNormal);
                    if (c == '(' ) stack.push(1, 0, pos, 1, PAREN);
                    else if (c == '{') stack.push(1, 0, pos, 1, BRACE);
                    else if (c == '[') stack.push(1, 0, pos, 1, BRACKET);
                    else { // must be ')' or '}' or ']'
                        if (afterNewline) {
                            lineIndent = stack.indent - 1;
                        }
                        if (stack.pop(c == ')' ? PAREN : 
                            (c == ']' ? BRACKET : BRACE))) {
                            // System.out.println("after ), ], or }: indent " + stack.indent);   
                        } else {
                            allMatched = false;
                            setColor(pos, 1, attrError);
                        }
                    }
                    //System.out.println("highlight/blink " + c + " pos " + pos +
                    //        " highlightLoc " + pane.highlightLoc + 
                    //        " blinkLoc " + pane.blinkLoc);
                    // there could be a race condition here, but failure
                    // is not critical -- this just blinks the closing paren
                    if (pane != null && pane.blinkLoc == pos && pane.blinkOn) {
                        setColor(pos, 1, attrBlink);
                    } else if (pane != null && pane.highlightLoc == pos) {
                        setColor(pos, 1, attrHighlight);
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
                if (c == '\n') { // possibly store indentation info
                    //System.out.println(pos + " indentPos " + indentPos + 
                    //                   " indent " + stack.indent);
                    if (pos == indentPos) {
                        indentBefore = lineIndent;
                        indentAfter = stack.indent;
                    }
                    lineIndent = stack.indent;
                    
                    afterNewline = true;
                    if (TCDEBUG) {
                        System.out.println("at newline"); stack.print(); }
                }
                /*
                System.out.print("At end of loop, pos ");
                System.out.println(pos);
                */
            } // END FOR LOOP
            //System.out.println("end parse loop");
            if (normal) {
                setColor(normalStart, pos - normalStart + 1, attrNormal);
            } else if (comment) {
                setColor(commentStart, pos - commentStart + 1, attrComment);
            } else if (string) {
                setColor(stringStart, pos - stringStart + 1, attrString);
            }
            // set error color to any unmatched tokens on stack:
            if (!stack.pop(TOP)) allMatched = false;
        } catch(Exception e ) { 
            System.out.println(e); 
            e.printStackTrace();
        }
        //System.out.println("salFormatVisible returns " + allMatched);
        return allMatched;
    }
    
    
    public static boolean formatVisible(CodePane pane, 
                                        String docText, int docLength)
    // start and end give the visible range over which we actually 
    // change the document.
    {
        boolean evenParens = false;

        try {
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
            Stack<Integer> parenStack = new Stack<Integer>();
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
                char c = docText.charAt(pos);
                if (comment) {
                    if (c == '\n') {
                        if (pos >= start) {
                            setColor(commentStart, pos - commentStart, 
                                         attrComment);
                        }
                        comment = false;
                    } // note that escape chars are ignored in comments
                } else if (string) {
                    if (c == '"') {
                        if (pos >= start)
                            setColor(stringStart, pos - stringStart + 1, 
                                         attrString);
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
                        int stop = pos;
                        if (!terminated) stop++;
                        String buf = docText.substring(wordStart, stop);
                        // WordList.printList(buf); // to get words list.
                        if (WordList.isKeyword(buf)) {
                            setColor(normalStart, wordStart - normalStart,
                                         attrNormal);
                            setColor(wordStart, pos - wordStart + 1, 
                                         attrBuiltin);
                            normal = true;
                            normalStart = pos + 1;
                        } else {
                            // continue normal (black) color
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
                                setColor(normalStart, 
                                         hashCommentStart - normalStart,
                                         attrNormal);
                            }
                            setColor(hashCommentStart, 
                                         pos - hashCommentStart + 1, attrComment);
                        }
                        normal = false;
                        hashComment = false;
                    } else if (c == '\\') pos++; // skip the quoted char
                } else if (barSymbol) {
                    if (c == '|') {
                        if (pos >= start) {
                            if (normal) {
                                setColor(normalStart, wordStart - normalStart,
                                             attrNormal);
                            }
                            normal = false;
                            setColor(wordStart, pos - wordStart, attrNormal);
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
                        setColor(normalStart, pos - normalStart, attrNormal);
                    }
                    normal = false;
                } else if (c == '"') { // String start
                    string = true;
                    stringStart = pos;
                    space = false;
                    if (pos >= start && normal) {
                        setColor(normalStart, pos - normalStart, attrNormal);
                    }
                    normal = false;
                } else if (c == '(' || c == ')') { // Paren start and end
                    space = false;
                    if (pos >= start && normal) {
                        setColor(normalStart, pos - normalStart, attrNormal);
                    }
                    normal = false;
                    if (pos >= start)
                        setColor(pos, 1, attrNormal);
                    if (c == '(' ) {
                        parenStack.push(new Integer(pos));
                    } else {
                        if (parenStack.empty()) {
                            if (pos >= start)
                                setColor(pos, 1, attrError);
                        } else {
                            // System.out.println("pop stack");
                            parenStack.pop();
                        }
                    }
                    //System.out.println("highlight/blink " + c + " pos " + pos +
                    //        " highlightLoc " + pane.highlightLoc + 
                    //        " blinkLoc " + pane.blinkLoc);
                    // there could be a race condition here, but failure
                    // is not critical -- this just blinks the closing paren
                    if (pane != null && pane.blinkLoc == pos && pane.blinkOn) {
                        setColor(pos, 1, attrBlink);
                    } else if (pane != null && pane.highlightLoc == pos) {
                        setColor(pos, 1, attrHighlight);
                    }
                } else if (c == '#') {
                    space = false;
                    if (pos >= start && normal) {
                        setColor(normalStart, pos - normalStart, attrNormal);
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
                setColor(normalStart, pos - normalStart + 1, attrNormal);
            } else if (comment) {
                setColor(commentStart, pos - commentStart + 1, attrComment);
            } else if (hashComment) {
                setColor(hashCommentStart, pos - hashCommentStart + 1,
                             attrComment);
            } else if (string) {
                setColor(stringStart, pos - stringStart + 1, attrString);
            }

            evenParens = parenStack.empty();
            while( !parenStack.empty()) {
                int parenPos = ((Integer)parenStack.pop()).intValue();
                if (pos >= start)
                    setColor(parenPos, 1, attrError);
            }
        }
        catch(Exception e ) { System.out.println(e); }

        return evenParens;
    }
}
