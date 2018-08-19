package jnyqide;


import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;


import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;

public class WordList {
    //public static Trie wordsTrie  = new Trie();
    public static JTextArea textArea;
    public static JTextPane pane;
    public static int startPos;
    public static int endPos;
    public static Map<String, String> URLLinks = new HashMap<String, String>();
    public static ArrayList<String> words = new ArrayList<String> ();
    
    public static String getlink(String word) {
        // strip off syntax help from word, e.g. (osc-tri hz) -> osc
        // System.out.println("getlink " + word);
        if (word.charAt(0) == '(') {
            int i = word.indexOf(" ");
            if (i <= 0) i = word.length() - 1; // remove ")"
            word = word.substring(1, i);
        } else /* maybe SAL mode */ {
            int i = word.indexOf("(");
            if (i > 0) word = word.substring(0, i);
        }
        String s = URLLinks.get(word);
        // System.out.println("getlink(" + word + ")->" + s);
        if (s == null) return "home.html";
        return s;
    }
    
    public static void init(JTextArea a, String extDir)
            throws FileNotFoundException {
        textArea = a;
        String nw = MainFrame.currentDir + "NyquistWords.txt";
        BufferedReader inf;
        try {
            inf = new BufferedReader(new FileReader(nw));
            System.out.println("\n\n**********Opened " + nw +
                               "*********\n\n");
            processWordFile(inf, false);
        } catch (IOException e) {
            String nw2 = MainFrame.currentDir + "jnyqide/NyquistWords.txt";
            System.out.println("could not find " + nw + ", trying " + nw2);
            inf = new BufferedReader(new FileReader(nw2));
        }

        // now look for extensions
        String[] directories = ExtensionManager.getExtensionDirs(extDir);
        for (String dir : directories) {
            processExtension(extDir + dir);
        }   
    }

    
    public static void processExtension(String dir) {
        String nw3 = dir + File.separator + "nyquistwords.txt";
        BufferedReader inf;
        try {
            inf = new BufferedReader(new FileReader(nw3));
            processWordFile(inf, true);
        } catch (IOException e) {
            return; // must not have a nyquistwords.txt file, it's ok
        }
    }


    public static void processWordFile(BufferedReader inf, boolean ext)
            throws IOException {
        String word, link;
        while ((word = inf.readLine()) != null) {
            System.out.println("processWordFile: word is |" + word + "|");
            if (word.length() == 0) {
                System.out.println("In processWordFile, encountered blank " +
                                   "word. Maybe an extra newline? Skipping " +
                                   "the rest of the file.");
                break;
            }
            word = word.trim();

            // only add word if it is not already there
            boolean found = false;
            for (String candidate : words) {
                if (word.equals(candidate)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                words.add(word);
            }

            link = inf.readLine();
            if ((link == null) || (link.equals(""))) link = "home.html";

            // mark links to extension documentation with "@" prefix
            if (ext) {
                link = "@" + link;
            }

            int i = word.indexOf(" ");
            if (i >= 0) word = word.substring(0, i);
            i = word.indexOf(")");
            if (i >= 0) word = word.substring(0, i);
            URLLinks.put(word, link);
        }
        inf.close();
    }


    public static String removeChar(String word, char c) {
        int loc = word.indexOf(c);
        while (loc >= 0) {
            word = word.substring(0, loc) + word.substring(loc + 1);
            loc = word.indexOf(c);
        }
        return word;
    }

    public static void appendSyntaxTip(StringBuffer s, String word,
                                       boolean sal) {
        // System.out.println("appendSyntaxTip word = |" + word + "|");
        if (word.charAt(word.length() - 1) == ')') { // indicates a function
            if (sal) {
                // make it prefix with commas
                int loc = word.indexOf(' '); // end of function name
                int loc2 = loc + 1; // trim the space
                if (loc < 0) {
                    loc = word.indexOf(')');
                    loc2 = loc; // don't trim the ')'
                }
                if (loc < 0) {
                    System.out.println("appendSyntaxTip internal error: |" + 
                                       word + "|");
                    return;
                }
                // now loc is the character after the function name and
                // loc2 is the character to place after the open paren
                word = word.substring(0, loc) + "(" + word.substring(loc2);
                // insert commas after tokens starting after open paren
                loc = loc + 1;
                while (loc < word.length()) {
                    // advance to token
                    while (loc < word.length() && 
                           (word.charAt(loc) == ' ' ||
                            word.charAt(loc) == '[' || 
                            word.charAt(loc) == ']')) loc++;
                    // retain starting location for keyword processing
                    loc2 = loc;
                    // advance to end of word
                    while (loc < word.length() && 
                           word.charAt(loc) != ' ' &&
                           word.charAt(loc) != '[' &&
                           word.charAt(loc) != ']') loc++;
                    // convert to keyword or maybe add comma
                    if (loc < word.length()) {
                        if (word.charAt(loc2) == ':') {
                            word = word.substring(0, loc2) + // before keyword
                                   word.substring(loc2 + 1, loc) + // after ':'
                                   ":" + word.substring(loc);
                        // insert comma if this is not the last parameter,
                        // determined by looking ahead for a space
                        } else if (word.indexOf(' ', loc) > 0) {
                            word = word.substring(0, loc) + "," + 
                                   word.substring(loc);
                            loc = loc + 1; // skip the comma
                        }
                    }
                    // System.out.println("word |" + word + "| loc " + loc);
                }
            } else {
                s.append("(");
            }
        }
        // System.out.println("s.append(" + word + ")");
        s.append(word);
        // if this is external, add the package name
        String link = getlink(word);
        if (link.charAt(0) == '@') {
            int loc = link.indexOf('/'); // end of external name
            if (loc >= 0) {
                s.append(" ");
                s.append("ext: ");
                s.append(link.substring(1, loc));
            } else {
                System.out.println("appendSyntaxTip " + link + " loc " + loc);
                System.out.println("WARNING appendSyntaxTip didn't find /");
            }            
        }
        s.append("\n");
    }
    
    
    public static void getAllWordsFor(String str, StringBuffer s, boolean sal) {
        for (int i = 0; i < words.size(); i++) {
            String word = (String) words.get(i);
            int pos = word.indexOf(str);
            if (pos >= 0) {
                int sp = word.indexOf(" ");
                if (sp < 0 || sp > pos) {
                    appendSyntaxTip(s, word, sal);
                }
            }
        }
    }
    
    
    public static boolean isKeyword(String str) {
        if (str.length() > 0 && Character.isLetter(str.charAt(0))) {
            String s = URLLinks.get(str);
            return s != null;
        } else {
            return false;
        }
    }

    
    public static void getWordsFor(String str, StringBuffer s, boolean sal) {
        System.out.println("getWordsFor str = |" + str + "|");
        for (String word : words) {
            //int i = 0; i < words.size(); i++) {
            //String word = (String) words.get(i);
            int pos = word.indexOf(str);
            if (pos == 0) {
                appendSyntaxTip(s, word, sal);
            }
        }
    }
    
    
    public static void printList(String str, JTextPane jTextPane, 
            int start, int end, boolean forceExact, boolean sal) {
        
        //List l;
        //System.out.println("printList: prefFullSearch = " + MainFrame.prefFullSearch);
        //if (MainFrame.prefFullSearch) l = wordsTrie.getAllWordsFor(str.toLowerCase());
        //else l = wordsTrie.getWordsFor(str.toLowerCase(), false);
        //System.out.println(l);
        //StringBuffer s = new StringBuffer();
        //if (l != null) {
        //    Iterator iter = l.iterator();
        //    while (iter.hasNext()) {
        //        s.append(iter.next()).append("\n");   
	//    }
        //}
        StringBuffer s = new StringBuffer();
        str = str.toLowerCase();
        if (MainFrame.prefFullSearch && !forceExact) getAllWordsFor(str, s, sal);
        else getWordsFor(str, s, sal);
        textArea.setText(s.toString());
        pane = jTextPane;
        startPos = start;
        endPos = end; // haven't inserted character yet
    }


    public static void replaceWithTemplate(String template, boolean isSal) {
        // templates were displayed in textArea based on what the user
        // typed. Now, the user has clicked on a template. Replace what
        // the user typed (and parens) with the template string.
        String before;
        System.out.println("replace 1: startPos " + startPos + " endPos " + endPos);
        try {
            if (!isSal) {
                startPos = startPos - 1;
            }
            before = pane.getText(startPos, 1);
            if (before.equals("(")) startPos--;
        } catch (Exception ex) {
        }
        String after;
        try {
            after = pane.getText(endPos, 1);
            while (Character.isWhitespace(after.charAt(0))) {
                endPos++;
                after = pane.getText(endPos, 1);
            }
            if (after.equals(")")) endPos++;
        } catch (Exception ex) {
        }
        pane.select(startPos, endPos);
        // remove those pesky brackets (denoting "optional")
        template = removeChar(removeChar(template, '['), ']');
        // if this is an extension, then there's an added suffix of the form
        //    ext: package-name, which we should remove
        int ext = template.indexOf(" ext: ");
        if (ext > 0) {
            template = template.substring(0, ext);
        }
        System.out.println("replace: startPos " + startPos + " endPos " + 
                 endPos + " isSal " + isSal + " template |" + template + "|");
        pane.replaceSelection(template);
    }
}
