/*
package jnyqide;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


public class Trie {
    Node root;

    /**
     * Creates an WordMode object with the given dictionary.
     * 
     * @param strs
     *            A list of strings containing the words in dictionary file which
     *            contains the list of valid words sorted by frequency
     */
/*    public Trie() 
    {
        root = new Node();
    }
    
    public void addWord(String word)
    {
        char[] letters = word.toCharArray();
        Node n = root;
        for(int i = 0; i < letters.length; i++)
        {            
            if(n.get((int)letters[i]) == null)
            {
                Node x = n.addNode((int)letters[i]);
                x.parent = n;
                n = x;
            }
            else
                n = n.get((int)letters[i]);            
        }
        if(letters[letters.length-1] == ')')
            word = "(" + word;
        n.addWord(word);
        while(n.parent != null)
        {
            n = n.parent;
            n.addWord(word);
        }
    }

    /* Since you are not allowed to use anything from java.util (except for the 
     * Iterator interface and AbstractList), you must create your own implementation
     * of a list.  (You may choose to extend AbstractList if you would like)
     * 
     *  If you would like to return an AbstractList,
     *  **Hint: What are the methods you need to implement in order to extend 
     *          AbstractList?  Also, in order for the AbstractList to function,
     *          what other methods must you override?
     */
    // if full, must match all letters
/*    public List getWordsFor(String s, Boolean full) {
        char[] letters = s.toCharArray();
        Node n = root;
        for(int i = 0; i < letters.length; i++)
        {
            if(n.get((int)letters[i]) == null)
                return (full ? null : n.unmodifiableList);
            else
                n = n.get((int)letters[i]);
        }
        return n.unmodifiableList;
    }
    
    
    public List getAllWordsFor(String s)
    {
        List r = new ArrayList<String>();
        List l = getWordsFor("", false);
        int i = 0;
        while (i < l.size())
        {
            if (l.get(i).toString().contains(s)) {
                r.add(l.get(i).toString());
            }
            i++;
        }
        return r;
    }
    
    
    private class Node {
        Node parent;
        Node nodes [] = new Node [128];
        ArrayList <String> words =  new ArrayList <String> ();
        List unmodifiableList;
        
        public Node()
        {            
            parent = null;
            unmodifiableList = Collections.unmodifiableList(new AbstractList() {
                public int size() {
                    return words.size();
                }

                public String get(int i) {
                    return (String) words.get(i);
                }
            });
        }
        
        public Node get(int index)
        {
            return nodes[index];
        }
        
        public Node addNode(int i)
        {
            if(nodes[i] == null)
                nodes[i] = new Node();
            return nodes[i];
        }
        
        public void addWord(String str)
        {
            words.add(str);
//            System.out.println("( " + str + " ) is added to Trie");
        }
    }
}
*/